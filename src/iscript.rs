use std::convert::TryFrom;

use byteorder::{ReadBytesExt, LE};
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use libc::c_void;
use serde_derive::{Serialize, Deserialize};

use bw_dat::{Game, Unit};

use crate::bw;
use crate::globals::{Globals, SerializableSprite};
use crate::parse::{self, CodePosition, CodePos, Iscript, VariableType, VariableId};
use crate::recurse_checked_mutex::{Mutex};
use crate::unit;
use crate::windows;

lazy_static! {
    static ref ISCRIPT: Mutex<Option<Iscript>> = Mutex::new(None);
    static ref SPRITE_OWNER_MAP: Mutex<SpriteOwnerMap> = Mutex::new(SpriteOwnerMap::new());
}

#[derive(Serialize, Deserialize, Default)]
pub struct IscriptState {
    sprite_locals: FxHashMap<SerializableSprite, Vec<SpriteLocal>>,
    globals: Vec<i32>,
}

#[derive(Serialize, Deserialize)]
struct SpriteLocal {
    id: u32,
    value: i32,
}

impl IscriptState {
    fn get_sprite_locals(&mut self, sprite: *mut bw::Sprite) -> &mut Vec<SpriteLocal> {
        self.sprite_locals.entry(SerializableSprite(sprite))
            .or_insert_with(|| Vec::new())
    }
}

pub unsafe extern fn run_aice_script(
    bw_pos: *mut c_void,
    iscript: *mut c_void,
    image: *mut c_void,
    dry_run: u32,
    speed_out: *mut u32,
) {
    let bw_pos = bw_pos as *const u8;
    let iscript = iscript as *mut bw::Iscript;
    let image = image as *mut bw::Image;
    let this = ISCRIPT.lock("run_aice_script");
    let mut globals = Globals::get("run_aice_script");
    let mut sprite_owner_map = SPRITE_OWNER_MAP.lock("run_aice_script");
    let this = this.as_ref().expect("No iscript");
    let bw_bin = crate::samase::get_iscript_bin();
    let bw_offset = (bw_pos as usize - 1).checked_sub(bw_bin as usize)
        .and_then(|x| u16::try_from(x).ok())
        .expect("Bad iscript.bin ptr");
    let aice_offset = match this.bw_aice_cmd_offsets.get(&bw_offset) {
        Some(&s) => s,
        None => {
            invalid_aice_command(iscript, image, CodePos::Bw(bw_offset));
            return;
        }
    };
    let game = Game::from_ptr(bw::game());
    let mut runner = IscriptRunner::new(
        this,
        &mut globals.iscript_state,
        aice_offset as usize,
        iscript,
        image,
        dry_run != 0,
        game,
        &mut sprite_owner_map,
    );
    let result = runner.run_script();
    if let Err(pos) = result {
        invalid_aice_command(iscript, image, CodePos::Aice(pos));
    }
}

struct IscriptRunner<'a> {
    iscript: &'a Iscript,
    state: &'a mut IscriptState,
    pos: usize,
    bw_script: *mut bw::Iscript,
    image: *mut bw::Image,
    dry_run: bool,
    game: Game,
    in_aice_code: bool,
    sprite_owner_map: &'a mut SpriteOwnerMap,
}

impl<'a> IscriptRunner<'a> {
    fn new(
        iscript: &'a Iscript,
        state: &'a mut IscriptState,
        pos: usize,
        bw_script: *mut bw::Iscript,
        image: *mut bw::Image,
        dry_run: bool,
        game: Game,
        sprite_owner_map: &'a mut SpriteOwnerMap,
    ) -> IscriptRunner<'a> {
        IscriptRunner {
            iscript,
            state,
            pos,
            bw_script,
            image,
            dry_run,
            game,
            sprite_owner_map,
            in_aice_code: true,
        }
    }

    /// Return error pos on error
    unsafe fn run_script(&mut self) -> Result<(), u32> {
        use crate::parse::aice_op::*;
        while self.in_aice_code {
            let opcode_pos = self.pos as u32;
            let opcode = self.read_u8()?;
            match opcode {
                JUMP_TO_BW => {
                    let dest = self.read_u16()?;
                    (*self.bw_script).pos = dest;
                    return Ok(());
                }
                ANIMATION_START => {
                    let header_index = ((*self.bw_script).header - 0x1c) / 8;
                    let animation = (*self.bw_script).animation;
                    let start_pos = self.iscript.headers.get(header_index as usize)
                        .and_then(|header| header.animations.get(animation as usize))
                        .and_then(|&x| x);
                    match start_pos {
                        Some(x) => self.jump_to(x),
                        None => return Err(opcode_pos),
                    }
                }
                IF => {
                    let cond = self.read_u32()?;
                    let dest = CodePosition(self.read_u32()?);
                    let unit = match self.unit() {
                        Some(s) => s,
                        None => {
                            warn!(
                                "No unit for if (Image {:04x}, anim {})",
                                (*self.image).image_id,
                                animation_name((*self.bw_script).animation),
                            );
                            continue;
                        }
                    };
                    if self.iscript.conditions[cond as usize].eval_with_unit(unit, self.game) {
                        self.jump_to(dest);
                    }
                }
                SET => {
                    let place = VariableId(self.read_u32()?);
                    let expr = self.read_u32()?;
                    if self.dry_run {
                        // Bad? Ideally would have dry-run state
                        // Maybe should stop here?
                        continue;
                    }
                    let unit = match self.unit() {
                        Some(s) => s,
                        None => {
                            warn!(
                                "No unit for if (Image {:04x}, anim {})",
                                (*self.image).image_id,
                                animation_name((*self.bw_script).animation),
                            );
                            continue;
                        }
                    };
                    let value = self.iscript.int_expressions[expr as usize]
                        .eval_with_unit(unit, self.game);
                    let id = place.index();
                    let ty = match place.ty() {
                        Some(s) => s,
                        None => {
                            error!("Invalid set place {:x?}", place);
                            return Err(opcode_pos);
                        }
                    };
                    match ty {
                        VariableType::Global => self.state.globals[id as usize] = value,
                        VariableType::SpriteLocal => {
                            let locals = self.state.get_sprite_locals((*self.image).parent);
                            match locals.iter().position(|x| x.id == id) {
                                Some(s) => locals[s].value = value,
                                None => locals.push(SpriteLocal {
                                    value,
                                    id,
                                }),
                            }
                        }
                    }
                }
                PRE_END => {
                    if self.dry_run {
                        warn!("Dry run with end");
                        (*self.bw_script).wait = 1;
                        break;
                    }
                    let sprite = (*self.image).parent;
                    if (*sprite).first_image == (*sprite).last_image {
                        // This is the last image being deleted, the sprite
                        // will be cleaned up as well
                        self.state.sprite_locals.remove(&SerializableSprite(sprite));
                    }
                    (*self.bw_script).pos = 0x4;
                    self.in_aice_code = false;
                }
                x => {
                    error!("Unknown opcode {:02x}", x);
                    return Err(opcode_pos);
                }
            }
        }
        Ok(())
    }

    fn unit(&mut self) -> Option<Unit> {
        unsafe {
            self.sprite_owner_map.get_unit((*self.image).parent)
        }
    }

    fn jump_to(&mut self, dest: CodePosition) {
        match dest.to_enum() {
            CodePos::Bw(pos) => {
                unsafe {
                    (*self.bw_script).pos = pos;
                }
                self.in_aice_code = false;
            }
            CodePos::Aice(pos) => {
                self.pos = pos as usize;
            }
        }
    }

    fn read_u8(&mut self) -> Result<u8, u32> {
        match self.iscript.aice_data.get(self.pos) {
            Some(&s) => {
                self.pos += 1;
                Ok(s)
            }
            None => Err(self.pos as u32),
        }
    }

    fn read_u16(&mut self) -> Result<u16, u32> {
        match self.iscript.aice_data.get(self.pos..).and_then(|mut x| x.read_u16::<LE>().ok()) {
            Some(s) => {
                self.pos += 2;
                Ok(s)
            }
            None => Err(self.pos as u32),
        }
    }

    fn read_u32(&mut self) -> Result<u32, u32> {
        match self.iscript.aice_data.get(self.pos..).and_then(|mut x| x.read_u32::<LE>().ok()) {
            Some(s) => {
                self.pos += 4;
                Ok(s)
            }
            None => Err(self.pos as u32),
        }
    }
}

struct SpriteOwnerMap {
    mapping: FxHashMap<*mut bw::Sprite, *mut bw::Unit>,
}

impl SpriteOwnerMap {
    pub fn new() -> SpriteOwnerMap {
        SpriteOwnerMap {
            mapping: FxHashMap::with_capacity_and_hasher(1024, Default::default()),
        }
    }

    pub fn get_unit(&mut self, sprite: *mut bw::Sprite) -> Option<Unit> {
        unsafe {
            // TODO bad if used without unit
            if let Some(&unit) = self.mapping.get(&sprite) {
                if (*unit).sprite == sprite {
                    return Unit::from_ptr(unit);
                }
            }
            self.mapping.clear();
            self.mapping.extend(unit::active_units().map(|x| ((**x).sprite, *x)));
            self.mapping.extend(unit::hidden_units().map(|x| ((**x).sprite, *x)));
            debug!("Built mapping to {} units", self.mapping.len());
            self.mapping.get(&sprite).and_then(|&x| Unit::from_ptr(x))
        }
    }
}

unsafe fn invalid_aice_command(iscript: *mut bw::Iscript, image: *mut bw::Image, offset: CodePos) {
    let sprite_id = match (*image).parent.is_null() {
        true => !0,
        false => (*(*image).parent).sprite_id,
    };
    let msg = format!(
        "Invalid aice command at {}, sprite {:04x} image {:04x}, animation {}",
        offset, sprite_id, (*image).image_id, animation_name((*iscript).animation),
    );
    error!("{}", msg);
    bw_print!("{}", msg);
    // TODO replace with recovery
    panic!("{}", msg);
}

fn animation_name(animation: u8) -> String {
    parse::animation_name(animation)
        .map(|x| x.into())
        .unwrap_or_else(|| format!("Unknown animation {:02x}", animation))
}

pub unsafe fn load_iscript(retry_on_error: bool) -> Iscript {
    use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

    loop {
        let iscript_txt = match crate::samase::read_file("scripts\\iscript.txt") {
            Some(s) => s,
            None => {
                windows::message_box("Aice", "Couldn't open scripts\\iscript.txt");
                if !retry_on_error {
                    TerminateProcess(GetCurrentProcess(), 0x4230daef);
                }
                continue;
            }
        };
        let start = std::time::Instant::now();
        match parse::compile_iscript_txt(&iscript_txt) {
            Ok(o) => {
                let time = start.elapsed();
                debug!(
                    "Compiled iscript in {}.{}s, bw bytes {}, aice bytes {}",
                    time.as_secs(), time.subsec_millis(), o.bw_data.len(), o.aice_data.len(),
                );
                return o;
            }
            Err(errors) => {
                use std::fmt::Write;

                error!("Iscript compliation failed");
                for e in &errors {
                    if let Some(line) = e.line {
                        error!("Line {}: {}", line, e.error);
                    } else {
                        error!("{}", e.error);
                    }
                }
                let mut msg = String::new();
                let _ = writeln!(msg, "Iscript compilation failed");
                for e in errors.iter().take(20) {
                    let _ = if let Some(line) = e.line {
                        writeln!(msg, "Line {}: {}", line, e.error)
                    } else {
                        writeln!(msg, "{}", e.error)
                    };
                }
                if errors.len() > 20 {
                    let _ = writeln!(msg, "({} errors follow)", errors.len() - 20);
                }
                windows::message_box("Aice", &msg);
                if !retry_on_error {
                    TerminateProcess(GetCurrentProcess(), 0x4230daef);
                }
            }
        }
    };
}

pub unsafe fn set_as_bw_script(iscript: Iscript) {
    //let _ = std::fs::write("iscript.bin", &iscript.bw_data);
    //let _ = std::fs::write("aice.bin", &iscript.aice_data);
    let bw_bin = crate::samase::get_iscript_bin();
    std::ptr::copy_nonoverlapping(iscript.bw_data.as_ptr(), bw_bin, iscript.bw_data.len());
    *ISCRIPT.lock("set_as_bw_script") = Some(iscript);
}

pub unsafe extern fn iscript_read_hook(_filename: *const u8, out_size: *mut u32) -> *mut u8 {
    use winapi::um::heapapi::{GetProcessHeap, HeapAlloc};
    debug!("Iscript read hook");
    let data = HeapAlloc(GetProcessHeap(), 0, 0x10000) as *mut u8;
    *out_size = 0x10000;
    data
}
