use std::convert::TryFrom;

use byteorder::{ReadBytesExt, LE};
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use libc::c_void;
use serde_derive::{Serialize, Deserialize};

use bw_dat::{Game, Unit};

use crate::bw;
use crate::globals::{Globals, SerializableSprite};
use crate::parse::{
    self, Int, Bool, CodePosition, CodePos, Iscript, Place, PlaceId, FlingyVar, BulletVar,
};
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
    fn get_sprite_locals(&self, sprite: *mut bw::Sprite) -> Option<&Vec<SpriteLocal>> {
        self.sprite_locals.get(&SerializableSprite(sprite))
    }

    fn get_sprite_locals_mut(&mut self, sprite: *mut bw::Sprite) -> &mut Vec<SpriteLocal> {
        self.sprite_locals.entry(SerializableSprite(sprite))
            .or_insert_with(|| Vec::new())
    }

    pub fn from_script(iscript: &Iscript) -> IscriptState {
        IscriptState {
            sprite_locals: FxHashMap::default(),
            globals: vec![0; iscript.global_count as usize],
        }
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

struct CustomCtx<'a> {
    state: &'a IscriptState,
    image: *mut bw::Image,
    unit: Option<Unit>,
    bullet: Option<*mut bw::Bullet>,
}

impl<'a> bw_dat::expr::CustomEval for CustomCtx<'a> {
    type State = parse::ExprState;

    fn eval_int(&mut self, val: &Int) -> i32 {
        match val {
            Int::Variable(id) => {
                match id.place() {
                    Place::Global(id) => self.state.globals[id as usize],
                    Place::SpriteLocal(id) => {
                        let value = self.state.get_sprite_locals(unsafe { (*self.image).parent })
                            .and_then(|locals| locals.iter().find(|x| x.id == id));
                        match value {
                            Some(s) => s.value,
                            None => unsafe {
                                error!(
                                    "Error: image {:04x} accessed uninitialized spritelocal",
                                    (*self.image).image_id,
                                );
                                bw_print!(
                                    "Error: image {:04x} accessed uninitialized spritelocal",
                                    (*self.image).image_id,
                                );
                                i32::min_value()
                            }
                        }
                    }
                    Place::Flingy(ty) => unsafe {
                        let flingy = self.unit.map(|x| *x)
                            .or_else(|| self.bullet.map(|x| x as *mut bw::Unit));
                        let flingy = match flingy {
                            Some(s) => s,
                            None => {
                                error!(
                                    "Error: image {:04x} has no flingy",
                                    (*self.image).image_id,
                                );
                                bw_print!(
                                    "Error: image {:04x} has no flingy",
                                    (*self.image).image_id,
                                );
                                return i32::min_value();
                            }
                        };
                        match ty {
                            FlingyVar::MoveTargetX => (*flingy).move_target.x as i32,
                            FlingyVar::MoveTargetY => (*flingy).move_target.y as i32,
                            FlingyVar::FacingDirection => {
                                bw_angle_to_degrees((*flingy).facing_direction) as i32
                            }
                            FlingyVar::MovementDirection => {
                                bw_angle_to_degrees((*flingy).movement_direction) as i32
                            }
                            FlingyVar::TargetDirection => {
                                bw_angle_to_degrees((*flingy).target_direction) as i32
                            }
                            FlingyVar::TurnSpeed => (*flingy).flingy_turn_speed as i32,
                            FlingyVar::Acceleration => (*flingy).acceleration as i32,
                            FlingyVar::TopSpeed => (*flingy).flingy_top_speed as i32,
                            FlingyVar::Speed => (*flingy).current_speed as i32,
                        }
                    },
                    Place::Bullet(ty) => unsafe {
                        let bullet = match self.bullet {
                            Some(s) => s,
                            None => {
                                error!(
                                    "Error: image {:04x} has no bullet",
                                    (*self.image).image_id,
                                );
                                bw_print!(
                                    "Error: image {:04x} has no bullet",
                                    (*self.image).image_id,
                                );
                                return i32::min_value();
                            }
                        };
                        match ty {
                            BulletVar::State => (*bullet).state as i32,
                            BulletVar::WeaponId => (*bullet).weapon_id as i32,
                            BulletVar::BouncesRemaining => (*bullet).bounces_remaining as i32,
                            BulletVar::DeathTimer => (*bullet).death_timer as i32,
                        }
                    },
                }
            }
        }
    }

    fn eval_bool(&mut self, val: &Bool) -> bool {
        match *val {
        }
    }
}

struct IscriptRunner<'a> {
    iscript: &'a Iscript,
    state: &'a mut IscriptState,
    pos: usize,
    bw_script: *mut bw::Iscript,
    image: *mut bw::Image,
    dry_run: bool,
    in_aice_code: bool,
    sprite_owner_map: &'a mut SpriteOwnerMap,
    owner_read: bool,
    game: Game,
    unit: Option<Unit>,
    bullet: Option<*mut bw::Bullet>,
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
            sprite_owner_map,
            game,
            owner_read: false,
            in_aice_code: true,
            unit: None,
            bullet: None,
        }
    }

    fn eval_ctx<'s>(&'s mut self) -> bw_dat::expr::EvalCtx<CustomCtx<'s>> {
        unsafe {
            if !self.owner_read {
                self.owner_read = true;
                self.unit = self.sprite_owner_map.get_unit((*self.image).parent);
                self.bullet = self.sprite_owner_map.get_bullet((*self.image).parent);
            }
        }
        bw_dat::expr::EvalCtx {
            game: Some(self.game),
            unit: self.unit,
            custom: CustomCtx {
                state: self.state,
                image: self.image,
                bullet: self.bullet,
                unit: self.unit,
            },
        }
    }

    /// Return error pos on error
    unsafe fn run_script(&mut self) -> Result<(), u32> {
        use crate::parse::aice_op::*;
        'op_loop: while self.in_aice_code {
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
                    let condition = &self.iscript.conditions[cond as usize];
                    let mut eval_ctx = self.eval_ctx();
                    if eval_ctx.eval_bool(&condition) {
                        self.jump_to(dest);
                    }
                }
                SET => {
                    let place = PlaceId(self.read_u32()?);
                    let expr = self.read_u32()?;
                    if self.dry_run {
                        // Bad? Ideally would have dry-run state
                        // Maybe should stop here?
                        continue;
                    }

                    let expression = &self.iscript.int_expressions[expr as usize];
                    let mut eval_ctx = self.eval_ctx();
                    let value = eval_ctx.eval_int(&expression);
                    match place.place() {
                        Place::Global(id) => self.state.globals[id as usize] = value,
                        Place::SpriteLocal(id) => {
                            let locals = self.state.get_sprite_locals_mut((*self.image).parent);
                            match locals.iter().position(|x| x.id == id) {
                                Some(s) => locals[s].value = value,
                                None => locals.push(SpriteLocal {
                                    value,
                                    id,
                                }),
                            }
                        }
                        Place::Flingy(ty) => {
                            let flingy = self.unit.map(|x| *x)
                                .or_else(|| self.bullet.map(|x| x as *mut bw::Unit));
                            let flingy = match flingy {
                                Some(s) => s,
                                None => {
                                    error!(
                                        "Error: image {:04x} has no flingy",
                                        (*self.image).image_id,
                                    );
                                    bw_print!(
                                        "Error: image {:04x} has no flingy",
                                        (*self.image).image_id,
                                    );
                                    continue 'op_loop;
                                }
                            };
                            set_flingy_var(flingy, ty, value);
                        }
                        Place::Bullet(ty) => {
                            let bullet = match self.bullet {
                                Some(s) => s,
                                None => {
                                    error!(
                                        "Error: image {:04x} has no bullet",
                                        (*self.image).image_id,
                                    );
                                    bw_print!(
                                        "Error: image {:04x} has no bullet",
                                        (*self.image).image_id,
                                    );
                                    continue 'op_loop;
                                }
                            };
                            match ty {
                                BulletVar::State => (*bullet).state = value as u8,
                                BulletVar::WeaponId => (*bullet).weapon_id = value as u8,
                                BulletVar::BouncesRemaining => {
                                    (*bullet).bounces_remaining = value as u8;
                                }
                                BulletVar::DeathTimer => (*bullet).death_timer = value as u8,
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

unsafe fn set_flingy_var(flingy: *mut bw::Unit, ty: FlingyVar, value: i32) {
    match ty {
        FlingyVar::MoveTargetX => {
            (*flingy).move_target.x = value as i16;
            (*flingy).next_move_waypoint.x = value as i16;
            (*flingy).unk_move_waypoint.x = value as i16;
        }
        FlingyVar::MoveTargetY => {
            (*flingy).move_target.y = value as i16;
            (*flingy).next_move_waypoint.y = value as i16;
            (*flingy).unk_move_waypoint.y = value as i16;
        }
        FlingyVar::FacingDirection => {
            (*flingy).facing_direction = degrees_to_bw_angle(value);
        }
        FlingyVar::MovementDirection => {
            (*flingy).movement_direction = degrees_to_bw_angle(value);
        }
        FlingyVar::TargetDirection => {
            (*flingy).target_direction = degrees_to_bw_angle(value);
        }
        FlingyVar::TurnSpeed => (*flingy).flingy_turn_speed = value as u8,
        FlingyVar::Acceleration => (*flingy).acceleration = value as u16,
        FlingyVar::TopSpeed => (*flingy).flingy_top_speed = value.max(0) as u32,
        FlingyVar::Speed => {
            (*flingy).current_speed = value;
            (*flingy).next_speed = value;
        }
    }
}

fn degrees_to_bw_angle(value: i32) -> u8 {
    (((value % 360) * 256 / -360) + 64) as u8
}

fn bw_angle_to_degrees(value: u8) -> i32 {
    ((((value as i32) + 192) * -1) & 0xff) * 360 / 256
}

#[test]
fn test_angle_conversions() {
    assert_eq!(degrees_to_bw_angle(0), 64);
    assert_eq!(degrees_to_bw_angle(90), 0);
    assert_eq!(degrees_to_bw_angle(180), 192);
    assert_eq!(degrees_to_bw_angle(270), 128);
    assert_eq!(bw_angle_to_degrees(0), 90);
    assert_eq!(bw_angle_to_degrees(64), 0);
    assert_eq!(bw_angle_to_degrees(128), 270);
    assert_eq!(bw_angle_to_degrees(192), 180);
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

    pub fn get_bullet(&mut self, sprite: *mut bw::Sprite) -> Option<*mut bw::Bullet> {
        None
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
    /*
    let _ = std::fs::write("iscript.bin", &iscript.bw_data);
    let _ = std::fs::write("aice.bin", &iscript.aice_data);
    for (bw, aice) in &iscript.bw_aice_cmd_offsets {
        debug!("Bw {:x} -> aice {:x}", bw, aice);
    }
    */
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
