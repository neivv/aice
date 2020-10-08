use std::convert::TryFrom;
use std::ptr::null_mut;
use std::sync::atomic::{AtomicUsize, Ordering};

use byteorder::{ReadBytesExt, LE};
use fxhash::FxHashMap;
use lazy_static::lazy_static;
use libc::c_void;
use serde_derive::{Serialize, Deserialize};

use bw_dat::{Game, Unit, UnitId, Sprite};

use crate::bw;
use crate::globals::{Globals, SerializableSprite};
use crate::parse::{
    self, Int, Bool, CodePosition, CodePos, Iscript, Place, PlaceId, FlingyVar, BulletVar,
    UnitVar, ImageVar,
};
use crate::recurse_checked_mutex::{Mutex};
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

const TEMP_LOCAL_ID: u32 = !0;

/// Unit id of a unit which is being created, !0 for none.
/// Used as a stack as a unit creation iscript can cause another unit to be created.
static CREATING_UNIT: AtomicUsize = AtomicUsize::new(!0);
static CREATING_BULLET: AtomicUsize = AtomicUsize::new(!0);
/// Track corner case of sprite changing during order, and then an animation
/// being played with the new sprite which is now attached to CURRENT_ORDER_UNIT.
static CURRENT_ORDER_UNIT: AtomicUsize = AtomicUsize::new(0);
static CURRENT_ORDER_SPRITE: AtomicUsize = AtomicUsize::new(0);

#[derive(Serialize, Deserialize)]
struct SpriteLocal {
    id: u32,
    value: i32,
}

impl IscriptState {
    fn get_sprite_locals(&self, sprite: *mut bw::Sprite) -> Option<&Vec<SpriteLocal>> {
        self.sprite_locals.get(&SerializableSprite(sprite))
    }

    fn get_sprite_local(&self, image: *mut bw::Image, id: u32) -> Option<i32> {
        self.get_sprite_locals(unsafe { (*image).parent })
            .and_then(|locals| locals.iter().find(|x| x.id == id))
            .map(|x| x.value)
    }

    fn set_sprite_local(&mut self, sprite: *mut bw::Sprite, id: u32, value: i32) {
        let locals = self.get_sprite_locals_mut(sprite);
        match locals.iter().position(|x| x.id == id) {
            Some(s) => locals[s].value = value,
            None => locals.push(SpriteLocal {
                value,
                id,
            }),
        }
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
    (*iscript).wait = 0;
    let image = image as *mut bw::Image;
    let mut this_guard = ISCRIPT.lock("run_aice_script");
    let mut this = this_guard.as_ref().expect("No iscript");
    let bw_bin = crate::samase::get_iscript_bin();
    let bw_offset = (bw_pos as usize - 1).checked_sub(bw_bin as usize)
        .and_then(|x| u16::try_from(x).ok())
        .expect("Bad iscript.bin ptr");
    let mut aice_offset = match this.bw_aice_cmd_offsets.get(&bw_offset) {
        Some(&s) => s as usize,
        None => {
            invalid_aice_command(iscript, image, CodePos::Bw(bw_offset));
            return;
        }
    };
    let game = Game::from_ptr(bw::game());

    let step_order_unit = CURRENT_ORDER_UNIT.load(Ordering::Relaxed);
    if step_order_unit != 0 {
        let unit = Unit::from_ptr(step_order_unit as *mut bw::Unit).unwrap();
        if let Some(sprite) = unit.sprite() {
            let expected_sprite = CURRENT_ORDER_SPRITE.load(Ordering::Relaxed);
            if *sprite != expected_sprite as *mut bw::Sprite {
                let mut sprite_owner_map = SPRITE_OWNER_MAP.lock("run_aice_script");
                sprite_owner_map.add_unit(unit, *sprite);
                CURRENT_ORDER_SPRITE.store(*sprite as usize, Ordering::Relaxed);
            }
        }
    }

    loop {
        let mut globals = Globals::get("run_aice_script");
        let mut sprite_owner_map = SPRITE_OWNER_MAP.lock("run_aice_script");
        let mut runner = IscriptRunner::new(
            this,
            &mut globals.iscript_state,
            aice_offset,
            iscript,
            image,
            dry_run != 0,
            game,
            &mut sprite_owner_map,
        );
        let result = runner.run_script();
        aice_offset = runner.pos;
        match result {
            Ok(ScriptRunResult::Done) => return,
            Ok(ScriptRunResult::CreateUnit(unit_id, pos, player)) => {
                drop(globals);
                drop(sprite_owner_map);
                drop(this_guard);
                if let Some(unit) = bw::create_unit(unit_id, &pos, player) {
                    bw::finish_unit_pre(unit);
                    bw::finish_unit_post(unit);
                    bw::give_ai(unit);
                }
            }
            Err(pos) => {
                invalid_aice_command(iscript, image, CodePos::Aice(pos));
                return;
            }
        }
        this_guard = ISCRIPT.lock("run_aice_script");
        this = this_guard.as_ref().unwrap();
    }
}

struct CustomCtx<'a, 'b> {
    parent: &'a mut IscriptRunner<'b>,
    image: *mut bw::Image,
    unit: Option<Unit>,
    bullet: Option<*mut bw::Bullet>,
    dry_run: bool,
}

impl<'a, 'b> bw_dat::expr::CustomEval for CustomCtx<'a, 'b> {
    type State = parse::ExprState;

    fn eval_int(&mut self, val: &Int) -> i32 {
        match val {
            Int::Variable(id) => {
                match id.place() {
                    Place::Global(id) => self.parent.state.globals[id as usize],
                    Place::SpriteLocal(id) => self.parent.get_sprite_local(self.image, id),
                    Place::Flingy(ty) => unsafe {
                        let flingy = self.unit.map(|x| *x)
                            .or_else(|| self.bullet.map(|x| x as *mut bw::Unit));
                        let flingy = match flingy {
                            Some(s) => s,
                            None => {
                                // Don't error on dry run as bw runs movement
                                // animation for a bit until before the unit
                                // is even fully created.
                                // Alternatively could get unit ptr from first_free_unit.
                                if !self.dry_run {
                                    self.parent.report_missing_parent("flingy");
                                    show_unit_frame0_help();
                                    show_bullet_frame0_help();
                                }
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
                            FlingyVar::PositionX => (*flingy).position.x as i32,
                            FlingyVar::PositionY => (*flingy).position.y as i32,
                            FlingyVar::Player => (*flingy).player as i32,
                        }
                    },
                    Place::Bullet(ty) => unsafe {
                        let bullet = match self.bullet {
                            Some(s) => s,
                            None => {
                                self.parent.report_missing_parent("bullet");
                                return i32::min_value();
                            }
                        };
                        match ty {
                            BulletVar::State => (*bullet).state as i32,
                            BulletVar::WeaponId => (*bullet).weapon_id as i32,
                            BulletVar::BouncesRemaining => (*bullet).bounces_remaining as i32,
                            BulletVar::DeathTimer => (*bullet).death_timer as i32,
                            BulletVar::OrderTargetX => (*bullet).order_target_pos.x as i32,
                            BulletVar::OrderTargetY => (*bullet).order_target_pos.y as i32,
                        }
                    },
                    Place::Unit(ty) => unsafe {
                        let unit = match self.unit {
                            Some(s) => s,
                            None => {
                                self.parent.report_missing_parent("unit");
                                return i32::min_value();
                            }
                        };
                        match ty {
                            UnitVar::DeathTimer => (**unit).death_timer as i32,
                            UnitVar::MatrixTimer => (**unit).matrix_timer as i32,
                            UnitVar::MatrixHp => (**unit).defensive_matrix_dmg as i32,
                            UnitVar::StimTimer => (**unit).stim_timer as i32,
                            UnitVar::EnsnareTimer => (**unit).ensnare_timer as i32,
                            UnitVar::LockdownTimer => (**unit).lockdown_timer as i32,
                            UnitVar::IrradiateTimer => (**unit).irradiate_timer as i32,
                            UnitVar::StasisTimer => (**unit).stasis_timer as i32,
                            UnitVar::PlagueTimer => (**unit).plague_timer as i32,
                            UnitVar::MaelstormTimer => (**unit).maelstrom_timer as i32,
                            UnitVar::IsBlind => (**unit).is_blind as i32,
                        }
                    },
                    Place::Image(ty) => unsafe {
                        let image = self.image;
                        match ty {
                            ImageVar::Drawfunc => (*image).drawfunc as i32,
                            ImageVar::DrawfuncParam => (*image).drawfunc_param as i32,
                        }
                    }
                }
            }
        }
    }

    fn eval_bool(&mut self, val: &Bool) -> bool {
        match *val {
            Bool::HasFlingy => self.bullet.is_some() || self.unit.is_some(),
            Bool::HasBullet => self.bullet.is_some(),
            Bool::HasUnit => self.unit.is_some(),
        }
    }
}

struct IscriptRunner<'a> {
    iscript: &'a Iscript,
    state: &'a mut IscriptState,
    pos: usize,
    /// `pos` gets incremented by each read_u32 etc,
    /// This one points to start of current opcode.
    opcode_pos: u32,
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

enum ScriptRunResult {
    Done,
    CreateUnit(UnitId, bw::Point, u8),
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
            opcode_pos: pos as u32,
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

    fn init_sprite_owner(&mut self) {
        unsafe {
            if !self.owner_read {
                self.owner_read = true;
                self.unit = self.sprite_owner_map.get_unit((*self.image).parent);
                self.bullet = self.sprite_owner_map.get_bullet((*self.image).parent);
            }
        }
    }

    fn eval_ctx<'s>(&'s mut self) -> bw_dat::expr::EvalCtx<CustomCtx<'s, 'a>> {
        self.init_sprite_owner();
        bw_dat::expr::EvalCtx {
            game: Some(self.game),
            unit: self.unit,
            custom: CustomCtx {
                image: self.image,
                bullet: self.bullet,
                unit: self.unit,
                dry_run: self.dry_run,
                parent: self,
            },
        }
    }

    #[cold]
    fn current_line(&self) -> String {
        format!(
            "scripts/iscript.txt:{}",
            self.iscript.line_info.pos_to_line(CodePosition::aice(self.opcode_pos)),
        )
    }

    fn get_sprite_local(&mut self, image: *mut bw::Image, id: u32) -> i32 {
        match self.state.get_sprite_local(image, id) {
            Some(s) => s,
            None => unsafe {
                error!(
                    "Error {}: image {:04x} accessed uninitialized spritelocal",
                    self.current_line(), (*image).image_id,
                );
                bw_print!(
                    "Error {}: image {:04x} accessed uninitialized spritelocal",
                    self.current_line(), (*image).image_id,
                );
                i32::min_value()
            }
        }
    }

    /// Return error pos on error.
    ///
    /// Some commands which may recurse to iscript running (create_unit)
    /// return a result to execute that outside of global locks.
    unsafe fn run_script(&mut self) -> Result<ScriptRunResult, u32> {
        use crate::parse::aice_op::*;
        'op_loop: while self.in_aice_code {
            let opcode_pos = self.pos as u32;
            self.opcode_pos = opcode_pos;
            let opcode = self.read_u8()?;
            match opcode {
                JUMP_TO_BW => {
                    let dest = self.read_u16()?;
                    (*self.bw_script).pos = dest;
                    return Ok(ScriptRunResult::Done);
                }
                ANIMATION_START => {
                    let header_index = ((*self.bw_script).header - 0x1c) / 8;
                    let animation = (*self.bw_script).animation;
                    let start_pos = self.iscript.headers.get(header_index as usize)
                        .and_then(|header| header.animations.get(animation as usize))
                        .and_then(|&x| x);
                    match start_pos {
                        Some(x) => self.jump_to(x),
                        None => {
                            // Can be valid since this fakes everything to have many animations
                            // and as such bw plays walking animation for every sprite not
                            // using flingy movement
                            (*self.bw_script).pos = 0x5;
                            return Ok(ScriptRunResult::Done);
                        }
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
                            self.state.set_sprite_local((*self.image).parent, id, value);
                        }
                        Place::Flingy(ty) => {
                            let flingy = self.unit.map(|x| *x)
                                .or_else(|| self.bullet.map(|x| x as *mut bw::Unit));
                            let flingy = match flingy {
                                Some(s) => s,
                                None => {
                                    self.report_missing_parent("flingy");
                                    show_unit_frame0_help();
                                    show_bullet_frame0_help();
                                    continue 'op_loop;
                                }
                            };
                            set_flingy_var(flingy, ty, value);
                        }
                        Place::Bullet(ty) => {
                            let bullet = match self.bullet {
                                Some(s) => s,
                                None => {
                                    self.report_missing_parent("bullet");
                                    show_bullet_frame0_help();
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
                                BulletVar::OrderTargetX => {
                                    (*bullet).order_target_pos.x = value as i16;
                                }
                                BulletVar::OrderTargetY => {
                                    (*bullet).order_target_pos.y = value as i16;
                                }
                            }
                        }
                        Place::Unit(ty) => {
                            let unit = match self.unit {
                                Some(s) => s,
                                None => {
                                    self.report_missing_parent("unit");
                                    show_unit_frame0_help();
                                    continue 'op_loop;
                                }
                            };
                            match ty {
                                UnitVar::DeathTimer => (**unit).death_timer = value as u16,
                                UnitVar::MatrixTimer => (**unit).matrix_timer = value as u8,
                                UnitVar::MatrixHp => (**unit).defensive_matrix_dmg = value as u16,
                                UnitVar::StimTimer => (**unit).stim_timer = value as u8,
                                UnitVar::EnsnareTimer => (**unit).ensnare_timer = value as u8,
                                UnitVar::LockdownTimer => (**unit).lockdown_timer = value as u8,
                                UnitVar::IrradiateTimer => (**unit).irradiate_timer = value as u8,
                                UnitVar::StasisTimer => (**unit).stasis_timer = value as u8,
                                UnitVar::PlagueTimer => (**unit).plague_timer = value as u8,
                                UnitVar::MaelstormTimer => (**unit).maelstrom_timer = value as u8,
                                UnitVar::IsBlind => (**unit).is_blind = value as u8,
                            }
                        },
                        Place::Image(ty) => {
                            let image = self.image;
                            match ty {
                                ImageVar::Drawfunc => (*image).drawfunc = value as u8,
                                ImageVar::DrawfuncParam => {
                                    (*image).drawfunc_param = value as usize as *mut c_void;
                                }
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
                    let sprite = Sprite::from_ptr((*self.image).parent)
                        .expect("Image had no parent??");
                    if sprite.images().count() == 1 {
                        // This is the last image being deleted, the sprite
                        // will be cleaned up as well
                        self.state.sprite_locals.remove(&SerializableSprite(*sprite));
                        self.init_sprite_owner();
                        if let Some(bullet) = self.bullet {
                            if (*bullet).state != 5 {
                                bw_print!(
                                    "ERROR {}: Bullet 0x{:x} ended its iscript \
                                    without bullet being in death state (bullet.state = 5)",
                                    self.current_line(), (*bullet).weapon_id,
                                );
                            }
                        } else if let Some(unit) = self.unit {
                            if unit.order() != bw_dat::order::DIE {
                                bw_print!(
                                    "ERROR {}: Unit 0x{:x} ended its iscript \
                                    while the unit is still alive",
                                    self.current_line(), unit.id().0,
                                );
                            }
                        }
                    }
                    (*self.bw_script).pos = 0x4;
                    self.in_aice_code = false;
                }
                SET_ORDER_WEAPON => {
                    let expr = self.read_u32()?;
                    if self.dry_run {
                        continue;
                    }
                    self.init_sprite_owner();
                    let unit = match self.unit {
                        Some(s) => s,
                        None => {
                            self.report_missing_parent("unit");
                            show_unit_frame0_help();
                            continue 'op_loop;
                        }
                    };
                    let sprite = (*self.image).parent;
                    let order_weapon =
                        (bw::orders_dat()[0xd].data as *mut u8).add(unit.order().0 as usize);
                    if expr != !0 {
                        let expression = &self.iscript.int_expressions[expr as usize];
                        let mut eval_ctx = self.eval_ctx();
                        let value = eval_ctx.eval_int(&expression);
                        let old = *order_weapon;
                        self.state.set_sprite_local(sprite, TEMP_LOCAL_ID, old as i32);
                        *order_weapon = value as u8;
                    } else {
                        *order_weapon = self.get_sprite_local(self.image, TEMP_LOCAL_ID) as u8;
                    }
                }
                CLEAR_ATTACKING_FLAG => {
                    if self.dry_run {
                        continue;
                    }
                    self.init_sprite_owner();
                    let unit = match self.unit {
                        Some(s) => s,
                        None => continue 'op_loop,
                    };
                    (**unit).flingy_flags &= !0x8;
                    (**unit).order_wait = 0;
                }
                CREATE_UNIT => {
                    let unit_id_expr = self.read_u32()? as usize;
                    let x_expr = self.read_u32()? as usize;
                    let y_expr = self.read_u32()? as usize;
                    let player_expr = self.read_u32()? as usize;
                    let unit_id = &self.iscript.int_expressions[unit_id_expr];
                    let x = &self.iscript.int_expressions[x_expr];
                    let y = &self.iscript.int_expressions[y_expr];
                    let player = &self.iscript.int_expressions[player_expr];
                    let mut eval_ctx = self.eval_ctx();
                    let unit_id = eval_ctx.eval_int(&unit_id);
                    let x = eval_ctx.eval_int(&x);
                    let y = eval_ctx.eval_int(&y);
                    let player = eval_ctx.eval_int(&player);
                    let unit_id = UnitId(unit_id as u16);
                    let pos = bw::Point {
                        x: x as i16,
                        y: y as i16,
                    };
                    let player = player as u8;
                    return Ok(ScriptRunResult::CreateUnit(unit_id, pos, player));
                }
                x => {
                    error!("Unknown opcode {:02x}", x);
                    return Err(opcode_pos);
                }
            }
        }
        Ok(ScriptRunResult::Done)
    }

    fn report_missing_parent(&self, parent: &str) {
        unsafe {
            let image_id = (*self.image).image_id;
            let sprite = bw_dat::Sprite::from_ptr((*self.image).parent)
                .unwrap_or_else(|| panic!("Fatal: image {:04x} has no parent sprite", image_id));
            let msg = format!(
                "Error {}: image {:04x} / sprite {:04x} has no {}",
                self.current_line(), image_id, sprite.id().0, parent
            );
            error!("{}", msg);
            bw_print!("{}", msg);
            let mut any_found = false;
            if let Some(unit) = self.sprite_owner_map.get_unit(*sprite) {
                bw_print!("Note: The sprite is owned by unit {:04x}", unit.id().0);
                any_found = true;
            }
            if let Some(bullet) = self.sprite_owner_map.get_bullet(*sprite) {
                bw_print!("Note: The sprite is owned by bullet {:04x}", (*bullet).weapon_id);
                any_found = true;
            }
            let mut lone = bw::first_lone_sprite();
            while !lone.is_null() {
                if (*lone).sprite == *sprite {
                    bw_print!("Note: The sprite is a parentless sprite (e.g. iscript sprol)");
                    any_found = true;
                    break;
                }
                lone = (*lone).next;
            }
            let mut fow = bw::first_fow_sprite();
            while !fow.is_null() {
                if (*fow).sprite == *sprite {
                    let unit_id = (*fow).hitpoints;
                    bw_print!("Note: The sprite is a fog sprite of unit {:04x}", unit_id);
                    any_found = true;
                    break;
                }
                fow = (*fow).next;
            }
            if !any_found {
                bw_print!("Note: One possibility is that the sprite is owned by a dying unit");
            }
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

/// Shows a message explaining that first frame of unit's iscript
/// cannot access the unit.
fn show_unit_frame0_help() {
    let unit_being_created = CREATING_UNIT.load(Ordering::Relaxed);
    if unit_being_created != !0 {
        bw_print!("Note: Unit id 0x{:x} is being created, but it does not exist while the \
            first frame of its iscript is being run", unit_being_created);
    }
}

fn show_bullet_frame0_help() {
    let bullet_being_created = CREATING_BULLET.load(Ordering::Relaxed);
    if bullet_being_created != !0 {
        bw_print!("Note: Bullet id 0x{:x} is being created, but it does not exist while the \
            first frame of its iscript is being run", bullet_being_created);
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
        // Todo
        FlingyVar::PositionX => bw_print!("Cannot set flingy pos"),
        FlingyVar::PositionY => bw_print!("Cannot set flingy pos"),
        FlingyVar::Player => bw_print!("Cannot set player"),
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
    // These maps may point to deleted units/bullets, all results must be verified
    // by checking unit.sprite == key
    unit_mapping: FxHashMap<*mut bw::Sprite, *mut bw::Unit>,
    bullet_mapping: FxHashMap<*mut bw::Sprite, *mut bw::Bullet>,
}

impl SpriteOwnerMap {
    pub fn new() -> SpriteOwnerMap {
        SpriteOwnerMap {
            unit_mapping: FxHashMap::with_capacity_and_hasher(1024, Default::default()),
            bullet_mapping: FxHashMap::with_capacity_and_hasher(256, Default::default()),
        }
    }

    pub fn add_unit(&mut self, unit: Unit, sprite: *mut bw::Sprite) {
        self.unit_mapping.insert(sprite, *unit);
    }

    pub fn add_bullet(&mut self, bullet: *mut bw::Bullet, sprite: *mut bw::Sprite) {
        self.bullet_mapping.insert(sprite, bullet);
    }

    pub fn get_unit(&self, sprite: *mut bw::Sprite) -> Option<Unit> {
        unsafe {
            if let Some(&unit) = self.unit_mapping.get(&sprite) {
                if (*unit).sprite == sprite {
                    return Unit::from_ptr(unit);
                }
            }
            None
        }
    }

    pub fn get_bullet(&self, sprite: *mut bw::Sprite) -> Option<*mut bw::Bullet> {
        unsafe {
            if let Some(&bullet) = self.bullet_mapping.get(&sprite) {
                if (*bullet).sprite == sprite {
                    return Some(bullet);
                }
            }
            None
        }
    }
}

pub fn rebuild_sprite_owners() {
    let mut map = SPRITE_OWNER_MAP.lock("rebuild_sprite_owners");
    map.unit_mapping.clear();
    map.bullet_mapping.clear();
    unsafe {
        let (units, len) = bw::unit_array();
        for i in 0..len {
            let unit = units.add(i);
            let sprite = (*unit).sprite;
            if !sprite.is_null() {
                map.add_unit(Unit::from_ptr(unit).unwrap(), sprite);
            }
        }
        let mut bullet = bw::first_active_bullet();
        while !bullet.is_null() {
            let sprite = (*bullet).sprite;
            if !sprite.is_null() {
                map.add_bullet(bullet, sprite);
            }
            bullet = (*bullet).next;
        }
    }
}

unsafe fn invalid_aice_command(iscript: *mut bw::Iscript, image: *mut bw::Image, offset: CodePos) {
    let sprite_id = match Sprite::from_ptr((*image).parent) {
        Some(sprite) => sprite.id().0,
        None => !0,
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
    assert!(iscript.bw_data.len() < 0x10000);
    std::ptr::copy_nonoverlapping(iscript.bw_data.as_ptr(), bw_bin, iscript.bw_data.len());
    *ISCRIPT.lock("set_as_bw_script") = Some(iscript);
}

pub unsafe extern fn iscript_read_hook(_filename: *const u8, out_size: *mut u32) -> *mut u8 {
    use winapi::um::heapapi::{GetProcessHeap, HeapAlloc};
    debug!("Iscript read hook");
    let data = HeapAlloc(GetProcessHeap(), 0, 0x10000) as *mut u8;
    *out_size = 0x10000;
    let iscript = crate::globals::init_for_lobby_map_preview();
    assert!(iscript.bw_data.len() < 0x10000);
    std::ptr::copy_nonoverlapping(iscript.bw_data.as_ptr(), data, iscript.bw_data.len());
    *ISCRIPT.lock("set_as_bw_script") = Some(iscript);
    data
}

pub unsafe extern fn create_unit_hook(
    id: u32,
    x: i32,
    y: i32,
    player: u32,
    skins: *const u8,
    orig: unsafe extern fn(u32, i32, i32, u32, *const u8) -> *mut c_void,
) -> *mut c_void {
    let prev_creating = CREATING_UNIT.load(Ordering::Relaxed);
    CREATING_UNIT.store(id as usize, Ordering::Relaxed);
    let result = orig(id, x, y, player, skins);
    CREATING_UNIT.store(prev_creating, Ordering::Relaxed);
    if let Some(unit) = Unit::from_ptr(result as *mut bw::Unit) {
        let sprite = (**unit).sprite;
        if sprite.is_null() {
            bw_print!("ERROR: Unit {:x} was created without a sprite", id);
            return null_mut();
        } else {
            let mut sprite_owner_map = SPRITE_OWNER_MAP.lock("create_unit_hook");
            sprite_owner_map.add_unit(unit, sprite);
            if let Some(subunit) = unit.subunit_linked() {
                sprite_owner_map.add_unit(subunit, (**subunit).sprite);
            }
        }
    }
    result
}

pub unsafe extern fn create_bullet_hook(
    id: u32,
    x: i32,
    y: i32,
    player: u32,
    direction: u32,
    parent: *mut c_void,
    orig: unsafe extern fn(u32, i32, i32, u32, u32, *mut c_void) -> *mut c_void,
) -> *mut c_void {
    let prev_creating = CREATING_BULLET.load(Ordering::Relaxed);
    CREATING_BULLET.store(id as usize, Ordering::Relaxed);
    let result = orig(id, x, y, player, direction, parent);
    CREATING_BULLET.store(prev_creating, Ordering::Relaxed);
    if result.is_null() == false {
        let bullet = result as *mut bw::Bullet;
        let sprite = (*bullet).sprite;
        if sprite.is_null() {
            bw_print!("ERROR: Bullet {:x} was created without a sprite", id);
            return null_mut();
        } else {
            let mut sprite_owner_map = SPRITE_OWNER_MAP.lock("create_bullet_hook");
            sprite_owner_map.add_bullet(bullet, sprite);
        }
    }
    result
}

// Account for unit's sprite having changed due to transforming
// by readding it to sprite_owner_map
// (Probably a bit inefficient but unit can transform due to order (could be caught here),
// but also player-sent command or even ai reacting to a hit)
pub unsafe extern fn order_hook(u: *mut c_void, orig: unsafe extern fn(*mut c_void)) {
    let unit = Unit::from_ptr(u as *mut bw::Unit).unwrap();
    if let Some(sprite) = unit.sprite() {
        let mut sprite_owner_map = SPRITE_OWNER_MAP.lock("order_hook");
        sprite_owner_map.add_unit(unit, *sprite);
        CURRENT_ORDER_SPRITE.store(*sprite as usize, Ordering::Relaxed);
    }
    CURRENT_ORDER_UNIT.store(u as usize, Ordering::Relaxed);
    orig(u);
    CURRENT_ORDER_UNIT.store(0, Ordering::Relaxed);
    CURRENT_ORDER_SPRITE.store(0, Ordering::Relaxed);
}
