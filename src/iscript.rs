use std::ffi::c_void;
use std::io::Write;
use std::mem;
use std::ptr::{self, null_mut, NonNull};
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, AtomicU32, Ordering};

use byteorder::{ByteOrder, LittleEndian};
use fxhash::FxHashMap;
use once_cell::sync::OnceCell;
use serde_derive::{Serialize, Deserialize};

use bw_dat::{
    Game, Image, ImageId, OrderId, Unit, UnitId, UpgradeId, Sprite, TechId, Race, WeaponId,
    UnitArray, SpriteId, FlingyId,
};

use crate::bw;
use crate::globals::{Globals, PlayerColorChoices, SerializableSprite, SerializableImage};
use crate::parse::{
    self, Int, Bool, CodePosition, CodePos, Iscript, Place, PlaceId, FlingyVar, BulletVar,
    UnitVar, ImageVar, GameVar, UnitRefId, UnitObjectOrVariable, UnitRefParts, SpriteLocalSetId,
    FormatStringId, AnyExpr, AnyTypeRef, AiceCommandParam, CommandParams, UnpackedExprId,
    LineInfo,
};
use crate::recurse_checked_mutex::{Mutex};
use crate::samase;
use crate::unit::UnitExt;
use crate::windows;

static ISCRIPT: Mutex<Option<Iscript>> = Mutex::new(None);
static LINE_INFO: parking_lot::Mutex<Option<Arc<LineInfo>>> = parking_lot::Mutex::new(None);
static SPRITE_OWNER_MAP: OnceCell<Mutex<SpriteOwnerMap>> = OnceCell::new();
static QUEUED_TRANSFORMS: AtomicU32 = AtomicU32::new(0);
static QUEUED_HIDE_SHOW: AtomicU32 = AtomicU32::new(0);

pub fn init_sprite_owner_map() {
    SPRITE_OWNER_MAP.get_or_init(|| Mutex::new(SpriteOwnerMap::new()));
}

#[derive(Serialize, Deserialize, Default)]
pub struct IscriptState {
    sprite_locals: FxHashMap<SerializableSprite, Vec<SpriteLocal>>,
    image_locals: FxHashMap<SerializableImage, FxHashMap<u32, u32>>,
    globals: Vec<i32>,
    queued_transforms: Vec<(u32, UnitId)>,
    /// hide = false, show = true
    queued_hide_show: Vec<(u32, bool)>,
    /// Highest bit = Currently valid, 0x7fff = Generation id
    /// Used for determining if a unit stored in variable is still
    /// valid or not when accessed.
    unit_generations: Vec<u16>,
    dry_run_call_stack: Vec<u32>,
    /// Index = image uid. Stores aice code pos after wait.
    /// Second vec has selection circles and hp bars
    wait_continue_pos: [Vec<u32>; 2],
    #[serde(skip)]
    dry_run_continue_pos: Option<(NonNull<bw::Iscript>, *mut bw::Image, u32)>,
    #[serde(skip)]
    with_vars: Vec<SpriteLocal>,
    #[serde(skip)]
    with_image: Option<ImageId>,
    #[serde(skip)]
    format_buffer: Vec<u8>,
    #[serde(skip)]
    unit_ext_field_to_samase: Vec<samase::ExtFieldId>,
}

unsafe impl Send for IscriptState {}
unsafe impl Sync for IscriptState {}

const TEMP_LOCAL_ID: u32 = !0;
const CALL_STACK_POS_ID: u32 = !1;
const FIRE_WEAPON_AT_TEMP_ID: u32 = !2;
const FIRE_WEAPON_AT_TEMP_ID2: u32 = !3;
const CALL_STACK_RETURN_POS_BASE: u32 = 0xff00_0000;
const CALL_STACK_LIMIT: u32 = 256;

/// Unit id of a unit which is being created, !0 for none.
/// Used as a stack as a unit creation iscript can cause another unit to be created.
static CREATING_UNIT: AtomicUsize = AtomicUsize::new(!0);
static CREATING_BULLET: AtomicUsize = AtomicUsize::new(!0);
/// Track corner case of sprite changing during order, and then an animation
/// being played with the new sprite which is now attached to CURRENT_ORDER_UNIT.
static CURRENT_ORDER_UNIT: AtomicUsize = AtomicUsize::new(0);
static CURRENT_ORDER_SPRITE: AtomicUsize = AtomicUsize::new(0);
/// Used for better panic info if the crate panics while running iscript
static CURRENT_SCRIPT_POS: AtomicU32 = AtomicU32::new(0);

pub fn current_script_line() -> Option<String> {
    let pos = CURRENT_SCRIPT_POS.load(Ordering::Relaxed).checked_sub(1)?;
    let line_info: Arc<LineInfo> = LINE_INFO.try_lock()?.as_ref()?.clone();
    Some(iscript_line(&line_info, CodePosition::aice(pos)))
}

#[cold]
fn iscript_line(line_info: &LineInfo, pos: CodePosition) -> String {
    format!("scripts/iscript.txt:{}", line_info.pos_to_line(pos))
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

    fn get_sprite_local_sprite(&self, sprite: Sprite, id: u32) -> Option<i32> {
        self.get_sprite_locals(*sprite)
            .and_then(|locals| locals.iter().find(|x| x.id == id))
            .map(|x| x.value)
    }

    fn set_sprite_local(&mut self, sprite: *mut bw::Sprite, id: u32, value: i32, if_uninit: bool) {
        let locals = self.get_sprite_locals_mut(sprite);
        match locals.iter().position(|x| x.id == id) {
            Some(s) => {
                if !if_uninit {
                    locals[s].value = value;
                }
            }
            None => locals.push(SpriteLocal {
                value,
                id,
            }),
        }
    }

    fn get_image_local(&self, image: *mut bw::Image, id: u32) -> Option<u32> {
        self.image_locals.get(&SerializableImage(image))?
            .get(&id)
            .copied()
    }

    fn set_image_local(&mut self, image: *mut bw::Image, id: u32, value: u32) {
        let entry = self.image_locals.entry(SerializableImage(image));
        entry.or_insert_with(|| FxHashMap::with_capacity_and_hasher(4, Default::default()))
            .insert(id, value);
    }

    fn get_sprite_locals_mut(&mut self, sprite: *mut bw::Sprite) -> &mut Vec<SpriteLocal> {
        self.sprite_locals.entry(SerializableSprite(sprite))
            .or_insert_with(|| Vec::new())
    }

    pub fn from_script(iscript: &Iscript) -> IscriptState {
        let mut globals = vec![0; iscript.global_count as usize];
        for &global_id in iscript.unit_type_globals() {
            globals[global_id as usize] = -1;
        }
        IscriptState {
            sprite_locals: FxHashMap::default(),
            image_locals: FxHashMap::default(),
            queued_transforms: Vec::new(),
            queued_hide_show: Vec::new(),
            globals,
            unit_generations: Vec::new(),
            dry_run_call_stack: Vec::new(),
            wait_continue_pos: [const { Vec::new() }; 2],
            dry_run_continue_pos: None,
            with_vars: Vec::new(),
            with_image: None,
            format_buffer: Vec::new(),
            // Inited in after_load
            unit_ext_field_to_samase: Vec::new(),
        }
    }

    fn queue_transform(&mut self, unit_array: UnitArray, unit: Unit, dest: UnitId) {
        let unit_uid = unit_array.to_unique_id(unit);
        self.queued_transforms.push((unit_uid, dest));
        QUEUED_TRANSFORMS.store(self.queued_transforms.len() as u32, Ordering::Relaxed);
    }

    fn queue_hide_show(&mut self, unit_array: UnitArray, unit: Unit, show: bool) {
        let unit_uid = unit_array.to_unique_id(unit);
        self.queued_hide_show.push((unit_uid, show));
        QUEUED_HIDE_SHOW.store(self.queued_hide_show.len() as u32, Ordering::Relaxed);
    }

    pub fn after_load(&mut self, iscript: &Iscript) {
        let unit_ext_field_to_samase = iscript.unit_ext_fields.iter()
            .map(|x| samase::create_extended_unit_field(&x))
            .collect();
        QUEUED_TRANSFORMS.store(self.queued_transforms.len() as u32, Ordering::Relaxed);
        QUEUED_HIDE_SHOW.store(self.queued_hide_show.len() as u32, Ordering::Relaxed);
        self.unit_ext_field_to_samase = unit_ext_field_to_samase;
    }
}

pub unsafe extern "C" fn run_aice_script(
    bw_pos: *mut c_void,
    iscript: *mut c_void,
    image: *mut c_void,
    dry_run: u32,
    _speed_out: *mut u32,
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
    let (active_unit, active_bullet) = bw::active_iscript_objects();
    let active_unit = Unit::from_ptr(active_unit);
    let active_bullet = Some(active_bullet).filter(|x| !x.is_null());

    let step_order_unit = CURRENT_ORDER_UNIT.load(Ordering::Relaxed);
    let sprite_owner_map_mutex = SPRITE_OWNER_MAP.get().unwrap();
    if step_order_unit != 0 {
        let unit = Unit::from_ptr(step_order_unit as *mut bw::Unit).unwrap();
        if let Some(sprite) = unit.sprite() {
            let expected_sprite = CURRENT_ORDER_SPRITE.load(Ordering::Relaxed);
            if *sprite != expected_sprite as *mut bw::Sprite {
                let mut sprite_owner_map = sprite_owner_map_mutex.lock("run_aice_script");
                sprite_owner_map.add_unit(unit, *sprite);
                CURRENT_ORDER_SPRITE.store(*sprite as usize, Ordering::Relaxed);
            }
        }
    }

    let prev_script_pos = CURRENT_SCRIPT_POS.load(Ordering::Relaxed);
    'main_loop: loop {
        let mut globals_guard = Globals::get("run_aice_script");
        let globals = &mut *globals_guard;
        let mut sprite_owner_map = sprite_owner_map_mutex.lock("run_aice_script");
        let mut runner = IscriptRunner::new(
            this,
            &mut globals.iscript_state,
            aice_offset,
            iscript,
            image,
            dry_run != 0,
            game,
            &mut globals.player_lobby_color_choices,
            &mut sprite_owner_map,
            active_unit,
            active_bullet,
        );
        let result = runner.run_script();
        aice_offset = runner.pos;
        match result {
            Ok(ScriptRunResult::Done) => break 'main_loop,
            Ok(ScriptRunResult::CreateUnit(unit_id, pos, player)) => {
                drop(globals_guard);
                drop(sprite_owner_map);
                drop(this_guard);
                if let Some(unit) = bw::create_unit(unit_id, &pos, player) {
                    bw::finish_unit_pre(unit);
                    bw::finish_unit_post(unit);
                    bw::give_ai(unit);
                    bw::place_finished_unit_creep(unit_id, &pos);
                } else {
                    let mut globals_guard = Globals::get("run_aice_script");
                    let globals = &mut *globals_guard;
                    globals.iscript_state.with_image = None;
                }
            }
            Ok(ScriptRunResult::CreateSprite(sprite_id, pos, player, elevation)) => {
                drop(globals_guard);
                drop(sprite_owner_map);
                drop(this_guard);
                if let Some(owner) = samase::create_lone_sprite(
                    sprite_id,
                    pos.x as i32,
                    pos.y as i32,
                    player,
                ) {
                    (*(*owner.as_ptr()).sprite).elevation_level = elevation;
                } else {
                    let mut globals_guard = Globals::get("run_aice_script");
                    let globals = &mut *globals_guard;
                    globals.iscript_state.with_image = None;
                }
            }
            Ok(ScriptRunResult::IssueOrder(unit, order, pos)) => {
                drop(globals_guard);
                drop(sprite_owner_map);
                drop(this_guard);
                if order.is_secondary() {
                    unit.issue_secondary_order(order);
                } else {
                    bw::issue_order(*unit, order, pos, null_mut(), bw_dat::unit::NONE);
                }
            }
            Ok(ScriptRunResult::AddOverlay(base, image_id, x, y, above)) => {
                drop(globals_guard);
                drop(sprite_owner_map);
                drop(this_guard);
                crate::samase::add_overlay_iscript(*base, image_id, x, y, above);
            }
            Ok(ScriptRunResult::GiveUnit(unit, player)) => {
                drop(globals_guard);
                drop(sprite_owner_map);
                drop(this_guard);
                crate::samase::give_unit(*unit, player);
            }
            Ok(ScriptRunResult::TransformUnit(unit, id)) => {
                // When transforming unit, the sprite will be deleted and reset,
                // so iscript has to stop.
                // This check may have to be more thorough too, check subunits etc?
                // Maybe this iscript loop will have to check if inner callback has transformed
                // the current unit too?
                let is_active = Some(unit) == active_unit;
                if is_active {
                    let unit_array = runner.unit_array();
                    globals.iscript_state.queue_transform(unit_array, unit, id);
                    drop(this_guard);
                } else {
                    drop(globals_guard);
                    drop(sprite_owner_map);
                    drop(this_guard);
                    crate::samase::transform_unit(*unit, id);
                }
            }
            Ok(ScriptRunResult::HideUnit(unit)) => {
                // Hide/show has always be queued, since
                // 1) It may remove next active / hidden unit which was to be processed
                //      when game is looping through units (Current one would be fine to remove)
                // 2) Hiding removes selection overlays, likely immediately breaking at next image
                //      of step_sprite_frame
                // Could queue then active == unit to be done immediately after step_order
                // though?
                let unit_array = runner.unit_array();
                globals.iscript_state.queue_hide_show(unit_array, unit, false);
                drop(this_guard);
            }
            Ok(ScriptRunResult::ShowUnit(unit)) => {
                let unit_array = runner.unit_array();
                globals.iscript_state.queue_hide_show(unit_array, unit, true);
                drop(this_guard);
            }
            Ok(ScriptRunResult::SetHp(unit, value)) => {
                drop(globals_guard);
                drop(sprite_owner_map);
                drop(this_guard);
                if value <= 0 {
                    crate::samase::kill_unit(*unit);
                } else {
                    crate::samase::unit_set_hp(*unit, value);
                }
            }
            Err(pos) => {
                invalid_aice_command(iscript, image, CodePos::Aice(pos));
                break 'main_loop;
            }
        }
        this_guard = ISCRIPT.lock("run_aice_script");
        this = this_guard.as_ref().unwrap();
    }
    CURRENT_SCRIPT_POS.store(prev_script_pos, Ordering::Relaxed);
}

struct CustomCtx<'a, 'b> {
    parent: &'a mut IscriptRunner<'b>,
    image: *mut bw::Image,
    unit: Option<Unit>,
    bullet: Option<*mut bw::Bullet>,
    evaluate_default: bool,
}

impl<'a, 'b> bw_dat::expr::CustomEval for CustomCtx<'a, 'b> {
    type State = parse::ExprState;

    fn eval_int(&mut self, val: &Int) -> i32 {
        if self.evaluate_default {
            // Early exit when this is defaulting anyway
            return 0;
        }
        let parent = &mut *self.parent;
        match val {
            Int::Variable(id, place_vars) => {
                match id.place() {
                    Place::Global(id) => parent.get_global(id),
                    Place::SpriteLocal(unit_ref, id) => {
                        if let Some(val) = parent.get_sprite_local(id, unit_ref) {
                            val
                        } else {
                            if unit_ref.is_this() {
                                parent.show_uninit_spritelocal_err();
                            } else {
                                self.evaluate_default = true;
                            }
                            i32::MIN
                        }
                    }
                    Place::Flingy(unit_ref, ty) => unsafe {
                        let flingy = if unit_ref.is_this() {
                            self.unit.map(|x| ptr::addr_of_mut!((**x).flingy))
                                .or_else(|| self.bullet.map(|x| ptr::addr_of_mut!((*x).flingy)))
                        } else {
                            match parent.resolve_unit_ref(unit_ref) {
                                Some(s) => Some(ptr::addr_of_mut!((**s).flingy)),
                                None => {
                                    self.evaluate_default = true;
                                    return 0;
                                }
                            }
                        };
                        let flingy = match flingy {
                            Some(s) => s,
                            None => {
                                parent.report_missing_parent("flingy");
                                show_unit_frame0_help();
                                show_bullet_frame0_help();
                                return i32::MIN;
                            }
                        };
                        match ty {
                            FlingyVar::MoveTargetX => (*flingy).move_target.pos.x as i32,
                            FlingyVar::MoveTargetY => (*flingy).move_target.pos.y as i32,
                            FlingyVar::FacingDirection => {
                                bw_angle_to_degrees((*flingy).facing_direction) as i32
                            }
                            FlingyVar::MovementDirection => {
                                bw_angle_to_degrees((*flingy).movement_direction) as i32
                            }
                            FlingyVar::TargetDirection => {
                                bw_angle_to_degrees((*flingy).target_direction) as i32
                            }
                            FlingyVar::TurnSpeed => (*flingy).turn_speed as i32,
                            FlingyVar::Acceleration => (*flingy).acceleration as i32,
                            FlingyVar::TopSpeed => (*flingy).top_speed as i32,
                            FlingyVar::Speed => (*flingy).current_speed as i32,
                            FlingyVar::PositionX => (*flingy).position.x as i32,
                            FlingyVar::PositionY => (*flingy).position.y as i32,
                            FlingyVar::Player => (*(flingy as *mut bw::Unit)).player as i32,
                            FlingyVar::FlingyId => (*flingy).flingy_id as i32,
                            FlingyVar::Flags => (*flingy).flingy_flags as i32,
                        }
                    },
                    Place::Bullet(ty) => unsafe {
                        let bullet = match self.bullet {
                            Some(s) => s,
                            None => {
                                parent.report_missing_parent("bullet");
                                return i32::MIN;
                            }
                        };
                        match ty {
                            BulletVar::State => (*bullet).state as i32,
                            BulletVar::WeaponId => (*bullet).weapon_id as i32,
                            BulletVar::BouncesRemaining => (*bullet).bounces_remaining as i32,
                            BulletVar::DeathTimer => (*bullet).death_timer as i32,
                            BulletVar::OrderTargetX => (*bullet).target.pos.x as i32,
                            BulletVar::OrderTargetY => (*bullet).target.pos.y as i32,
                            BulletVar::Flags => (*bullet).flags as i32,
                        }
                    },
                    Place::Unit(unit_ref, ty) => unsafe {
                        let unit = match parent.resolve_unit_ref(unit_ref) {
                            Some(s) => s,
                            None => {
                                if unit_ref.is_this() {
                                    parent.report_missing_parent("unit");
                                } else {
                                    self.evaluate_default = true;
                                }
                                return i32::MIN;
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
                            UnitVar::SpellUpdateTimer => (**unit).master_spell_timer as i32,
                            UnitVar::ParasitePlayers => (**unit).parasited_by_players as i32,
                            UnitVar::IsBeingHealed => (**unit).is_being_healed as i32,
                            UnitVar::IsUnderStorm => (**unit).is_under_storm as i32,
                            UnitVar::Hitpoints => unit.hitpoints(),
                            UnitVar::Shields => unit.shields(),
                            UnitVar::Energy => unit.energy() as i32,
                            UnitVar::MaxHitpoints => unit.id().hitpoints(),
                            UnitVar::MaxShields => unit.id().shields(),
                            UnitVar::MaxEnergy => {
                                parent.game.max_energy(unit.player(), unit.id()) as i32
                            }
                            UnitVar::MineralCost => unit.id().mineral_cost() as i32,
                            UnitVar::GasCost => unit.id().gas_cost() as i32,
                            UnitVar::SupplyCost => unit.id().supply_cost() as i32,
                            UnitVar::OverlaySize => {
                                match unit.id().flags() {
                                    x if x & 0x0200_0000 != 0 => 1,
                                    x if x & 0x0400_0000 != 0 => 2,
                                    _ => 0,
                                }
                            }
                            UnitVar::Resources => match unit.id().is_resource_container() {
                                true => unit.resource_amount() as i32,
                                false => 0,
                            },
                            UnitVar::HangarCountInside => match unit.uses_fighters() {
                                true => (**unit).unit_specific.carrier.in_hangar_count as i32,
                                false => 0,
                            },
                            UnitVar::HangarCountOutside => match unit.uses_fighters() {
                                true => (**unit).unit_specific.carrier.out_hangar_count as i32,
                                false => 0,
                            },
                            UnitVar::LoadedCount => unit.cargo_count() as i32,
                            UnitVar::CurrentTech => unit.tech_in_progress()
                                .unwrap_or(bw_dat::tech::NONE).0 as i32,
                            UnitVar::CurrentUpgrade => unit.upgrade_in_progress()
                                .unwrap_or(bw_dat::upgrade::NONE).0 as i32,
                            UnitVar::BuildQueue => {
                                let mut child_ctx = parent.eval_ctx();
                                let val = place_vars[0].as_ref()
                                    .and_then(|x| u8::try_from(child_ctx.eval_int(x)).ok())
                                    .and_then(|x| unit.nth_queued_unit(x))
                                    .unwrap_or(bw_dat::unit::NONE).0 as i32;
                                self.evaluate_default = child_ctx.custom.evaluate_default;
                                val
                            }
                            UnitVar::RemainingBuildTime => (**unit).remaining_build_time as i32,
                            UnitVar::RemainingResearchTime => {
                                if unit.tech_in_progress().is_some() ||
                                    unit.upgrade_in_progress().is_some()
                                {
                                    (**unit).unit_specific.building.research_time_remaining as i32
                                } else {
                                    0
                                }
                            }
                            UnitVar::UnitId => unit.id().0 as i32,
                            UnitVar::Kills => unit.kills() as i32,
                            UnitVar::CarriedResourceAmount =>
                                unit.carried_resource_amount() as i32,
                            UnitVar::GroundCooldown => (**unit).ground_cooldown as i32,
                            UnitVar::AirCooldown => (**unit).air_cooldown as i32,
                            UnitVar::SpellCooldown => (**unit).spell_cooldown as i32,
                            UnitVar::Order => unit.order().0 as i32,
                            UnitVar::OrderTimer => (**unit).order_timer as i32,
                            UnitVar::OrderState => (**unit).order_state as i32,
                            UnitVar::RankIncrease => (**unit).rank as i32,
                            UnitVar::MineAmount => unit.mine_amount(parent.game) as i32,
                            UnitVar::RallyX | UnitVar::RallyY => {
                                if unit.id().is_building() && unit.id() != bw_dat::unit::PYLON {
                                    if ty == UnitVar::RallyX {
                                        (**unit).rally_pylon.rally.pos.x as i32
                                    } else {
                                        (**unit).rally_pylon.rally.pos.y as i32
                                    }
                                } else {
                                    0
                                }
                            }
                            UnitVar::Flags => (**unit).flags as i32,
                            UnitVar::DetectionStatus => (**unit).detection_status as i32,
                            UnitVar::PathingFlags => (**unit).pathing_flags as i32,
                            UnitVar::MovementState => (**unit).movement_state as i32,
                            UnitVar::RepulseX => (**unit).repulse_chunk_x as i32,
                            UnitVar::RepulseY => (**unit).repulse_chunk_y as i32,
                            UnitVar::RepulseMisc => (**unit).repulse_misc as i32,
                            UnitVar::RepulseDirection => {
                                bw_angle_to_degrees((**unit).repulse_direction) as i32
                            }
                            UnitVar::CurrentButtonSet => (**unit).buttons as i32,
                            UnitVar::AirStrength => (**unit).air_strength as i32,
                            UnitVar::GroundStrength => (**unit).ground_strength as i32,
                            UnitVar::OrderTargetX => (**unit).order_target.pos.x as i32,
                            UnitVar::OrderTargetY => (**unit).order_target.pos.y as i32,
                            UnitVar::CloakCount => (**unit).invisibility_effects as i32,
                            UnitVar::SecondaryOrder => (**unit).secondary_order as i32,
                            UnitVar::SecondaryOrderState => (**unit).secondary_order_state as i32,
                            UnitVar::LastAttackingPlayer => (**unit).last_attacking_player as i32,
                        }
                    },
                    Place::UnitExt(unit_ref, field_id) => {
                        let unit = match parent.resolve_unit_ref(unit_ref) {
                            Some(s) => s,
                            None => {
                                if unit_ref.is_this() {
                                    parent.report_missing_parent("unit");
                                } else {
                                    self.evaluate_default = true;
                                }
                                return i32::MIN;
                            }
                        };
                        let unit_index = parent.unit_array().to_index(unit) as u32;
                        let field_id = parent.samase_unit_ext_field_id(field_id);
                        samase::read_extended_unit_field(unit_index, field_id) as i32
                    }
                    Place::Image(ty) => unsafe {
                        let image = self.image;
                        match ty {
                            ImageVar::Drawfunc => (*image).drawfunc as i32,
                            ImageVar::DrawfuncParam => (*image).drawfunc_param as i32,
                            ImageVar::Frame => (*image).frame as i32,
                            ImageVar::BaseFrame => (*image).frameset as i32,
                        }
                    },
                    Place::Game(ty) => unsafe {
                        use crate::parse::GameVar::*;
                        let mut child_ctx = parent.eval_ctx();
                        let mut vars = [0i32; 4];
                        for i in 0..place_vars.len() {
                            if let Some(ref expr) = place_vars[i] {
                                vars[i] = child_ctx.eval_int(expr);
                                if child_ctx.custom.evaluate_default {
                                    self.evaluate_default = true;
                                    return 0;
                                }
                            }
                        }
                        let game = parent.game;
                        // Moving this outside match since it is pretty common
                        let player = (vars[0] as u8).clamp(0, 11) as u8;
                        match ty {
                            Deaths => game.unit_deaths(player, UnitId(vars[1] as u16)) as i32,
                            Kills => game.unit_kills(player, UnitId(vars[1] as u16)) as i32,
                            UpgradeLevel => {
                                game.upgrade_level(player, UpgradeId(vars[1] as u16)) as i32
                            }
                            UpgradeLimit => {
                                game.upgrade_max_level(player, UpgradeId(vars[1] as u16)) as i32
                            }
                            TechLevel => {
                                game.tech_researched(player, TechId(vars[1] as u16)) as i32
                            }
                            TechAvailability => {
                                game.tech_available(player, TechId(vars[1] as u16)) as i32
                            }
                            UnitAvailability => {
                                game.unit_available(player, UnitId(vars[1] as u16)) as i32
                            }
                            Alliance => game.allied(player, vars[1].clamp(0, 11) as u8) as i32,
                            SharedVision => {
                                game.shared_vision(player, vars[1].clamp(0, 11) as u8) as i32
                            }
                            Minerals => game.minerals(player) as i32,
                            Gas => game.gas(player) as i32,
                            ZergSupplyMax | TerranSupplyMax | ProtossSupplyMax |
                                ZergSupplyUsed | TerranSupplyUsed | ProtossSupplyUsed |
                                ZergSupplyProvided | TerranSupplyProvided |
                                ProtossSupplyProvided =>
                            {
                                let race = match ty {
                                    ZergSupplyMax | ZergSupplyUsed | ZergSupplyProvided => {
                                        Race::Zerg
                                    }
                                    TerranSupplyMax | TerranSupplyUsed | TerranSupplyProvided => {
                                        Race::Terran
                                    }
                                    _ => Race::Protoss,
                                };
                                match ty {
                                    ZergSupplyMax | TerranSupplyMax | ProtossSupplyMax => {
                                        game.supply_max(player, race) as i32
                                    }
                                    ZergSupplyUsed | TerranSupplyUsed | ProtossSupplyUsed => {
                                        game.supply_used(player, race) as i32
                                    }
                                    _ => {
                                        game.supply_provided(player, race) as i32
                                    }
                                }
                            }
                            UnitsTotal | UnitsProduced | UnitsOwned | UnitsLost | UnitsKilled |
                                UnitsScore | UnitsKilledScore | BuildingsTotal |
                                BuildingsConstructed | BuildingsOwned | BuildingsLost |
                                BuildingsRazed | BuildingsScore | BuildingsRazedScore|
                                FactoriesConstructed | FactoriesOwned | FactoriesLost |
                                FactoriesRazed =>
                            {
                                let index = ty as u8 - UnitsTotal as u8;
                                game.score(index, player) as i32
                            }
                            CustomScore => game.custom_score(player) as i32,
                            PlayerColorChoice => {
                                if crate::samase::is_multiplayer() {
                                    parent.player_lobby_color_choices.get(player) as i32
                                } else {
                                    0x17
                                }
                            }
                            PlayerType | PlayerRace => {
                                let players = crate::samase::players();
                                let player = players.add(player as usize);
                                if ty == PlayerType {
                                    (*player).player_type as i32
                                } else {
                                    (*player).race as i32
                                }
                            }
                            LocationLeft | LocationTop | LocationRight | LocationBottom => {
                                let location = (vars[0]).clamp(0, 254) as u8;
                                let location = (**game).locations[location as usize];
                                match ty {
                                    LocationLeft => location.area.left as i32,
                                    LocationTop => location.area.top as i32,
                                    LocationRight => location.area.right as i32,
                                    LocationBottom | _ => location.area.bottom as i32,
                                }
                            }
                            LeaderboardType => (**game).leaderboard_type as i32,
                            LeaderboardLocation => (**game).leaderboard_location as i32,
                            LeaderboardParameter => (**game).leaderboard_parameter as i32,
                            LeaderboardGoal => (**game).leaderboard_goal as i32,
                            LeaderboardComputers => (**game).computers_in_leaderboard as i32,
                            Dat => self.parent.read_dat(vars[0], vars[1], vars[2]),
                        }
                    },
                }
            }
            Int::Default(ref pair) => {
                let mut child_ctx = parent.eval_ctx();
                let mut val = child_ctx.eval_int(&pair.0);
                if child_ctx.custom.evaluate_default {
                    child_ctx.custom.evaluate_default = false;
                    val = child_ctx.eval_int(&pair.1);
                    self.evaluate_default = child_ctx.custom.evaluate_default;
                }
                val
            }
        }
    }

    fn eval_bool(&mut self, val: &Bool) -> bool {
        if self.evaluate_default {
            // Early exit when this is defaulting anyway
            return false;
        }
        match *val {
            Bool::HasFlingy => self.bullet.is_some() || self.unit.is_some(),
            Bool::HasBullet => self.bullet.is_some(),
            Bool::Has(unit) => self.parent.resolve_unit_ref(unit).is_some(),
            Bool::Default(ref pair) => {
                let mut child_ctx = self.parent.eval_ctx();
                let mut val = child_ctx.eval_bool(&pair.0);
                if child_ctx.custom.evaluate_default {
                    child_ctx.custom.evaluate_default = false;
                    val = child_ctx.eval_bool(&pair.1);
                    self.evaluate_default = child_ctx.custom.evaluate_default;
                }
                val
            }
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
    map_tile_flags: Option<*mut u32>,
    bullet: Option<*mut bw::Bullet>,
    player_lobby_color_choices: &'a mut PlayerColorChoices,
    active_iscript_unit: Option<Unit>,
    active_iscript_bullet: Option<*mut bw::Bullet>,
    unit_array: Option<UnitArray>,
}

enum ScriptRunResult {
    Done,
    CreateUnit(UnitId, bw::Point, u8),
    CreateSprite(SpriteId, bw::Point, u8, u8),
    IssueOrder(Unit, OrderId, bw::Point),
    AddOverlay(Image, ImageId, i8, i8, bool),
    GiveUnit(Unit, u8),
    TransformUnit(Unit, UnitId),
    HideUnit(Unit),
    ShowUnit(Unit),
    SetHp(Unit, i32),
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
        player_lobby_color_choices: &'a mut PlayerColorChoices,
        sprite_owner_map: &'a mut SpriteOwnerMap,
        active_iscript_unit: Option<Unit>,
        active_iscript_bullet: Option<*mut bw::Bullet>,
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
            map_tile_flags: None,
            unit_array: None,
            player_lobby_color_choices,
            active_iscript_unit,
            active_iscript_bullet,
        }
    }

    fn init_sprite_owner(&mut self) {
        unsafe {
            if !self.owner_read {
                let sprite = (*self.image).parent;
                self.owner_read = true;
                self.unit = self.active_iscript_unit
                    .filter(|x| match x.sprite() {
                        Some(x) => *x == sprite,
                        None => false,
                    })
                    .or_else(|| self.sprite_owner_map.get_unit(sprite))
                    .or_else(|| {
                        // Semi-hacky fallback to make pylon auras use parent unit as well
                        // when stepping their frame.
                        let step_order_unit = CURRENT_ORDER_UNIT.load(Ordering::Relaxed);
                        if let Some(unit) = Unit::from_ptr(step_order_unit as *mut bw::Unit) {
                            if let Some(aura) = unit.pylon_aura() {
                                if *aura == sprite {
                                    return Some(unit);
                                }
                            }
                        }
                        None
                    });
                self.bullet = self.active_iscript_bullet
                    .filter(|&x| (*x).flingy.sprite == sprite)
                    .or_else(|| self.sprite_owner_map.get_bullet(sprite));
            }
        }
    }

    fn eval_ctx<'s>(&'s mut self) -> bw_dat::expr::EvalCtx<CustomCtx<'s, 'a>> {
        self.init_sprite_owner();
        bw_dat::expr::EvalCtx {
            game: Some(self.game),
            unit: self.unit,
            map_tile_flags:
                Some(*self.map_tile_flags.get_or_insert_with(|| bw::map_tile_flags())),
            custom: CustomCtx {
                image: self.image,
                bullet: self.bullet,
                unit: self.unit,
                parent: self,
                evaluate_default: false,
            },
        }
    }

    fn samase_unit_ext_field_id(&self, in_id: parse::UnitExtId) -> samase::ExtFieldId {
        let id = match self.state.unit_ext_field_to_samase.get(in_id.0 as usize) {
            Some(&s) => s,
            None => samase::ExtFieldId(0),
        };
        debug_assert!(id.0 != 0, "Tried to get id {}", in_id.0);
        id
    }

    fn unit_array(&mut self) -> UnitArray {
        *self.unit_array.get_or_insert_with(|| bw::unit_array())
    }

    #[cold]
    fn current_line(&self) -> String {
        iscript_line(&self.iscript.line_info, CodePosition::aice(self.opcode_pos))
    }

    fn get_sprite_local(&mut self, id: u32, unit_ref: UnitRefId) -> Option<i32> {
        let sprite = if unit_ref.is_this() {
            unsafe { Sprite::from_ptr((*self.image).parent)? }
        } else {
            self.resolve_unit_ref(unit_ref)?.sprite()?
        };
        self.state.get_sprite_local_sprite(sprite, id)
    }

    fn show_uninit_spritelocal_err(&self) {
        unsafe {
            let image = self.image;
            bw_print_error!(
                "Error {}: image {:04x} accessed uninitialized spritelocal",
                self.current_line(), (*image).image_id,
            );
        }
    }

    fn get_global(&self, id: u32) -> i32 {
        self.state.globals[id as usize]
    }

    fn resolve_unit_ref_part(
        &mut self,
        unit_obj: UnitObjectOrVariable,
        unit: Option<Unit>,
        bullet: Option<*mut bw::Bullet>,
    ) -> Option<Unit> {
        use crate::parse::UnitObject::*;
        if let Some(unit_obj) = unit_obj.if_unit_object() {
            match unit_obj {
                This => unit,
                UnitParent => {
                    let unit = unit?;
                    let id = unit.id();
                    match id {
                        bw_dat::unit::NUCLEAR_MISSILE | bw_dat::unit::LARVA => unit.related(),
                        bw_dat::unit::INTERCEPTOR | bw_dat::unit::SCARAB => unit.fighter_parent(),
                        _ => {
                            if id.is_powerup() {
                                unit.powerup_worker()
                            } else if id.is_subunit() {
                                unit.subunit_linked()
                            } else if id.is_addon() {
                                unit.related()
                            } else {
                                None
                            }
                        }
                    }
                }
                Nuke => {
                    let unit = unit?;
                    match unit.id() {
                        bw_dat::unit::NUCLEAR_SILO => unit.silo_nuke(),
                        bw_dat::unit::GHOST => unit.related(),
                        _ => None,
                    }
                }
                CurrentlyBuilding => {
                    let unit = unit?;
                    if unit.id() == bw_dat::unit::SCV {
                        if unit.order() == bw_dat::order::SCV_BUILD {
                            unit.target()
                        } else {
                            None
                        }
                    } else if unit.id() == bw_dat::unit::NYDUS_CANAL {
                        unit.nydus_linked().filter(|x| !x.is_completed())
                    } else {
                        unit.currently_building()
                    }
                }
                UnitTarget => unit?.target(),
                Transport => {
                    let unit = unit?;
                    match unit.in_transport() {
                        true => unit.related(),
                        false => None,
                    }
                }
                Addon => unit?.addon(),
                Subunit => unit.filter(|x| !x.id().is_subunit())?.subunit_linked(),
                LinkedNydus => unit?.nydus_linked().filter(|x| x.is_completed()),
                Powerup => unit?.powerup(),
                RallyTarget => unit?.rally_unit(),
                IrradiatedBy => unsafe { Unit::from_ptr((**unit?).irradiated_by) },
                BulletParent => unsafe { Unit::from_ptr((*bullet?).parent) },
                BulletTarget => unsafe { Unit::from_ptr((*bullet?).target.unit) },
                BulletPreviousBounceTarget => {
                    unsafe { Unit::from_ptr((*bullet?).previous_bounce_target) }
                }
                Null | _Last => None,
            }
        } else {
            let id = unit_obj.variable_id();
            let value = if unit_obj.is_global() {
                self.get_global(id)
            } else {
                // If there is no unit, assume that we are on first part of the chain
                // refering to current sprite (It could also have an unit but then
                // it works anyway)
                let sprite = match unit {
                    Some(s) => s.sprite()?,
                    None => unsafe { Sprite::from_ptr((*self.image).parent)? },
                };
                self.state.get_sprite_local_sprite(sprite, id)?
            };
            self.variable_to_unit(value)
        }
    }

    fn resolve_unit_ref(&mut self, unit_ref: UnitRefId) -> Option<Unit> {
        let mut unit = self.get_unit_silent();
        let bullet = self.get_bullet_silent();
        match self.iscript.unit_ref_object(unit_ref) {
            UnitRefParts::Single(unit_obj) => {
                self.resolve_unit_ref_part(unit_obj, unit, bullet)
            }
            UnitRefParts::Many(parts) => {
                for &part in parts {
                    unit = Some(self.resolve_unit_ref_part(part, unit, bullet)?);
                }
                unit
            }
        }
    }

    /// Currently requires place to be an spritelocal, else returns None
    fn resolve_unit_ref_as_place(&mut self, unit_ref: UnitRefId) -> Option<(Unit, u32)> {
        let mut unit = self.get_unit_silent();
        let bullet = self.get_bullet_silent();
        match self.iscript.unit_ref_object(unit_ref) {
            UnitRefParts::Single(unit_obj) => {
                let id = unit_obj.if_sprite_local()?;
                Some((unit?, id))
            }
            UnitRefParts::Many(parts) => {
                let (last, rest) = parts.split_last()?;
                let id = last.if_sprite_local()?;
                for &part in rest {
                    unit = Some(self.resolve_unit_ref_part(part, unit, bullet)?);
                }
                Some((unit?, id))
            }
        }
    }

    fn variable_to_unit(&mut self, var: i32) -> Option<Unit> {
        if var == -1 {
            // Early exit, would be caught by unit_array.get_by_index though
            return None;
        }
        let var = var as u32;
        let index = var & 0xffff;
        let gen = (var >> 16) as u16;
        let &current_gen = self.state.unit_generations.get(index as usize)?;
        if gen | 0x8000 == current_gen {
            let unit_array = self.unit_array();
            unit_array.get_by_index(index)
        } else {
            None
        }
    }

    fn unit_to_variable(&mut self, unit: Option<Unit>) -> i32 {
        match unit {
            Some(unit) => {
                let unit_array = self.unit_array();
                let index = unit_array.to_index(unit);
                if index >= 0xffff {
                    return -1;
                }
                let generation = self.unit_generation_add(&unit_array, index);
                (((generation as u32) << 16) | index) as i32
            }
            None => -1,
        }
    }

    /// Gets generation number for unit, using the current one if it hasn't been changed
    /// or generates the next one and marks it valid.
    fn unit_generation_add(&mut self, unit_array: &UnitArray, index: u32) -> u16 {
        let index = index as usize;
        let state = &mut self.state;
        if state.unit_generations.len() == 0 {
            state.unit_generations.resize_with(unit_array.len(), || 0);
        }
        if let Some(entry) = state.unit_generations.get_mut(index) {
            let value = *entry;
            if value & 0x8000 != 0 {
                value & 0x7fff
            } else {
                if value == 0x7fff {
                    self.gc_unit_generations();
                    self.unit_generation_add(unit_array, index as u32)
                } else {
                    *entry = (value + 1) | 0x8000;
                    value + 1
                }
            }
        } else {
            0
        }
    }

    fn gc_unit_generations(&mut self) {
        let state = &mut self.state;
        // Go through all variables, if their unit is invalid gen then make
        // variable invalid, otherwise make it gen 0
        // Then make all unit generations 0 (Keep valid ones valid)
        for &global_id in self.iscript.unit_type_globals() {
            if let Some(entry) = state.globals.get_mut(global_id as usize) {
                let value = *entry as u32;
                let index = value & 0xffff;
                let gen = (value >> 16) as u16;
                if let Some(&current_gen) = state.unit_generations.get(index as usize) {
                    if current_gen == gen | 0x8000 {
                        *entry = index as i32;
                    } else {
                        *entry = -1;
                    }
                }
            }
        }
        for sprite_local_list in state.sprite_locals.values_mut() {
            for entry in sprite_local_list {
                if self.iscript.is_unit_sprite_local(entry.id) {
                    let value = entry.value as u32;
                    let index = value & 0xffff;
                    let gen = (value >> 16) as u16;
                    if let Some(&current_gen) = state.unit_generations.get(index as usize) {
                        if current_gen == gen | 0x8000 {
                            entry.value = index as i32;
                        } else {
                            entry.value = -1;
                        }
                    }
                }
            }
        }
        for entry in &mut state.unit_generations {
            *entry &= 0x8000;
        }
    }

    fn set_with_vars(&mut self, image: ImageId, locals: SpriteLocalSetId) {
        let set = self.iscript.sprite_local_set(locals);
        if set.is_empty() {
            return;
        }
        if let Some(image) = self.state.with_image {
            bw_print!(
                "Warning: Spritelocals for image {} did not get properly inited",
                image.0,
            );
        }
        self.state.with_image = Some(image);
        self.state.with_vars.clear();
        self.state.with_vars.reserve(set.len());
        for &(var, expr_id) in set {
            let value = match expr_id.unpack() {
                UnpackedExprId::Int(index) => {
                    let expression = &self.iscript.int_expressions[index];
                    let mut eval_ctx = self.eval_ctx();
                    eval_ctx.eval_int(&expression)
                }
                UnpackedExprId::Unit(unit_ref) => {
                    let unit = self.resolve_unit_ref(unit_ref);
                    self.unit_to_variable(unit)
                }
            };
            self.state.with_vars.push(SpriteLocal {
                id: var,
                value,
            });
        }
    }

    fn eval_any_type(&mut self, expr: &AnyTypeRef<'_>) -> Option<i32> {
        match *expr {
            AnyTypeRef::Variable(place_id) => {
                match place_id.place() {
                    Place::Global(x) => Some(self.get_global(x)),
                    Place::SpriteLocal(unit_ref, x) => self.get_sprite_local(x, unit_ref),
                    _ => None,
                }
            }
            AnyTypeRef::Default(a, b) => {
                let a = parse::read_any_type(a)?.0;
                self.eval_any_type(&a)
                    .or_else(|| {
                        let b = parse::read_any_type(b)?.0;
                        self.eval_any_type(&b)
                    })
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
            CURRENT_SCRIPT_POS.store(opcode_pos.wrapping_add(1), Ordering::Relaxed);
            self.opcode_pos = opcode_pos;
            let opcode = self.read_u8()?;
            match opcode {
                JUMP_TO_BW => {
                    let dest = self.read_u16()?;
                    (*self.bw_script).pos = dest;
                    return Ok(ScriptRunResult::Done);
                }
                ANIMATION_START => {
                    // Avoid allocating imglocals if calls weren't used
                    if self.dry_run {
                        self.state.dry_run_call_stack.clear();
                    } else {
                        let needs_call_stack_reset = self.state
                            .get_image_local(self.image, CALL_STACK_POS_ID)
                            .filter(|&x| x != 0)
                            .is_some();
                        if needs_call_stack_reset {
                            self.state.set_image_local(self.image, CALL_STACK_POS_ID, 0);
                        }
                    }
                    let header_index = ((*self.bw_script).header - 0x1c) / 8;
                    let animation = (*self.bw_script).animation;
                    if animation == 0 {
                        // Check for init vars
                        if self.state.with_image == Some(ImageId((*self.image).image_id)) {
                            let sprite = Sprite::from_ptr((*self.image).parent)
                                .expect("Image had no parent??");
                            let vars = mem::replace(&mut self.state.with_vars, Vec::new());
                            self.state.sprite_locals.insert(SerializableSprite(*sprite), vars);
                            self.state.with_image = None;
                        }
                    }
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
                IF | IF_CALL => {
                    let cond = self.read_u32()?;
                    let dest = CodePosition(self.read_u32()?);
                    let condition = &self.iscript.conditions[cond as usize];
                    let mut eval_ctx = self.eval_ctx();
                    if eval_ctx.eval_bool(&condition) {
                        if opcode == IF {
                            self.jump_to(dest);
                        } else {
                            self.do_call(dest);
                        }
                    }
                }
                CALL => {
                    let dest = CodePosition(self.read_u32()?);
                    self.do_call(dest);
                }
                RETURN => {
                    if self.dry_run {
                        if let Some(ret) = self.state.dry_run_call_stack.pop() {
                            self.jump_to(CodePosition::aice(ret));
                        } else {
                            (*self.bw_script).pos = 0x5;
                            self.in_aice_code = false;
                        }
                    } else {
                        let call_stack_pos =
                            self.state.get_image_local(self.image, CALL_STACK_POS_ID)
                                .unwrap_or(0);
                        if call_stack_pos == 0 {
                            bw_print!(
                                "Error {}: tried to return without using call",
                                self.current_line(),
                            );
                            // Global infloop
                            (*self.bw_script).pos = 0x5;
                            self.in_aice_code = false;
                        } else {
                            let return_pos = self.state.get_image_local(
                                self.image,
                                CALL_STACK_RETURN_POS_BASE + call_stack_pos - 1,
                            );
                            self.state.set_image_local(
                                self.image,
                                CALL_STACK_POS_ID,
                                call_stack_pos - 1,
                            );
                            match return_pos {
                                Some(return_pos) => {
                                    self.jump_to(CodePosition::aice(return_pos));
                                }
                                None => {
                                    warn!("Broken return");
                                    (*self.bw_script).pos = 0x5;
                                    self.in_aice_code = false;
                                }
                            }
                        }
                    }
                }
                SET_COPY => {
                    let place_id = PlaceId(self.read_u32()?);
                    let place = place_id.place();
                    let expr = self.read_any_type()?;
                    if self.dry_run {
                        continue;
                    }
                    let value = self.eval_any_type(&expr);
                    let value = match value {
                        Some(s) => s,
                        None => {
                            if self.iscript.is_int_var_place(place_id) {
                                self.show_uninit_spritelocal_err();
                                continue 'op_loop;
                            }
                            0
                        }
                    };
                    match place {
                        Place::Global(id) => self.state.globals[id as usize] = value,
                        Place::SpriteLocal(unit_ref, id) => {
                            let uninit = place_id.if_uninit();
                            let sprite = if unit_ref.is_this() {
                                Sprite::from_ptr((*self.image).parent)
                            } else {
                                self.resolve_unit_ref(unit_ref).and_then(|x| x.sprite())
                            };
                            if let Some(sprite) = sprite {
                                self.state.set_sprite_local(*sprite, id, value, uninit);
                            }
                        }
                        _ => {
                            warn!("Invalid SET_COPY dest");
                            continue 'op_loop;
                        }
                    }
                }
                SET => {
                    let place_id = PlaceId(self.read_u32()?);
                    let expr = self.read_u32()?;
                    let place = place_id.place();
                    let mut var_exprs = [0u32; 4];
                    let var_count = place.var_count() as usize;
                    for i in 0..var_count {
                        var_exprs[i] = self.read_u32()?;
                    }
                    if self.dry_run {
                        // Bad? Ideally would have dry-run state
                        // Maybe should stop here?
                        continue;
                    }

                    let value = {
                        let expression = &self.iscript.int_expressions[expr as usize];
                        let mut eval_ctx = self.eval_ctx();
                        eval_ctx.eval_int(&expression)
                    };
                    let mut vars = [0i32; 4];
                    for i in 0..var_count {
                        let expr = var_exprs[i];
                        let expression = &self.iscript.int_expressions[expr as usize];
                        let mut eval_ctx = self.eval_ctx();
                        vars[i] = eval_ctx.eval_int(&expression);
                        if eval_ctx.custom.evaluate_default {
                            continue 'op_loop;
                        }
                    }
                    let res = self.set_variable(place, place_id, value, &vars);
                    if let Some(res) = res {
                        return Ok(res);
                    }
                }
                SET_UNIT => {
                    let place_id = PlaceId(self.read_u32()?);
                    let unit_ref = UnitRefId(self.read_u16()?);
                    if self.dry_run {
                        // Bad? Ideally would have dry-run state
                        // Maybe should stop here?
                        continue;
                    }

                    let value = {
                        let unit = self.resolve_unit_ref(unit_ref);
                        self.unit_to_variable(unit)
                    };
                    let vars = [0i32; 4];
                    let res = self.set_variable(place_id.place(), place_id, value, &vars);
                    if let Some(res) = res {
                        return Ok(res);
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
                    self.state.image_locals.remove(&SerializableImage(self.image));
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
                                // Global infloop
                                (*self.bw_script).pos = 0x5;
                                self.in_aice_code = false;
                                continue;
                            }
                            // Don't keep dangling bullets in sprite_owner_map
                            // to be compatible with bullet limit extenders that free bullet
                            // memory on deletion.
                            self.sprite_owner_map.remove_bullet(*sprite);
                        } else if let Some(unit) = self.unit {
                            if unit.order() != bw_dat::order::DIE {
                                bw_print!(
                                    "ERROR {}: Unit 0x{:x} ended its iscript \
                                    while the unit is still alive",
                                    self.current_line(), unit.id().0,
                                );
                                // Global infloop
                                (*self.bw_script).pos = 0x5;
                                self.in_aice_code = false;
                                continue;
                            }
                        }
                    }
                    (*self.bw_script).pos = 0x4;
                    self.in_aice_code = false;
                }
                RESET_ORDER_WEAPON | FIRE_WEAPON => {
                    let target;
                    let weapon_id;
                    let sprite_locals;
                    if opcode == FIRE_WEAPON {
                        let values = self.read_aice_params(&FIRE_WEAPON_AT_PARAMS)?;
                        target = Some(values[0] as u16)
                            .filter(|&x| x != 0xffff)
                            .map(|x| UnitRefId(x));
                        weapon_id = values[1];
                        sprite_locals = SpriteLocalSetId(values[2] as u32);
                    } else {
                        target = None;
                        weapon_id = 0;
                        sprite_locals = SpriteLocalSetId(0);
                    }
                    if self.dry_run {
                        continue;
                    }
                    self.init_sprite_owner();
                    let unit = match self.get_unit() {
                        Some(s) => s,
                        None => continue 'op_loop,
                    };
                    let sprite = (*self.image).parent;
                    let orders_dat_weapon = &bw::orders_dat()[0xd];
                    let order = unit.order().0 as usize;
                    let new_value;
                    if opcode == FIRE_WEAPON {
                        if unit.order() == bw_dat::order::DIE && unit.order_state() == 1 {
                            // First frame of death order is still fine. Detect that by
                            // checking for unit being currently in step_order.
                            // Dying units don't run their orders anymore.
                            let current_order_unit = CURRENT_ORDER_UNIT.load(Ordering::Relaxed);
                            if current_order_unit as *mut bw::Unit != *unit {
                                bw_print!(
                                    "ERROR {}: Unit 0x{:x} used fireweapon while dying",
                                    self.current_line(), unit.id().0,
                                );
                                // Global infloop. Wouldn't be fully necessary but seems
                                // that iscript is doing something incorrectly so better stop it.
                                (*self.bw_script).pos = 0x5;
                                self.in_aice_code = false;
                                continue 'op_loop;
                            }
                        }
                        let target = match target {
                            Some(s) => match self.resolve_unit_ref(s).filter(|x| !x.is_hidden()) {
                                Some(s) => Some((s, s.position())),
                                None => {
                                    // Skip past castspell and RESET_ORDER_WEAPON
                                    // This is super hacky but parse code verifies that
                                    // this should work when emitting FIRE_WEAPON
                                    // 3 bytes for JUMP_TO_BW and 1 byte for RESET_ORDER_WEAPON
                                    self.pos += 4;
                                    continue 'op_loop;
                                }
                            }
                            None => None,
                        };
                        let old = match orders_dat_weapon.entry_size {
                            1 => *(orders_dat_weapon.data as *mut u8).add(order) as u32,
                            2 => *(orders_dat_weapon.data as *mut u16).add(order) as u32,
                            4 => *(orders_dat_weapon.data as *mut u32).add(order),
                            _ => 0,
                        };
                        if let Some(target) = target {
                            let current_target = unit.target();
                            let old_target = current_target
                                .map(|x| self.unit_array().to_index(x) as i32)
                                .unwrap_or(-1);
                            let old_pos = unit.target_pos();
                            let old_pos = (old_pos.x as u32 | ((old_pos.y as u32) << 16)) as i32;

                            self.state.set_sprite_local(
                                sprite,
                                FIRE_WEAPON_AT_TEMP_ID,
                                old_target,
                                false,
                            );
                            self.state.set_sprite_local(
                                sprite,
                                FIRE_WEAPON_AT_TEMP_ID2,
                                old_pos,
                                false,
                            );
                            (**unit).order_target.unit = *target.0;
                            (**unit).order_target.pos = target.1;
                        } else {
                            // Set this anyway; otherwise would have to delete spritelocal at
                            // RESET_ORDER_WEAPON to distinguish not setting anything and
                            // setting to null
                            self.state.set_sprite_local(
                                sprite,
                                FIRE_WEAPON_AT_TEMP_ID,
                                -2,
                                false,
                            );
                        }
                        self.state.set_sprite_local(sprite, TEMP_LOCAL_ID, old as i32, false);
                        new_value = Some(weapon_id);
                        let image = WeaponId(weapon_id as u16).flingy().sprite().image();
                        self.set_with_vars(image, sprite_locals);
                    } else {
                        new_value = self.get_sprite_local(TEMP_LOCAL_ID, UnitRefId::this());
                        let restore_target =
                            self.get_sprite_local(FIRE_WEAPON_AT_TEMP_ID, UnitRefId::this())
                                .filter(|&x| x != -2);
                        if let Some(restore) = restore_target {
                            let restore_pos =
                                self.get_sprite_local(FIRE_WEAPON_AT_TEMP_ID2, UnitRefId::this());
                            if restore == -1 {
                                (**unit).order_target.unit = null_mut();
                            } else {
                                (**unit).order_target.unit =
                                    self.unit_array().get_by_index(restore as u32)
                                        .map(|x| *x)
                                        .unwrap_or(null_mut());
                            }
                            if let Some(pos) = restore_pos {
                                let pos = pos as u32;
                                (**unit).order_target.pos = bw::Point {
                                    x: pos as u16 as i16,
                                    y: (pos >> 16) as u16 as i16,
                                };
                            }
                        }
                        self.state.with_image = None;
                    }
                    if let Some(new_value) = new_value {
                        let data = orders_dat_weapon.data;
                        match orders_dat_weapon.entry_size {
                            1 => {
                                *(data as *mut u8).add(order) = new_value as u8;
                            }
                            2 => {
                                *(data as *mut u16).add(order) = new_value as u16;
                            }
                            4 => {
                                *(data as *mut u32).add(order) = new_value as u32;
                            }
                            _ => (),
                        }
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
                    (**unit).flingy.flingy_flags &= !0x8;
                    (**unit).order_wait = 0;
                }
                CREATE_UNIT => {
                    let values = self.read_aice_params(&CREATE_UNIT_PARAMS)?;
                    if self.dry_run {
                        continue;
                    }
                    let unit_id = match u32::try_from(values[0]).ok().and_then(UnitId::optional) {
                        Some(s) => s,
                        None => continue,
                    };
                    let pos = bw::Point {
                        x: values[1] as i16,
                        y: values[2] as i16,
                    };
                    let player = match values[3] {
                        0..=11 => values[3] as u8,
                        _ => continue,
                    };
                    let sprite_locals = SpriteLocalSetId(values[4] as u32);
                    let image = unit_id.flingy().sprite().image();
                    self.set_with_vars(image, sprite_locals);
                    return Ok(ScriptRunResult::CreateUnit(unit_id, pos, player));
                }
                CREATE_SPRITE => {
                    let values = self.read_aice_params(&CREATE_SPRITE_PARAMS)?;
                    if self.dry_run {
                        continue;
                    }
                    let sprite_id = match u32::try_from(values[0]).ok()
                        .and_then(SpriteId::optional)
                    {
                        Some(s) => s,
                        None => continue,
                    };
                    let pos = bw::Point {
                        x: values[1] as i16,
                        y: values[2] as i16,
                    };
                    let player = match values[3] {
                        0..=11 => values[3] as u8,
                        _ => continue,
                    };
                    let elevation = values[4] as u8;
                    let sprite_locals = SpriteLocalSetId(values[5] as u32);
                    let image = sprite_id.image();
                    self.set_with_vars(image, sprite_locals);
                    return Ok(ScriptRunResult::CreateSprite(sprite_id, pos, player, elevation));
                }
                ISSUE_ORDER => {
                    let values = self.read_aice_params(&ISSUE_ORDER_PARAMS)?;
                    if self.dry_run {
                        continue;
                    }
                    let order_id = OrderId(values[0] as u8);
                    let pos = bw::Point {
                        x: values[1] as i16,
                        y: values[2] as i16,
                    };
                    let unit = match self.unit {
                        Some(s) => s,
                        None => continue 'op_loop,
                    };
                    return Ok(ScriptRunResult::IssueOrder(unit, order_id, pos));
                }
                PLAY_FRAME => {
                    let values = self.read_aice_params(&PLAY_FRAME_PARAMS)?;
                    if self.dry_run {
                        continue;
                    }
                    let value = values[0] as u16;
                    let image = self.image;
                    if let Err(limit) = image_play_frame(image, value) {
                        bw_print_error!(
                            "ERROR {}: Image 0x{:x} has only {} frames, tried to display \
                            frame {}",
                            self.current_line(), (*image).image_id, limit, value,
                        );
                    }
                }
                WAIT => {
                    let time = self.read_u8()?;
                    if time == 1 {
                        // Samase iscript extension api is bad in that it doesn't allow wait
                        // 1 that writes 0 to wait value. So jump to 0xa that has bw wait 1 there.
                        (*self.bw_script).pos = 0xa;
                    } else {
                        // Jump to 0xc directly
                        (*self.bw_script).wait = time.wrapping_sub(1);
                        (*self.bw_script).pos = 0xc;
                    }
                    self.in_aice_code = false;
                    let continue_pos = self.pos as u32;
                    self.set_continue_pos(continue_pos);
                    return Ok(ScriptRunResult::Done);
                }
                CONTINUE => {
                    if let Some(pos) = self.get_continue_pos() {
                        self.pos = pos as usize;
                    } else {
                        let image = self.image;
                        bw_print_error!(
                            "ERROR invalid continue pos for image 0x{:x} {:p}",
                            (*image).image_id, image,
                        );
                    }
                }
                SIGORDER | ORDER_DONE => {
                    let flags = self.read_u8()?;
                    if self.dry_run {
                        continue;
                    }
                    let flingy = match self.get_flingy() {
                        Some(s) => s as *mut bw::Unit,
                        None => {
                            // Global infloop
                            (*self.bw_script).pos = 0x5;
                            self.in_aice_code = false;
                            continue;
                        }
                    };
                    if opcode == SIGORDER {
                        (*flingy).order_signal |= flags;
                    } else {
                        (*flingy).order_signal &= !flags;
                    }
                }
                IMG_ON => {
                    let values = self.read_aice_params(&IMGUL_ON_PARAMS)?;
                    if self.dry_run {
                        continue 'op_loop;
                    }
                    let above = values[0] != 0;
                    let unit_ref = UnitRefId(values[1] as u16);
                    let image_id = ImageId(values[2] as u16);
                    let x = values[3] as i8;
                    let y = values[4] as i8;

                    let sprite = match self.resolve_unit_ref(unit_ref).and_then(|x| x.sprite()) {
                        Some(s) => s,
                        None => continue 'op_loop,
                    };
                    let base_image = match above {
                        true => sprite.images().next(),
                        // Ugh but lazy and pretty minor in the end
                        false => sprite.images().last(),
                    };

                    if let Some(base_image) = base_image {
                        return Ok(ScriptRunResult::AddOverlay(base_image, image_id, x, y, above));
                    }
                }
                GIVE_UNIT | TRANSFORM_UNIT | HIDE_SHOW_UNIT=> {
                    let unit_ref;
                    let values;
                    if opcode == GIVE_UNIT {
                        values = self.read_aice_params(&GIVE_UNIT_PARAMS)?;
                        unit_ref = UnitRefId(values[0] as u16);
                    } else if opcode == TRANSFORM_UNIT {
                        values = self.read_aice_params(&TRANSFORM_UNIT_PARAMS)?;
                        unit_ref = UnitRefId(values[0] as u16);
                    } else {
                        values = self.read_aice_params(&HIDE_UNIT_PARAMS)?;
                        unit_ref = UnitRefId(values[1] as u16);
                    };
                    if self.dry_run {
                        continue;
                    }
                    let unit = match self.resolve_unit_ref(unit_ref) {
                        Some(s) => s,
                        None => continue 'op_loop,
                    };
                    if opcode == GIVE_UNIT {
                        let player = values[1] as u8;
                        if player >= 12 {
                            continue 'op_loop;
                        }
                        return Ok(ScriptRunResult::GiveUnit(unit, player));
                    } else if opcode == TRANSFORM_UNIT {
                        return Ok(ScriptRunResult::TransformUnit(unit, UnitId(values[1] as u16)));
                    } else {
                        let hide = values[0] == 0;
                        if hide {
                            return Ok(ScriptRunResult::HideUnit(unit));
                        } else {
                            return Ok(ScriptRunResult::ShowUnit(unit));
                        }
                    }
                }
                PRINT => {
                    let text_id = FormatStringId(self.read_u32()?);
                    if self.dry_run {
                        continue;
                    }
                    let mut buf = mem::replace(&mut self.state.format_buffer, Vec::new());
                    buf.clear();
                    buf.reserve(128);
                    let iscript = self.iscript;
                    iscript.format_strings.format(text_id, &mut buf, |buf, expr| {
                        match *expr {
                            AnyExpr::Int(expr) => {
                                let mut eval_ctx = self.eval_ctx();
                                let expr = &iscript.int_expressions[expr as usize];
                                let val = eval_ctx.eval_int(expr);
                                if eval_ctx.custom.evaluate_default {
                                    buf.extend_from_slice(b"NONE");
                                } else {
                                    let _ = write!(buf, "{}", val);
                                }
                            }
                            AnyExpr::Bool(expr) => {
                                let mut eval_ctx = self.eval_ctx();
                                let expr = &iscript.conditions[expr as usize];
                                let val = eval_ctx.eval_bool(expr);
                                let msg = if eval_ctx.custom.evaluate_default {
                                    &b"NONE"[..]
                                } else {
                                    [&b"false"[..], b"true"][val as usize]
                                };
                                buf.extend_from_slice(msg);
                            }
                            AnyExpr::UnitRef(id) => {
                                if let Some(unit) = self.resolve_unit_ref(id) {
                                    let pos = unit.position();
                                    let unit_uid = self.unit_array().to_unique_id(unit);
                                    let _ = write!(
                                        buf,
                                        "uid 0x{:x}, id {} at {},{}, owned by player {}",
                                        unit_uid, unit.id().0, pos.x, pos.y, unit.player(),
                                    );
                                } else {
                                    buf.extend_from_slice(b"NONE");
                                }
                            }
                        }
                    });
                    buf.push(0);
                    crate::samase::print_text(buf.as_ptr());
                    self.state.format_buffer = buf;
                }
                x => {
                    error!("Unknown opcode {:02x}", x);
                    return Err(opcode_pos);
                }
            }
        }
        Ok(ScriptRunResult::Done)
    }

    #[must_use]
    unsafe fn set_variable(
        &mut self,
        place: Place,
        place_id: PlaceId,
        value: i32,
        vars: &[i32; 4],
    ) -> Option<ScriptRunResult> {
        match place {
            Place::Global(id) => self.state.globals[id as usize] = value,
            Place::SpriteLocal(unit_ref, id) => {
                let uninit = place_id.if_uninit();
                let (id, sprite) = if id == 0xffff {
                    let (unit, id) = self.resolve_unit_ref_as_place(unit_ref)?;
                    (id, unit.sprite())
                } else {
                    let sprite = if unit_ref.is_this() {
                        Sprite::from_ptr((*self.image).parent)
                    } else {
                        self.resolve_unit_ref(unit_ref)?.sprite()
                    };
                    (id, sprite)
                };
                if let Some(sprite) = sprite {
                    self.state.set_sprite_local(*sprite, id, value, uninit);
                }
            }
            Place::Flingy(unit_ref, ty) => {
                let flingy = if unit_ref.is_this() {
                    match self.get_flingy() {
                        Some(s) => s,
                        None => return None,
                    }
                } else {
                    match self.resolve_unit_ref(unit_ref) {
                        Some(s) => ptr::addr_of_mut!((**s).flingy),
                        None => return None,
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
                        return None;
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
                        (*bullet).target.pos.x = value as i16;
                    }
                    BulletVar::OrderTargetY => {
                        (*bullet).target.pos.y = value as i16;
                    }
                    BulletVar::Flags => (*bullet).flags = value as u8,
                }
            }
            Place::Unit(unit_ref, ty) => {
                let unit = match self.resolve_unit_ref(unit_ref) {
                    Some(s) => s,
                    None => {
                        if unit_ref.is_this() {
                            self.report_missing_parent("unit");
                            show_unit_frame0_help();
                        }
                        return None;
                    }
                };
                let val_u16 = clamp_i32_u16(value);
                let val_u8 = clamp_i32_u8(value);
                match ty {
                    UnitVar::DeathTimer => (**unit).death_timer = val_u16,
                    UnitVar::MatrixTimer => (**unit).matrix_timer = val_u8,
                    UnitVar::MatrixHp => (**unit).defensive_matrix_dmg = val_u16,
                    UnitVar::StimTimer => (**unit).stim_timer = val_u8,
                    UnitVar::EnsnareTimer => (**unit).ensnare_timer = val_u8,
                    UnitVar::LockdownTimer => (**unit).lockdown_timer = val_u8,
                    UnitVar::IrradiateTimer => (**unit).irradiate_timer = val_u8,
                    UnitVar::StasisTimer => (**unit).stasis_timer = val_u8,
                    UnitVar::PlagueTimer => (**unit).plague_timer = val_u8,
                    UnitVar::MaelstormTimer => (**unit).maelstrom_timer = val_u8,
                    UnitVar::IsBlind => (**unit).is_blind = val_u8,
                    UnitVar::SpellUpdateTimer => (**unit).master_spell_timer = val_u8,
                    UnitVar::ParasitePlayers => (**unit).parasited_by_players = val_u8,
                    UnitVar::IsBeingHealed => (**unit).is_being_healed = val_u8,
                    UnitVar::IsUnderStorm => (**unit).is_under_storm = val_u8,
                    UnitVar::Hitpoints => {
                        if value != unit.hitpoints() {
                            return Some(ScriptRunResult::SetHp(unit, value));
                        }
                    }
                    UnitVar::Shields => (**unit).shields = value,
                    UnitVar::Energy => (**unit).energy = val_u16,
                    UnitVar::MaxHitpoints => bw_print!("Cannot set max hitpoints"),
                    UnitVar::MaxShields => bw_print!("Cannot set max shields"),
                    UnitVar::MaxEnergy => bw_print!("Cannot set max energy"),
                    UnitVar::MineralCost => bw_print!("Cannot set mineral cost"),
                    UnitVar::GasCost => bw_print!("Cannot set gas cost"),
                    UnitVar::SupplyCost => bw_print!("Cannot set supply cost"),
                    UnitVar::OverlaySize => bw_print!("Cannot set overlay size"),
                    UnitVar::Resources => if unit.id().is_resource_container() {
                        (**unit).unit_specific2.resource.amount = val_u16;
                    },
                    UnitVar::HangarCountInside | UnitVar::HangarCountOutside =>
                        bw_print!("Cannot set hangar count"),
                    UnitVar::LoadedCount => bw_print!("Cannot set loaded count"),
                    UnitVar::CurrentUpgrade =>
                        bw_print!("Cannot set current upgrade"),
                    UnitVar::CurrentTech => bw_print!("Cannot set current tech"),
                    UnitVar::BuildQueue => {
                        if vars[0] < 5 {
                            let index = ((**unit).current_build_slot as usize)
                                .wrapping_add(vars[0] as usize) % 5;
                            (**unit).build_queue[index] = val_u16;
                        }
                    }
                    UnitVar::RemainingBuildTime => {
                        (**unit).remaining_build_time = val_u16;
                    }
                    UnitVar::RemainingResearchTime => {
                        if unit.tech_in_progress().is_some() ||
                            unit.upgrade_in_progress().is_some()
                        {
                            (**unit).unit_specific.building.research_time_remaining =
                                val_u16;
                        }
                    }
                    UnitVar::UnitId => bw_print!("Cannot set unit id"),
                    UnitVar::Kills => (**unit).kills = val_u8,
                    UnitVar::CarriedResourceAmount => {
                        if unit.id().is_worker() {
                            (**unit).unit_specific.worker.carried_resource_count =
                                val_u8;
                        }
                    }
                    UnitVar::GroundCooldown => (**unit).ground_cooldown = val_u8,
                    UnitVar::AirCooldown => (**unit).air_cooldown = val_u8,
                    UnitVar::SpellCooldown => (**unit).spell_cooldown = val_u8,
                    UnitVar::Order => bw_print!("Cannot set order"),
                    UnitVar::OrderTimer => (**unit).order_timer = val_u8,
                    UnitVar::OrderState => (**unit).order_state = val_u8,
                    UnitVar::RankIncrease => (**unit).rank =
                        clamp_i32_iu8(value) as u8,
                    UnitVar::MineAmount => {
                        if matches!(
                            unit.id(),
                            bw_dat::unit::VULTURE |
                            bw_dat::unit::JIM_RAYNOR_VULTURE
                        ) {
                            (**unit).unit_specific.vulture.mines = val_u8;
                        }
                    }
                    UnitVar::RallyX | UnitVar::RallyY => {
                        if unit.id().is_building() && unit.id() != bw_dat::unit::PYLON {
                            if ty == UnitVar::RallyX {
                                (**unit).rally_pylon.rally.pos.x = val_u16 as i16;
                            } else {
                                (**unit).rally_pylon.rally.pos.y = val_u16 as i16;
                            }
                        }
                    }
                    UnitVar::Flags => (**unit).flags = value as u32,
                    UnitVar::DetectionStatus => {
                        (**unit).detection_status = value as u32;
                    }
                    UnitVar::PathingFlags => (**unit).pathing_flags = val_u8,
                    UnitVar::MovementState => (**unit).movement_state = val_u8,
                    UnitVar::RepulseX => (**unit).repulse_chunk_x = val_u8,
                    UnitVar::RepulseY => (**unit).repulse_chunk_y = val_u8,
                    UnitVar::RepulseMisc => (**unit).repulse_misc = val_u8,
                    UnitVar::RepulseDirection => {
                        (**unit).repulse_direction = degrees_to_bw_angle(value);
                    }
                    UnitVar::CurrentButtonSet => (**unit).buttons = val_u16,
                    UnitVar::AirStrength => (**unit).air_strength = val_u16,
                    UnitVar::GroundStrength => (**unit).ground_strength = val_u16,
                    UnitVar::OrderTargetX => (**unit).order_target.pos.x = val_u16 as i16,
                    UnitVar::OrderTargetY => (**unit).order_target.pos.y = val_u16 as i16,
                    UnitVar::CloakCount => (**unit).invisibility_effects = val_u8,
                    UnitVar::SecondaryOrder => (**unit).secondary_order = val_u8,
                    UnitVar::SecondaryOrderState => (**unit).secondary_order_state = val_u8,
                    UnitVar::LastAttackingPlayer => (**unit).last_attacking_player = val_u8,
                }
            },
            Place::Image(ty) => {
                let image = self.image;
                match ty {
                    ImageVar::Drawfunc => (*image).drawfunc = value as u8,
                    ImageVar::DrawfuncParam => {
                        (*image).drawfunc_param = value as usize as *mut c_void;
                    }
                    ImageVar::Frame => bw_print!("Cannot set frame"),
                    ImageVar::BaseFrame => bw_print!("Cannot set base frame"),
                }
            }
            Place::Game(ty) => {
                self.set_game_var(ty, value, &vars);
            }
            Place::UnitExt(unit_ref, field_id) => {
                let unit = match self.resolve_unit_ref(unit_ref) {
                    Some(s) => s,
                    None => {
                        if unit_ref.is_this() {
                            self.report_missing_parent("unit");
                            show_unit_frame0_help();
                        }
                        return None;
                    }
                };
                let unit_index = self.unit_array().to_index(unit) as u32;
                let field_id = self.samase_unit_ext_field_id(field_id);
                samase::write_extended_unit_field(unit_index, field_id, value as u32);
            }
        }
        None
    }

    fn get_bullet_silent(&mut self) -> Option<*mut bw::Bullet> {
        self.init_sprite_owner();
        self.bullet
    }

    fn get_unit_silent(&mut self) -> Option<Unit> {
        self.init_sprite_owner();
        self.unit
    }

    fn get_unit(&mut self) -> Option<Unit> {
        match self.get_unit_silent() {
            Some(s) => Some(s),
            None => {
                self.report_missing_parent("unit");
                show_unit_frame0_help();
                None
            }
        }
    }

    fn get_flingy(&mut self) -> Option<*mut bw::Flingy> {
        self.init_sprite_owner();
        let flingy = unsafe {
            self.unit.map(|x| ptr::addr_of_mut!((**x).flingy))
                .or_else(|| self.bullet.map(|x| ptr::addr_of_mut!((*x).flingy)))
        };
        match flingy {
            Some(s) => Some(s),
            None => {
                self.report_missing_parent("flingy");
                show_unit_frame0_help();
                show_bullet_frame0_help();
                None
            }
        }
    }

    fn do_call(&mut self, dest: CodePosition) {
        let return_pos = self.pos;
        if self.dry_run {
            self.state.dry_run_call_stack.push(return_pos as u32);
            self.jump_to(dest);
        } else {
            let call_stack_pos = self.state.get_image_local(self.image, CALL_STACK_POS_ID)
                .unwrap_or(0);
            if call_stack_pos >= CALL_STACK_LIMIT {
                bw_print!(
                    "Error {}: call stack depth limit ({}) reached",
                    self.current_line(), CALL_STACK_LIMIT,
                );
            } else {
                self.state.set_image_local(
                    self.image,
                    CALL_STACK_POS_ID,
                    call_stack_pos + 1,
                );
                self.state.set_image_local(
                    self.image,
                    CALL_STACK_RETURN_POS_BASE + call_stack_pos,
                    return_pos as u32,
                );
                self.jump_to(dest);
            }
        }
    }

    fn set_game_var(&mut self, ty: GameVar, value: i32, vars: &[i32; 4]) -> Option<()> {
        use crate::parse::GameVar::*;
        let game = self.game;
        // Moving this outside match since it is pretty common
        let player = try_clamp_player(vars[0]);
        match ty {
            Deaths => game.set_unit_deaths(player?, UnitId(vars[1] as u16), value as u32),
            Kills => game.set_unit_kills(player?, UnitId(vars[1] as u16), value as u32),
            UpgradeLevel => {
                game.set_upgrade_level(player?, UpgradeId(vars[1] as u16), value.min(255) as u8);
            }
            UpgradeLimit => {
                let upgrade = UpgradeId(vars[1] as u16);
                game.set_upgrade_max_level(player?, upgrade, value.min(255) as u8);
            }
            TechLevel => {
                game.set_tech_level(player?, TechId(vars[1] as u16), (value > 0) as u8);
            }
            TechAvailability => {
                game.set_tech_availability(player?, TechId(vars[1] as u16), (value > 0) as u8);
            }
            UnitAvailability => {
                game.set_unit_availability(player?, UnitId(vars[1] as u16), value > 0);
            }
            Alliance => game.set_alliance(player?, try_clamp_player(vars[1])?, value > 0),
            SharedVision => game.set_shared_vision(player?, try_clamp_player(vars[1])?, value > 0),
            Minerals => game.set_minerals(player?, value as u32),
            Gas => game.set_gas(player?, value as u32),
            ZergSupplyMax | TerranSupplyMax | ProtossSupplyMax |
                ZergSupplyUsed | TerranSupplyUsed | ProtossSupplyUsed |
                ZergSupplyProvided | TerranSupplyProvided |
                ProtossSupplyProvided =>
            {
                let race = match ty {
                    ZergSupplyMax | ZergSupplyUsed | ZergSupplyProvided => {
                        Race::Zerg
                    }
                    TerranSupplyMax | TerranSupplyUsed | TerranSupplyProvided => {
                        Race::Terran
                    }
                    _ => Race::Protoss,
                };
                match ty {
                    ZergSupplyMax | TerranSupplyMax | ProtossSupplyMax => {
                        game.set_supply_max(player?, race, value as u32);
                    }
                    ZergSupplyUsed | TerranSupplyUsed | ProtossSupplyUsed => {
                        game.set_supply_used(player?, race, value as u32);
                    }
                    _ => {
                        game.set_supply_provided(player?, race, value as u32);
                    }
                }
            }
            UnitsTotal | UnitsProduced | UnitsOwned | UnitsLost | UnitsKilled |
                UnitsScore | UnitsKilledScore | BuildingsTotal |
                BuildingsConstructed | BuildingsOwned | BuildingsLost |
                BuildingsRazed | BuildingsScore | BuildingsRazedScore | FactoriesConstructed |
                FactoriesOwned | FactoriesLost | FactoriesRazed =>
            {
                let index = ty as u8 - UnitsTotal as u8;
                game.set_score(index, player?, value.max(0) as u32);
            }
            CustomScore => game.set_custom_score(player?, value as u32),
            LocationLeft | LocationTop | LocationRight | LocationBottom => unsafe {
                let location = (vars[0]).clamp(0, 254) as u8;
                let location = (**game).locations.as_mut_ptr().add(location as usize);
                match ty {
                    LocationLeft => {
                        (*location).area.left = value.max(0)
                            .min((game.map_width_tiles() as i32) << 5);
                    }
                    LocationTop => {
                        (*location).area.top = value.max(0)
                            .min((game.map_height_tiles() as i32) << 5);
                    }
                    LocationRight => {
                        (*location).area.right = value.max(0)
                            .min((game.map_width_tiles() as i32) << 5);
                    }
                    LocationBottom | _ => {
                        (*location).area.bottom = value.max(0)
                            .min((game.map_height_tiles() as i32) << 5);
                    }
                }
            },
            PlayerColorChoice => bw_print!("Cannot set player_color_choice"),
            PlayerType | PlayerRace => unsafe {
                if ty == PlayerType && (value == 1 || value == 2) {
                    bw_print!("Cannot set player type to 1/2");
                } else {
                    let players = crate::samase::players();
                    let player = players.add(player? as usize);
                    if ty == PlayerType {
                        (*player).player_type = value as u8;
                    } else {
                        (*player).race = value as u8;
                    }
                }
            }
            LeaderboardType => unsafe {
                (**game).leaderboard_type = u8::try_from(value).unwrap_or(0)
            }
            LeaderboardLocation => unsafe {
                (**game).leaderboard_location = u8::try_from(value).unwrap_or(0)
            }
            LeaderboardParameter => unsafe {
                (**game).leaderboard_parameter = clamp_i32_u16(value)
            }
            LeaderboardGoal => unsafe {
                (**game).leaderboard_goal = value as u32
            }
            LeaderboardComputers => unsafe {
                (**game).computers_in_leaderboard = value as u32
            }
            Dat => {
                self.write_dat(vars[0], vars[1], vars[2], value);
            }
        }
        Some(())
    }

    /// Dat must be bw_dat expr enum
    fn read_dat(&self, dat: i32, field: i32, id: i32) -> i32 {
        match self.read_dat_opt(dat, field, id) {
            Some(s) => s,
            None => {
                let msg = format!(
                    "Error {}: Failed to read dat({}, {}, {})",
                    self.current_line(), dat, field, id,
                );
                bw_print_error!("{}", msg);
                0
            }
        }
    }

    /// Dat must be bw_dat expr enum
    fn write_dat(&self, dat: i32, field: i32, id: i32, value: i32) {
        match self.write_dat_opt(dat, field, id, value) {
            Some(s) => s,
            None => {
                let msg = format!(
                    "Error {}: Failed to write dat({}, {}, {})",
                    self.current_line(), dat, field, id,
                );
                bw_print_error!("{}", msg);
            }
        }
    }

    /// Dat must be bw_dat expr enum
    fn read_dat_opt(&self, dat: i32, field: i32, id: i32) -> Option<i32> {
        let field = u32::try_from(field).ok()?;
        let id = u32::try_from(id).ok()?;
        let value = match dat {
            0 => UnitId::optional(id)?.get_opt(field)? as i32,
            1 => WeaponId::optional(id)?.get_opt(field)? as i32,
            2 => FlingyId::optional(id)?.get_opt(field)? as i32,
            3 => SpriteId::optional(id)?.get_opt(field)? as i32,
            4 => ImageId::optional(id)?.get_opt(field)? as i32,
            5 => OrderId::optional(id)?.get_opt(field)? as i32,
            6 => UpgradeId::optional(id)?.get_opt(field)? as i32,
            7 => TechId::optional(id)?.get_opt(field)? as i32,
            _ => return None,
        };
        Some(value)
    }

    /// Dat must be bw_dat expr enum
    fn write_dat_opt(&self, dat: i32, field: i32, id: i32, value: i32) -> Option<()> {
        // bw_dat enum to samase enum
        let dat = match dat {
            0 => 0, // Units
            1 => 1, // Weapons
            2 => 2, // Flingy
            3 => 5, // Sprites
            4 => 6, // Images
            5 => 7, // Orders
            6 => 3, // Upgrades
            7 => 4, // Tech
            9 => 9, // Portdata
            10 => 11, // Buttons
            _ => return None,
        };
        if dat == 9 && field < 2 {
            // First 2 fields in portdata are string pointers
            return None;
        }
        let field = u32::try_from(field).ok()?;
        let id = usize::try_from(id).ok()?;
        unsafe {
            let dat = samase::mutate_dat(dat, field)?.as_ptr();
            if (*dat).entries as usize <= id {
                return None;
            }
            let ptr = (*dat).data;
            debug_assert!(!ptr.is_null());
            match (*dat).entry_size {
                1 => {
                    if value > 0xff || value < -0x80 {
                        return None;
                    }
                    *(ptr as *mut u8).add(id) = value as u8;
                }
                2 => {
                    if value > 0xffff || value < -0x8000 {
                        return None;
                    }
                    *(ptr as *mut u16).add(id) = value as u16;
                }
                4 => {
                    *(ptr as *mut u32).add(id) = value as u32;
                }
                _ => return None,
            };
            Some(())
        }
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
            bw_print_error!("{}", msg);
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

    unsafe fn set_continue_pos(&mut self, pos: u32) {
        let state = &mut self.state;
        if self.bw_script != &raw mut (*self.image).iscript {
            state.dry_run_continue_pos = NonNull::new(self.bw_script)
                .map(|x| (x, self.image, pos));
        } else {
            if let Some((vec, index)) = image_to_index(self.image) {
                if let Some(vec) = state.wait_continue_pos.get_mut(vec) {
                    if vec.len() <= index {
                        vec.resize_with(index + 1, || 0);
                    }
                    vec[index] = pos;
                }
            }
        }
    }

    unsafe fn get_continue_pos(&mut self) -> Option<u32> {
        let state = &mut self.state;
        if self.bw_script != &raw mut (*self.image).iscript {
            if let Some((ptr, image, pos)) = state.dry_run_continue_pos.take() {
                if ptr.as_ptr() == self.bw_script && image == self.image {
                    return Some(pos);
                }
            }
            // The dry run iscript is sometimes initialized to start from movement animation
            // start, in which case the above check passes.
            // However, if the parent image is in movement animation already it starts from the
            // position the parent image was at without, in which case it may be in CONTINUE
            // opcode with the continue pos only stored for parent image.
            //
            // If image's iscript is in movement animation and position was 0xc (CONTINUE),
            // use position from it.
            if (*self.image).iscript.animation == 0xb && (*self.image).iscript.pos == 0xc {
                let (vec, index) = image_to_index(self.image)?;
                // Dry run iscript is only for main images, so vec should be 0
                if vec == 0 {
                    let pos = state.wait_continue_pos.get_mut(vec)?.get_mut(index)?;
                    if *pos != 0 {
                        return Some(*pos);
                    }
                }
            }
            None
        } else {
            let (vec, index) = image_to_index(self.image)?;
            let pos = state.wait_continue_pos.get_mut(vec)?.get_mut(index)?;
            let ret = *pos;
            // Make continue pos 0 afterwards to catch issues with unintended continues.
            *pos = 0;
            if ret == 0 {
                None
            } else {
                Some(ret)
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
                self.pos = self.pos.wrapping_add(1);
                Ok(s)
            }
            None => Err(self.pos as u32),
        }
    }

    fn read_u16(&mut self) -> Result<u16, u32> {
        match self.iscript.aice_data.get(self.pos..).and_then(|x| x.get(..2)) {
            Some(s) => {
                self.pos = self.pos.wrapping_add(2);
                Ok(LittleEndian::read_u16(s))
            }
            None => Err(self.pos as u32),
        }
    }

    fn read_u32(&mut self) -> Result<u32, u32> {
        match self.iscript.aice_data.get(self.pos..).and_then(|x| x.get(..4)) {
            Some(s) => {
                self.pos = self.pos.wrapping_add(4);
                Ok(LittleEndian::read_u32(s))
            }
            None => Err(self.pos as u32),
        }
    }

    fn read_any_type(&mut self) -> Result<AnyTypeRef<'a>, u32> {
        let pos = self.pos;
        let aice_data = &self.iscript.aice_data;
        aice_data.get(pos..)
            .and_then(|data| {
                let (val, rest) = parse::read_any_type(data)?;
                let new_pos = aice_data.len().wrapping_sub(rest.len());
                self.pos = new_pos;
                Some(val)
            })
            .ok_or(pos as u32)
    }

    fn read_aice_params(&mut self, params: &CommandParams) -> Result<[i32; 8], u32> {
        let mut result = [0i32; 8];
        let mut exprs = [0u8; 8];
        let start_pos = self.pos;
        unsafe {
            let base_ptr = self.iscript.aice_data.as_ptr();
            let read_ptr = base_ptr.add(start_pos);
            let (new_ptr, expr_total) =
                parse::read_aice_params(params, read_ptr, &mut result, &mut exprs);
            let new_pos = (new_ptr as usize).wrapping_sub(base_ptr as usize);
            if new_pos > self.iscript.aice_data.len() {
                return Err(start_pos as u32);
            }
            self.pos = new_pos;

            if expr_total != 0 {
                if !self.dry_run {
                    self.init_sprite_owner();
                    for i in 0..params.params.len().min(exprs.len()) {
                        if exprs[i] != 0 {
                            let expr = result[i];
                            let expression = &self.iscript.int_expressions[expr as usize];
                            let mut eval_ctx = self.eval_ctx();
                            let mut value = eval_ctx.eval_int(&expression);
                            let param_type = params.params[i];
                            if param_type == AiceCommandParam::IntExprOrConstU8 {
                                value = clamp_i32_u8(value) as i32;
                            } else if param_type == AiceCommandParam::IntExprOrConstI8 {
                                value = clamp_i32_iu8(value) as i32;
                            } else if param_type == AiceCommandParam::IntExprOrConstU16 {
                                value = clamp_i32_u16(value) as i32;
                            }
                            result[i] = value;
                        }
                    }
                }
            }
        }
        Ok(result)
    }
}

fn try_clamp_player(val: i32) -> Option<u8> {
    if val < 0 || val > 11 {
        None
    } else {
        Some(val as u8)
    }
}

fn clamp_i32_u16(val: i32) -> u16 {
    val.max(0).min(i32::from(u16::MAX)) as u16
}

fn clamp_i32_u8(val: i32) -> u8 {
    val.max(0).min(i32::from(u8::MAX)) as u8
}

fn clamp_i32_iu8(val: i32) -> i8 {
    if val < 0 {
        val.max(i8::MIN.into()) as i8
    } else {
        val.min(u8::MAX.into()) as u8 as i8
    }
}

#[test]
fn test_clamps() {
    assert_eq!(clamp_i32_u16(-1), 0);
    assert_eq!(clamp_i32_u16(-130), 0);
    assert_eq!(clamp_i32_u16(130), 130);
    assert_eq!(clamp_i32_u16(69094), 65535);
    assert_eq!(clamp_i32_iu8(-1), -1);
    assert_eq!(clamp_i32_iu8(-130), -128);
    assert_eq!(clamp_i32_iu8(130), 130u8 as i8);
    assert_eq!(clamp_i32_iu8(126), 126);
    assert_eq!(clamp_i32_iu8(127), 127);
    assert_eq!(clamp_i32_iu8(500), 255u8 as i8);
}

unsafe fn image_play_frame(image: *mut bw::Image, frameset: u16) -> Result<(), u16> {
    if (*image).drawfunc == 0xb {
        // HP bar, does not have a concept of frame and grp pointer isn't same struct.
        return Ok(());
    }
    if (*image).frameset != frameset {
        let frame = frameset.saturating_add((*image).direction as u16);
        let limit = grp_frames((*image).grp);
        if frame >= limit {
            return Err(limit);
        }
        (*image).frameset = frameset;
        (*image).frame = frame;
        (*image).flags |= 0x1;
    }
    Ok(())
}

unsafe fn grp_frames(grp: *mut c_void) -> u16 {
    *(grp as *mut u16)
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

unsafe fn set_flingy_var(flingy: *mut bw::Flingy, ty: FlingyVar, value: i32) {
    match ty {
        FlingyVar::MoveTargetX => {
            (*flingy).move_target.pos.x = value as i16;
            (*flingy).next_move_waypoint.x = value as i16;
            (*flingy).unk_move_waypoint.x = value as i16;
        }
        FlingyVar::MoveTargetY => {
            (*flingy).move_target.pos.y = value as i16;
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
        FlingyVar::TurnSpeed => (*flingy).turn_speed = value as u8,
        FlingyVar::Acceleration => (*flingy).acceleration = clamp_i32_u16(value),
        FlingyVar::TopSpeed => (*flingy).top_speed = value.max(0) as u32,
        FlingyVar::Speed => {
            (*flingy).current_speed = value;
            (*flingy).next_speed = value;
        }
        // Todo
        FlingyVar::PositionX => bw_print!("Cannot set flingy pos"),
        FlingyVar::PositionY => bw_print!("Cannot set flingy pos"),
        FlingyVar::Player => bw_print!("Cannot set player"),
        FlingyVar::FlingyId => (*flingy).flingy_id = clamp_i32_u16(value),
        FlingyVar::Flags => (*flingy).flingy_flags = value as u8,
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

unsafe impl Sync for SpriteOwnerMap {}
unsafe impl Send for SpriteOwnerMap {}

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
                if (*unit).flingy.sprite == sprite {
                    return Unit::from_ptr(unit);
                }
            }
            None
        }
    }

    pub fn get_bullet(&self, sprite: *mut bw::Sprite) -> Option<*mut bw::Bullet> {
        unsafe {
            if let Some(&bullet) = self.bullet_mapping.get(&sprite) {
                if (*bullet).flingy.sprite == sprite {
                    return Some(bullet);
                }
            }
            None
        }
    }

    pub fn remove_bullet(&mut self, sprite: *mut bw::Sprite) {
        self.bullet_mapping.remove(&sprite);
    }
}

pub fn rebuild_sprite_owners() {
    if let Some(map) = SPRITE_OWNER_MAP.get() {
        let mut map = map.lock("rebuild_sprite_owners");
        map.unit_mapping.clear();
        map.bullet_mapping.clear();
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
    bw_print_error!("{}", msg);
    // TODO replace with recovery
    panic!("{}", msg);
}

fn animation_name(animation: u8) -> String {
    parse::animation_name(animation)
        .map(|x| x.into())
        .unwrap_or_else(|| format!("Unknown animation {:02x}", animation))
}

pub unsafe fn load_iscript() -> Iscript {
    use windows_sys::Win32::System::Threading::{GetCurrentProcess, TerminateProcess};

    loop {
        let iscript_txt = match crate::samase::read_file("scripts\\iscript.txt") {
            Some(s) => s,
            None => {
                let retry =
                    windows::message_box_retry("Aice", "Couldn't open scripts\\iscript.txt");
                if !retry {
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
                let retry = windows::message_box_retry("Aice", &msg);
                if !retry {
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
    let line_info = iscript.line_info.clone();
    *ISCRIPT.lock("set_as_bw_script") = Some(iscript);
    *LINE_INFO.lock() = Some(line_info);
}

pub unsafe extern "C" fn iscript_read_hook(_filename: *const u8, out_size: *mut u32) -> *mut u8 {
    use windows_sys::Win32::System::Memory::{GetProcessHeap, HeapAlloc};
    debug!("Iscript read hook");
    let data = HeapAlloc(GetProcessHeap(), 0, 0x10000) as *mut u8;
    *out_size = 0x10000;
    let iscript = crate::globals::init_for_lobby_map_preview();
    assert!(iscript.bw_data.len() < 0x10000);
    std::ptr::copy_nonoverlapping(iscript.bw_data.as_ptr(), data, iscript.bw_data.len());
    let line_info = iscript.line_info.clone();
    *ISCRIPT.lock("set_as_bw_script") = Some(iscript);
    *LINE_INFO.lock() = Some(line_info);
    data
}

pub unsafe extern "C" fn create_unit_hook(
    id: u32,
    x: i32,
    y: i32,
    player: u32,
    skins: *const u8,
    orig: unsafe extern "C" fn(u32, i32, i32, u32, *const u8) -> *mut c_void,
) -> *mut c_void {
    let prev_creating = CREATING_UNIT.load(Ordering::Relaxed);
    CREATING_UNIT.store(id as usize, Ordering::Relaxed);
    let result = orig(id, x, y, player, skins);
    CREATING_UNIT.store(prev_creating, Ordering::Relaxed);
    if let Some(unit) = Unit::from_ptr(result as *mut bw::Unit) {
        let sprite = (**unit).flingy.sprite;
        if sprite.is_null() {
            bw_print!("ERROR: Unit {:x} was created without a sprite", id);
            return null_mut();
        } else {
            if let Some(map) = SPRITE_OWNER_MAP.get() {
                let mut sprite_owner_map = map.lock("create_unit_hook");
                sprite_owner_map.add_unit(unit, sprite);
                if let Some(subunit) = unit.subunit_linked() {
                    sprite_owner_map.add_unit(subunit, (**subunit).flingy.sprite);
                }
            }
        }
    }
    result
}

pub unsafe extern "C" fn create_bullet_hook(
    id: u32,
    x: i32,
    y: i32,
    player: u32,
    direction: u32,
    parent: *mut c_void,
    orig: unsafe extern "C" fn(u32, i32, i32, u32, u32, *mut c_void) -> *mut c_void,
) -> *mut c_void {
    let prev_creating = CREATING_BULLET.load(Ordering::Relaxed);
    CREATING_BULLET.store(id as usize, Ordering::Relaxed);
    let result = orig(id, x, y, player, direction, parent);
    CREATING_BULLET.store(prev_creating, Ordering::Relaxed);
    if result.is_null() == false {
        let bullet = result as *mut bw::Bullet;
        let sprite = (*bullet).flingy.sprite;
        if sprite.is_null() {
            bw_print!("ERROR: Bullet {:x} was created without a sprite", id);
            return null_mut();
        } else {
            if let Some(map) = SPRITE_OWNER_MAP.get() {
                let mut sprite_owner_map = map.lock("create_bullet_hook");
                sprite_owner_map.add_bullet(bullet, sprite);
            }
        }
    }
    result
}

pub unsafe extern "C" fn order_hook(u: *mut c_void, orig: unsafe extern "C" fn(*mut c_void)) {
    do_queued_transforms();

    let unit = Unit::from_ptr(u as *mut bw::Unit).unwrap();

    // Account for unit's sprite having changed due to transforming
    // by readding it to sprite_owner_map
    // (Probably a bit inefficient but unit can transform due to order (could be caught here),
    // but also player-sent command or even ai reacting to a hit)
    if let Some(sprite) = unit.sprite() {
        if let Some(map) = SPRITE_OWNER_MAP.get() {
            let mut sprite_owner_map = map.lock("order_hook");
            sprite_owner_map.add_unit(unit, *sprite);
        }
        CURRENT_ORDER_SPRITE.store(*sprite as usize, Ordering::Relaxed);
    }
    CURRENT_ORDER_UNIT.store(u as usize, Ordering::Relaxed);
    let order = unit.order();
    let order_state = unit.order_state();
    orig(u);
    if order == bw_dat::order::DIE && order_state == 0 {
        let mut globals_guard = Globals::get("order_hook die cleanup");
        let globals = &mut *globals_guard;
        let unit_array = bw::unit_array();
        let index = unit_array.to_index(unit);
        if let Some(x) = globals.iscript_state.unit_generations.get_mut(index as usize) {
            if *x & 0x8000 == 0x8000 {
                *x &= 0x7fff;
            }
        }
    }

    // Make pylon auras run their iscript every frame, allowing them to be customized a bit
    if let Some(aura) = unit.pylon_aura() {
        for image in aura.images() {
            let image = *image;
            let iscript = ptr::addr_of_mut!((*image).iscript);
            crate::samase::step_iscript(image, iscript, false);
        }
    }
    CURRENT_ORDER_UNIT.store(0, Ordering::Relaxed);
    CURRENT_ORDER_SPRITE.store(0, Ordering::Relaxed);
}

fn do_queued_transforms() {
    if QUEUED_TRANSFORMS.load(Ordering::Relaxed) != 0 {
        // Note: Will handle also any new queues from transforming too.
        let unit_array = bw::unit_array();
        for i in 0.. {
            let (unit, dest) = {
                let mut globals_guard = Globals::get("do_queued_transforms");
                let globals = &mut *globals_guard;
                match globals.iscript_state.queued_transforms.get(i)  {
                    Some(&s) => s,
                    None => {
                        globals.iscript_state.queued_transforms.clear();
                        break;
                    }
                }
            };
            if let Some(unit) = unit_array.get_by_unique_id(unit) {
                unsafe {
                    crate::samase::transform_unit(*unit, dest);
                }
            }
        }
        QUEUED_TRANSFORMS.store(0, Ordering::Relaxed);
    }
}

pub unsafe extern "C" fn after_step_objects() {
    do_queued_hide_show();
}

fn do_queued_hide_show() {
    let mut queue = Vec::new();
    while QUEUED_HIDE_SHOW.load(Ordering::Relaxed) != 0 {
        let unit_array = bw::unit_array();
        {
            let mut globals_guard = Globals::get("do_queued_hide_show");
            let globals = &mut *globals_guard;
            mem::swap(&mut globals.iscript_state.queued_hide_show, &mut queue)
        }
        QUEUED_HIDE_SHOW.store(0, Ordering::Relaxed);
        for &(unit, show) in &queue {
            if let Some(unit) = unit_array.get_by_unique_id(unit) {
                unsafe {
                    if show {
                        crate::samase::show_unit(*unit);
                    } else {
                        crate::samase::hide_unit(*unit);
                    }
                }
            }
        }
        queue.clear();
    }
    if queue.capacity() != 0 {
        let mut globals_guard = Globals::get("do_queued_hide_show");
        let globals = &mut *globals_guard;
        mem::swap(&mut globals.iscript_state.queued_hide_show, &mut queue)
    }
}

/// Return (0, index) for normal images, (1, index) for selection circles (0..0x50),
/// (1, index) for hp bars (0x50..0x5c), (1, 0x5c..0x9c) for placement images,
/// (1, 0x9c..0xdc) for placement rects,
fn image_to_index(image: *mut bw::Image) -> Option<(usize, usize)> {
    #[cold]
    fn invalid_image(image: *mut bw::Image) -> Option<(usize, usize)> {
        bw_print_error!("ERROR: Invalid image pointer {:p}", image);
        None
    }

    fn check(
        image: *mut bw::Image,
        arr_id: usize,
        array_start: *mut bw::Image,
        array_size: usize,
    ) -> Option<(usize, usize)> {
        let offset_bytes = image.addr().wrapping_sub(array_start.addr());
        let index = offset_bytes / size_of::<bw::Image>();
        if offset_bytes % size_of::<bw::Image>() != 0 {
            return None;
        }
        if index >= array_size {
            return None;
        }
        Some((arr_id, index))
    }

    unsafe {
        let vec = samase::images_vector();
        let array_start = *vec;
        let vec_size = *(vec as *mut usize).add(1);
        check(image, 0, array_start, vec_size)
            .or_else(|| {
                let images = samase::selection_and_placment_images();
                check(image, 1, images[0], 0x50)
                    .or_else(|| check(image, 1, images[1], 0xc).map(|x| (x.0, 0x50 + x.1)))
                    .or_else(|| check(image, 1, images[2], 0x40).map(|x| (x.0, 0x5c + x.1)))
                    .or_else(|| check(image, 1, images[3], 0x40).map(|x| (x.0, 0x9c + x.1)))
            })
            .or_else(|| invalid_image(image))
    }
}
