use std::mem;
use std::ptr::{NonNull, null_mut};
use std::sync::atomic::{AtomicUsize, Ordering};

use libc::c_void;
use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

use bw_dat::{OrderId, UnitId, ImageId};
use samase_plugin::{FuncId, PluginApi};

use crate::bw;
use crate::iscript;
use crate::parse;
use crate::windows;

struct GlobalFunc<T: Copy>(Option<T>);

impl<T: Copy> GlobalFunc<T> {
    fn get(&self) -> T {
        self.0.unwrap()
    }

    fn try_init(&mut self, val: Option<*mut c_void>) -> bool {
        let val = match val {
            Some(s) => s,
            None => return false,
        };
        unsafe {
            assert_eq!(mem::size_of::<T>(), mem::size_of::<*mut c_void>());
            let mut typecast_hack: mem::MaybeUninit<T> = mem::MaybeUninit::uninit();
            *(typecast_hack.as_mut_ptr() as *mut *mut c_void) = val;
            self.0 = Some(typecast_hack.assume_init());
        }
        true
    }

    fn init(&mut self, val: Option<*mut c_void>, desc: &str) {
        if !self.try_init(val) {
            fatal(&format!("Can't get {}", desc));
        }
    }
}

fn fatal(text: &str) -> ! {
    let msg = format!("This StarCraft version is not supported :(\n({})", text);
    windows::message_box("Aice", &msg);
    unsafe {
        TerminateProcess(GetCurrentProcess(), 0x4230daef);
    }
    unreachable!();
}

static mut CRASH_WITH_MESSAGE: GlobalFunc<unsafe extern fn(*const u8) -> !> = GlobalFunc(None);
pub fn crash_with_message(msg: &str) -> ! {
    let msg = format!("{}\0", msg);
    unsafe { CRASH_WITH_MESSAGE.get()(msg.as_bytes().as_ptr()) }
}

static mut GAME: GlobalFunc<extern fn() -> *mut bw::Game> = GlobalFunc(None);
pub fn game() -> *mut bw::Game {
    unsafe { GAME.get()() }
}

static mut FIRST_ACTIVE_UNIT: GlobalFunc<extern fn() -> *mut bw::Unit> = GlobalFunc(None);
pub fn first_active_unit() -> *mut bw::Unit {
    unsafe { FIRST_ACTIVE_UNIT.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut FIRST_HIDDEN_UNIT: GlobalFunc<extern fn() -> *mut bw::Unit> = GlobalFunc(None);
pub fn first_hidden_unit() -> *mut bw::Unit {
    unsafe { FIRST_HIDDEN_UNIT.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut FIRST_FOW_SPRITE: GlobalFunc<extern fn() -> *mut bw::LoneSprite> = GlobalFunc(None);
pub fn first_fow_sprite() -> *mut bw::LoneSprite {
    unsafe { FIRST_FOW_SPRITE.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut FIRST_LONE_SPRITE: GlobalFunc<extern fn() -> *mut bw::LoneSprite> = GlobalFunc(None);
pub fn first_lone_sprite() -> *mut bw::LoneSprite {
    unsafe { FIRST_LONE_SPRITE.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut GET_REGION: GlobalFunc<extern fn(u32, u32) -> u32> = GlobalFunc(None);
pub fn get_region(x: u32, y: u32) -> u32 {
    unsafe { GET_REGION.get()(x, y) }
}

static mut DAT_REQUIREMENTS: GlobalFunc<extern fn(u32, u32) -> *const u16> = GlobalFunc(None);
pub fn requirements(ty: u32, id: u32) -> *const u16 {
    unsafe { DAT_REQUIREMENTS.get()(ty, id) }
}

static mut PLAYERS: GlobalFunc<extern fn() -> *mut bw::Player> = GlobalFunc(None);
pub fn players() -> *mut bw::Player {
    unsafe { PLAYERS.get()() }
}

static mut MAP_TILE_FLAGS: GlobalFunc<extern fn() -> *mut u32> = GlobalFunc(None);
pub fn map_tile_flags() -> *mut u32 {
    unsafe { MAP_TILE_FLAGS.get()() }
}

static mut GET_ISCRIPT_BIN: GlobalFunc<extern fn() -> *mut u8> = GlobalFunc(None);
pub fn get_iscript_bin() -> *mut u8 {
    unsafe { GET_ISCRIPT_BIN.get()() }
}

static mut SPRITE_HLINES: GlobalFunc<extern fn() -> *mut *mut bw::Sprite> = GlobalFunc(None);
pub fn sprite_hlines() -> *mut *mut bw::Sprite {
    unsafe { SPRITE_HLINES.get()() }
}

static mut UNIT_ARRAY_LEN: GlobalFunc<extern fn(*mut *mut bw::Unit, *mut usize)> = GlobalFunc(None);
pub unsafe fn unit_array() -> (*mut bw::Unit, usize) {
    let mut size = 0usize;
    let mut ptr = null_mut();
    UNIT_ARRAY_LEN.get()(&mut ptr, &mut size);
    (ptr, size)
}

static ORDERS_DAT: AtomicUsize = AtomicUsize::new(0);
pub fn orders_dat() -> *mut bw_dat::DatTable {
    ORDERS_DAT.load(Ordering::Relaxed) as *mut bw_dat::DatTable
}

static mut PRINT_TEXT: GlobalFunc<extern fn(*const u8)> = GlobalFunc(None);
// Too common to be inlined. Would be better if PRINT_TEXT were changed to always be valid
// (But C ABI is still worse for binsize)
#[inline(never)]
pub fn print_text(msg: *const u8) {
    unsafe {
        if let Some(print) = PRINT_TEXT.0 {
            print(msg);
        }
    }
}

static mut RNG_SEED: GlobalFunc<extern fn() -> u32> = GlobalFunc(None);
pub fn rng_seed() -> Option<u32> {
    unsafe {
        if let Some(rng) = RNG_SEED.0 {
            Some(rng())
        } else {
            None
        }
    }
}

static mut CREATE_UNIT: GlobalFunc<extern fn(u32, i32, i32, u32, *const u8) -> *mut bw::Unit> = GlobalFunc(None);
pub fn create_unit(id: u32, x: i32, y: i32, player: u32, skins: *const u8) -> *mut bw::Unit {
    unsafe {
        if let Some(create) = CREATE_UNIT.0 {
            create(id, x, y, player, skins)
        } else {
            null_mut()
        }
    }
}

static mut FINISH_UNIT_PRE: GlobalFunc<extern fn(*mut bw::Unit)> = GlobalFunc(None);
pub unsafe fn finish_unit_pre(unit: *mut bw::Unit) {
    (FINISH_UNIT_PRE.0.unwrap())(unit)
}

static mut FINISH_UNIT_POST: GlobalFunc<extern fn(*mut bw::Unit)> = GlobalFunc(None);
pub unsafe fn finish_unit_post(unit: *mut bw::Unit) {
    (FINISH_UNIT_POST.0.unwrap())(unit)
}

static mut GIVE_AI: GlobalFunc<extern fn(*mut bw::Unit)> = GlobalFunc(None);
pub unsafe fn give_ai(unit: *mut bw::Unit) {
    (GIVE_AI.0.unwrap())(unit)
}

static mut IS_MULTIPLAYER: GlobalFunc<extern fn() -> u32> = GlobalFunc(None);
pub fn is_multiplayer() -> bool {
    unsafe { (IS_MULTIPLAYER.0.unwrap())() != 0 }
}

static mut ISCRIPT_OBJECTS:
    GlobalFunc<extern fn(*mut *mut c_void, *const *mut c_void)> = GlobalFunc(None);
pub unsafe fn active_iscript_objects(read: *mut *mut c_void, write: *const *mut c_void) {
    (ISCRIPT_OBJECTS.0.unwrap())(read, write)
}

static mut ISSUE_ORDER: GlobalFunc<
    unsafe extern fn(*mut bw::Unit, u32, u32, u32, *mut bw::Unit, u32),
> = GlobalFunc(None);

pub fn issue_order(
    unit: *mut bw::Unit,
    order: OrderId,
    x: u32,
    y: u32,
    target: *mut bw::Unit,
    fow_unit: UnitId,
) {
    assert!(x < 0x10000);
    assert!(y < 0x10000);
    assert!(unit != null_mut());
    unsafe { ISSUE_ORDER.get()(unit, order.0 as u32, x, y, target, fow_unit.0 as u32) }
}

static mut ADD_OVERLAY_ISCRIPT: GlobalFunc<
    unsafe extern fn(*mut bw::Image, u32, i32, i32, u32),
> = GlobalFunc(None);

pub unsafe fn add_overlay_iscript(
    base_image: *mut bw::Image,
    image: ImageId,
    x: i8,
    y: i8,
    above: bool,
) {
    ADD_OVERLAY_ISCRIPT.get()(base_image, image.0 as u32, x as i32, y as i32, above as u32)
}

static mut STEP_ISCRIPT: GlobalFunc<
    unsafe extern fn(*mut bw::Image, *mut bw::Iscript, u32, *mut u32)
> = GlobalFunc(None);

pub unsafe fn step_iscript(image: *mut bw::Image, iscript: *mut bw::Iscript, dry: bool) {
    STEP_ISCRIPT.get()(image, iscript, dry as u32, null_mut());
}

pub struct SamaseBox {
    data: NonNull<u8>,
    len: usize,
}

impl std::ops::Deref for SamaseBox {
    type Target = [u8];
    fn deref(&self) -> &[u8] {
        unsafe {
            ::std::slice::from_raw_parts(self.data.as_ptr(), self.len)
        }
    }
}

impl std::ops::Drop for SamaseBox {
    fn drop(&mut self) {
        use winapi::um::heapapi::{GetProcessHeap, HeapFree};
        unsafe {
            HeapFree(GetProcessHeap(), 0, self.data.as_ptr() as *mut _);
        }
    }
}

static mut READ_FILE: GlobalFunc<extern fn(*const u8, *mut usize) -> *mut u8> = GlobalFunc(None);
pub fn read_file(name: &str) -> Option<SamaseBox> {
    // Uh, should work fine
    let cstring = format!("{}\0", name);
    let mut size = 0usize;
    let result = unsafe { READ_FILE.get()(cstring.as_ptr(), &mut size) };
    NonNull::new(result).map(|data| SamaseBox {
        data,
        len: size,
    })
}

static KILL_UNIT: AtomicUsize = AtomicUsize::new(0);
static UNIT_SET_HP: AtomicUsize = AtomicUsize::new(0);
static GIVE_UNIT: AtomicUsize = AtomicUsize::new(0);
static TRANSFORM_UNIT: AtomicUsize = AtomicUsize::new(0);

#[cold]
fn func_fatal(value: usize) -> ! {
    if value == 1 {
        fatal("Newer samase is required to use this feature");
    } else {
        fatal("A required function was not accessible");
    }
}

#[inline]
fn load_func<T: Copy>(global: &AtomicUsize) -> T {
    let value = global.load(Ordering::Relaxed);
    if value < 2 {
        func_fatal(value);
    }
    debug_assert!(value != 0);
    assert!(mem::size_of::<T>() == mem::size_of::<fn()>());
    unsafe {
        mem::transmute_copy(&value)
    }
}

pub unsafe fn kill_unit(unit: *mut bw::Unit) {
    load_func::<unsafe extern fn(*mut bw::Unit)>(&KILL_UNIT)(unit)
}

pub unsafe fn unit_set_hp(unit: *mut bw::Unit, value: i32) {
    load_func::<unsafe extern fn(*mut bw::Unit, i32)>(&UNIT_SET_HP)(unit, value)
}

pub unsafe fn give_unit(unit: *mut bw::Unit, player: u8) {
    load_func::<unsafe extern fn(*mut bw::Unit, usize)>(&GIVE_UNIT)(unit, player as usize)
}

pub unsafe fn transform_unit(unit: *mut bw::Unit, id: UnitId) {
    load_func::<unsafe extern fn(*mut bw::Unit, usize)>(&TRANSFORM_UNIT)(unit, id.0 as usize)
}

#[no_mangle]
pub unsafe extern fn samase_plugin_init(api: *const PluginApi) {
    bw_dat::set_is_scr(crate::is_scr());
    let required_version = 35;
    if (*api).version < required_version {
        fatal(&format!(
            "Newer samase is required. (Plugin API version {}, this plugin requires version {})",
            (*api).version,
            required_version,
        ));
    }

    let mut ext_arrays = null_mut();
    let ext_arrays_len = ((*api).extended_arrays)(&mut ext_arrays);
    bw_dat::set_extended_arrays(ext_arrays as *mut _, ext_arrays_len);
    CRASH_WITH_MESSAGE.0 = Some((*api).crash_with_message);

    GAME.init(((*api).game)().map(|x| mem::transmute(x)), "Game object");
    GET_REGION.init(
        ((*api).get_region)().map(|x| mem::transmute(x)),
        "get_region",
    );
    DAT_REQUIREMENTS.init(
        ((*api).dat_requirements)().map(|x| mem::transmute(x)),
        "dat_requirements",
    );
    FIRST_ACTIVE_UNIT.init(
        ((*api).first_active_unit)().map(|x| mem::transmute(x)),
        "first active unit",
    );
    FIRST_HIDDEN_UNIT.init(
        ((*api).first_hidden_unit)().map(|x| mem::transmute(x)),
        "first hidden unit",
    );
    FIRST_LONE_SPRITE.try_init(((*api).first_lone_sprite)().map(|x| mem::transmute(x)));
    FIRST_FOW_SPRITE.try_init(((*api).first_fow_sprite)().map(|x| mem::transmute(x)));
    PLAYERS.init(((*api).players)().map(|x| mem::transmute(x)), "players");
    MAP_TILE_FLAGS.init(((*api).map_tile_flags)().map(|x| mem::transmute(x)), "map_tile_flags");
    let read_file = ((*api).read_file)();
    READ_FILE.0 = Some(mem::transmute(read_file));
    let mut dat_len = 0usize;
    let units_dat = ((*api).extended_dat)(0).expect("units.dat")(&mut dat_len);
    bw_dat::init_units(units_dat as *const _, dat_len);
    let weapons_dat = ((*api).extended_dat)(1).expect("weapons.dat")(&mut dat_len);
    bw_dat::init_weapons(weapons_dat as *const _, dat_len);
    let flingy_dat = ((*api).extended_dat)(2).expect("flingy.dat")(&mut dat_len);
    bw_dat::init_flingy(flingy_dat as *const _, dat_len);
    let upgrades_dat = ((*api).extended_dat)(3).expect("upgrades.dat")(&mut dat_len);
    bw_dat::init_upgrades(upgrades_dat as *const _, dat_len);
    let techdata_dat = ((*api).extended_dat)(4).expect("techdata.dat")(&mut dat_len);
    bw_dat::init_techdata(techdata_dat as *const _, dat_len);
    let sprites_dat = ((*api).extended_dat)(5).expect("sprites.dat")(&mut dat_len);
    bw_dat::init_sprites(sprites_dat as *const _, dat_len);
    let orders_dat = ((*api).extended_dat)(7).expect("orders.dat")(&mut dat_len);
    bw_dat::init_orders(orders_dat as *const _, dat_len);
    ORDERS_DAT.store(orders_dat as usize, Ordering::Relaxed);

    GET_ISCRIPT_BIN.init(
        ((*api).get_iscript_bin)().map(|x| mem::transmute(x)),
        "get_iscript_bin",
    );
    SPRITE_HLINES.init(((*api).sprite_hlines)().map(|x| mem::transmute(x)), "sprite_hlines");
    UNIT_ARRAY_LEN.0 = Some(mem::transmute(((*api).unit_array_len)()));

    PRINT_TEXT.0 = Some(mem::transmute(((*api).print_text)()));
    RNG_SEED.0 = Some(mem::transmute(((*api).rng_seed)()));
    CREATE_UNIT.0 = Some(mem::transmute(((*api).create_unit)()));
    FINISH_UNIT_PRE.0 = Some(mem::transmute(((*api).finish_unit_pre)()));
    FINISH_UNIT_POST.0 = Some(mem::transmute(((*api).finish_unit_post)()));
    GIVE_AI.0 = Some(mem::transmute(((*api).give_ai)()));
    IS_MULTIPLAYER.0 = Some(mem::transmute(((*api).is_multiplayer)()));
    ISSUE_ORDER.0 = Some(mem::transmute(((*api).issue_order)()));
    ADD_OVERLAY_ISCRIPT.0 = Some(mem::transmute(((*api).add_overlay_iscript)()));
    STEP_ISCRIPT.0 = Some(mem::transmute(((*api).step_iscript)()));
    ISCRIPT_OBJECTS.0 = Some(mem::transmute(((*api).active_iscript_objects)()));
    let result = ((*api).extend_save)(
        "aice\0".as_ptr(),
        Some(crate::globals::save),
        Some(crate::globals::load),
        crate::globals::init_game,
    );
    if result == 0 {
        ((*api).warn_unsupported_feature)(b"Saving\0".as_ptr());
    }
    ((*api).hook_init_units)(crate::globals::init_units_hook);

    let ok =
        ((*api).hook_iscript_opcode)(parse::AICE_COMMAND as u32, iscript::run_aice_script);
    if ok == 0 {
        fatal("Can't hook iscript");
    }
    ((*api).hook_file_read)(b"scripts\\iscript.bin\0".as_ptr(), iscript::iscript_read_hook);

    let ok = ((*api).hook_create_bullet)(crate::iscript::create_bullet_hook);
    if ok == 0 {
        fatal("Can't hook create_bullet");
    }

    let ok = ((*api).hook_create_unit)(crate::iscript::create_unit_hook);
    if ok == 0 {
        fatal("Can't hook create_unit");
    }
    let result = ((*api).hook_step_order)(crate::iscript::order_hook);
    if result == 0 {
        fatal("Couldn't hook step_order");
    }
    ((*api).hook_game_loop_start)(crate::globals::on_game_loop);

    static FUNCS: &[(&AtomicUsize, FuncId)] = &[
        (&KILL_UNIT, FuncId::KillUnit),
        (&UNIT_SET_HP, FuncId::UnitSetHp),
        (&GIVE_UNIT, FuncId::GiveUnit),
        (&TRANSFORM_UNIT, FuncId::TransformUnit),
    ];
    for &(global, func_id) in FUNCS {
        if func_id as u16 >= (*api).max_func_id {
            global.store(1, Ordering::Relaxed);
            continue;
        }
        let func = ((*api).get_func)(func_id as u16);
        if let Some(f) = func {
            global.store(f as usize, Ordering::Relaxed);
        }
    }

    crate::init();
}
