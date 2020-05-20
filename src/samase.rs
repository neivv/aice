use std::mem;
use std::ptr::{NonNull, null_mut};

use libc::c_void;
use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

use samase_shim::PluginApi;

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

static mut FIRST_ACTIVE_BULLET: GlobalFunc<extern fn() -> *mut bw::Bullet> = GlobalFunc(None);
pub fn first_active_bullet() -> *mut bw::Bullet {
    unsafe { FIRST_ACTIVE_BULLET.0.map(|x| x()).unwrap_or(null_mut()) }
}

static mut UNITS_DAT: GlobalFunc<extern fn() -> *mut bw_dat::DatTable> = GlobalFunc(None);
pub fn units_dat() -> *mut bw_dat::DatTable {
    unsafe { UNITS_DAT.get()() }
}

static mut WEAPONS_DAT: GlobalFunc<extern fn() -> *mut bw_dat::DatTable> = GlobalFunc(None);
pub fn weapons_dat() -> *mut bw_dat::DatTable {
    unsafe { WEAPONS_DAT.get()() }
}

static mut UPGRADES_DAT: GlobalFunc<extern fn() -> *mut bw_dat::DatTable> = GlobalFunc(None);
pub fn upgrades_dat() -> *mut bw_dat::DatTable {
    unsafe { UPGRADES_DAT.get()() }
}

static mut TECHDATA_DAT: GlobalFunc<extern fn() -> *mut bw_dat::DatTable> = GlobalFunc(None);
pub fn techdata_dat() -> *mut bw_dat::DatTable {
    unsafe { TECHDATA_DAT.get()() }
}

static mut ORDERS_DAT: GlobalFunc<extern fn() -> *mut bw_dat::DatTable> = GlobalFunc(None);
pub fn orders_dat() -> *mut bw_dat::DatTable {
    unsafe { ORDERS_DAT.get()() }
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

static mut GET_ISCRIPT_BIN: GlobalFunc<extern fn() -> *mut u8> = GlobalFunc(None);
pub fn get_iscript_bin() -> *mut u8 {
    unsafe { GET_ISCRIPT_BIN.get()() }
}

static mut SPRITE_HLINES: GlobalFunc<extern fn() -> *mut *mut bw::Sprite> = GlobalFunc(None);
pub fn sprite_hlines() -> *mut *mut bw::Sprite {
    unsafe { SPRITE_HLINES.get()() }
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

#[no_mangle]
pub unsafe extern fn samase_plugin_init(api: *const PluginApi) {
    bw_dat::set_is_scr(crate::is_scr());
    let required_version = 22;
    if (*api).version < required_version {
        fatal(&format!(
            "Newer samase is required. (Plugin API version {}, this plugin requires version {})",
            (*api).version,
            required_version,
        ));
    }

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
    FIRST_ACTIVE_BULLET.init(
        ((*api).first_active_bullet)().map(|x| mem::transmute(x)),
        "first active bullet",
    );
    FIRST_HIDDEN_UNIT.init(
        ((*api).first_hidden_unit)().map(|x| mem::transmute(x)),
        "first hidden unit",
    );
    PLAYERS.init(((*api).players)().map(|x| mem::transmute(x)), "players");
    let read_file = ((*api).read_file)();
    READ_FILE.0 = Some(mem::transmute(read_file));
    UNITS_DAT.init(((*api).dat)(0).map(|x| mem::transmute(x)), "units.dat");
    bw_dat::init_units(units_dat());
    WEAPONS_DAT.init(((*api).dat)(1).map(|x| mem::transmute(x)), "weapons.dat");
    bw_dat::init_weapons(weapons_dat());
    UPGRADES_DAT.init(((*api).dat)(3).map(|x| mem::transmute(x)), "upgrades.dat");
    bw_dat::init_upgrades(upgrades_dat());
    TECHDATA_DAT.init(((*api).dat)(4).map(|x| mem::transmute(x)), "techdata.dat");
    bw_dat::init_techdata(techdata_dat());
    ORDERS_DAT.init(((*api).dat)(7).map(|x| mem::transmute(x)), "orders.dat");
    bw_dat::init_orders(orders_dat());

    GET_ISCRIPT_BIN.init(
        ((*api).get_iscript_bin)().map(|x| mem::transmute(x)),
        "get_iscript_bin",
    );
    SPRITE_HLINES.init(((*api).sprite_hlines)().map(|x| mem::transmute(x)), "sprite_hlines");

    PRINT_TEXT.0 = Some(mem::transmute(((*api).print_text)()));
    RNG_SEED.0 = Some(mem::transmute(((*api).rng_seed)()));
    CREATE_UNIT.0 = Some(mem::transmute(((*api).create_unit)()));
    FINISH_UNIT_PRE.0 = Some(mem::transmute(((*api).finish_unit_pre)()));
    FINISH_UNIT_POST.0 = Some(mem::transmute(((*api).finish_unit_post)()));
    let result = ((*api).extend_save)(
        "aice\0".as_ptr(),
        Some(crate::globals::save),
        Some(crate::globals::load),
        crate::globals::init_game,
    );
    if result == 0 {
        ((*api).warn_unsupported_feature)(b"Saving\0".as_ptr());
    }

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

    crate::init();
}
