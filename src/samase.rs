use std::mem;
use std::ptr::{NonNull, null_mut};
use std::sync::atomic::{AtomicUsize, AtomicU8, AtomicPtr, Ordering};

use libc::c_void;
use winapi::um::processthreadsapi::{GetCurrentProcess, TerminateProcess};

use bw_dat::{OrderId, UnitId, ImageId, SpriteId};
use samase_plugin::{FfiStr, FuncId, PluginApi, VarId};

use crate::bw;
use crate::iscript;
use crate::parse;
use crate::windows;

struct GlobalFunc<T: Copy>(AtomicUsize, std::marker::PhantomData<T>);

impl<T: Copy> GlobalFunc<T> {
    const fn new() -> GlobalFunc<T> {
        GlobalFunc(AtomicUsize::new(0), std::marker::PhantomData)
    }

    fn set(&self, val: T) {
        unsafe {
            self.0.store(mem::transmute_copy(&val), Ordering::Relaxed);
        }
    }

    fn set_required<U>(&self, val: Option<U>) {
        assert!(size_of::<U>() == size_of::<T>());
        if let Some(val) = val {
            unsafe {
                self.0.store(mem::transmute_copy(&val), Ordering::Relaxed);
            }
        } else {
            panic!("Required function not available");
        }
    }

    fn get(&self) -> T {
        unsafe {
            let value = self.0.load(Ordering::Relaxed);
            debug_assert!(value != 0);
            mem::transmute_copy(&value)
        }
    }

    fn get_opt(&self) -> Option<T> {
        unsafe {
            mem::transmute_copy::<usize, Option<T>>(&self.0.load(Ordering::Relaxed))
        }
    }

    #[expect(dead_code)]
    fn has_value(&self) -> bool {
        self.get_opt().is_some()
    }

    fn try_init(&self, val: Option<*mut c_void>) -> bool {
        let val = match val {
            Some(s) => s,
            None => return false,
        };
        unsafe {
            let value: usize = mem::transmute(val);
            self.0.store(value, Ordering::Relaxed);
        }
        true
    }

    fn init(&self, val: Option<*mut c_void>, desc: &str) {
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

static GLOBALS: &[VarId] = &[
    VarId::Game,
    VarId::FirstActiveUnit,
    VarId::FirstHiddenUnit,
    VarId::FirstLoneSprite,
    VarId::FirstFowSprite,
    VarId::Players,
    VarId::MapTileFlags,
    VarId::IscriptBin,
    VarId::SpriteHlines,
    VarId::IsMultiplayer,
    VarId::ActiveIscriptBullet,
    VarId::ActiveIscriptUnit,
    VarId::ImagesVector,
    VarId::SelectionCircleImages,
    VarId::HpBarImages,
    VarId::PlacementImages,
    VarId::PlacementRects,
    // Writable
    // Optional
    VarId::RngSeed,
    // Writable + Optional
];

const fn global_idx(var: VarId) -> usize {
    let mut i = 0;
    loop {
        if GLOBALS[i] as u16 == var as u16 {
            return i;
        }
        i += 1;
    }
}

const FIRST_WRITABLE_GLOBAL: usize = global_idx(VarId::RngSeed);
const FIRST_OPTIONAL_GLOBAL: usize = global_idx(VarId::RngSeed);

const ZERO_U8: AtomicU8 = AtomicU8::new(0);
static OPT_GLOBALS: [AtomicU8; GLOBALS.len() - FIRST_OPTIONAL_GLOBAL] = [
    ZERO_U8; GLOBALS.len() - FIRST_OPTIONAL_GLOBAL
];

unsafe fn init_globals(api: *const samase_plugin::PluginApi) {
    let mut result = [0u8; GLOBALS.len()];
    ((*api).load_vars)(GLOBALS.as_ptr() as *const u16, result.as_mut_ptr(), GLOBALS.len());
    let mut i = 0;
    for (last, needed) in [(FIRST_WRITABLE_GLOBAL, 2), (FIRST_OPTIONAL_GLOBAL, 3)] {
        while i < last {
            if result[i] < needed {
                if result[i] == 1 {
                    fatal(
                        &format!("Newer samase is required (Failed to get variable {:?})", GLOBALS[i])
                    );
                } else {
                    fatal(&format!("Failed to get variable {:?}", GLOBALS[i]));
                }
            }
            i += 1;
        }
    }
    while i < GLOBALS.len() {
        OPT_GLOBALS[i - FIRST_OPTIONAL_GLOBAL].store(result[i], Ordering::Relaxed);
        i += 1;
    }
}

static READ_VARS:
    GlobalFunc<unsafe extern "C" fn(*const u16, *mut usize, usize)> = GlobalFunc::new();
fn read_var(var: VarId) -> usize {
    unsafe {
        let var = var as u16;
        let mut out = 0usize;
        READ_VARS.get()(&var, &mut out, 1);
        out
    }
}

fn read_vars<const N: usize>(vars: &[VarId; N]) -> [usize; N] {
    unsafe {
        let mut out = [0usize; N];
        let mut u16_vars = [0u16; N];
        for i in 0..N {
            u16_vars[i] = vars[i] as u16;
        }
        READ_VARS.get()(u16_vars.as_ptr(), out.as_mut_ptr(), N);
        out
    }
}

static WRITE_VARS:
    GlobalFunc<unsafe extern "C" fn(*const u16, *const usize, usize)> = GlobalFunc::new();
#[expect(dead_code)]
fn write_var(var: VarId, value: usize) {
    unsafe {
        let var = var as u16;
        WRITE_VARS.get()(&var, &value, 1);
    }
}

/// Returns result from samase api (0/1 = bad, 2 = read-only, 3 = read/write)
macro_rules! opt_global {
    ($id:expr) => {{
        const IDX: usize = global_idx($id) - FIRST_OPTIONAL_GLOBAL;
        OPT_GLOBALS[IDX].load(Ordering::Relaxed)
    }}
}

static CRASH_WITH_MESSAGE: GlobalFunc<unsafe extern "C" fn(*const u8) -> !> = GlobalFunc::new();
pub fn crash_with_message(msg: &str) -> ! {
    let msg = format!("{}\0", msg);
    unsafe { CRASH_WITH_MESSAGE.get()(msg.as_bytes().as_ptr()) }
}

static CREATE_EXT_UNIT_FIELD:
    GlobalFunc<unsafe extern "C" fn(*const FfiStr) -> u32> = GlobalFunc::new();
static READ_EXT_UNIT_FIELD: GlobalFunc<unsafe extern "C" fn(u32, u32) -> u32> = GlobalFunc::new();
static WRITE_EXT_UNIT_FIELD:
    GlobalFunc<unsafe extern "C" fn(u32, u32, u32) -> u32> = GlobalFunc::new();
static MUTATE_DAT:
    GlobalFunc<unsafe extern "C" fn(u32, u32) -> *mut c_void> = GlobalFunc::new();

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct ExtFieldId(pub u32);

pub fn create_extended_unit_field(name: &[u8]) -> ExtFieldId {
    unsafe {
        let name_ffi = FfiStr::from_bytes(name);
        let result = CREATE_EXT_UNIT_FIELD.get_opt().expect("Need newer samase")(&name_ffi);
        if result == 0 {
            panic!("Failed to init ext unit field {}", String::from_utf8_lossy(name));
        }
        ExtFieldId(result)
    }
}

pub fn read_extended_unit_field(unit_index: u32, id: ExtFieldId) -> u32 {
    unsafe {
        READ_EXT_UNIT_FIELD.get()(unit_index, id.0)
    }
}

/// Returns old value
pub fn write_extended_unit_field(unit_index: u32, id: ExtFieldId, value: u32) -> u32 {
    unsafe {
        WRITE_EXT_UNIT_FIELD.get()(unit_index, id.0, value)
    }
}

pub fn mutate_dat(dat: u32, array_id: u32) -> Option<NonNull<bw::DatTable>> {
    unsafe {
        let result = MUTATE_DAT.get_opt().expect("Need newer samase")(dat, array_id);
        NonNull::new(result as *mut bw::DatTable)
    }
}

pub fn game() -> *mut bw::Game {
    read_var(VarId::Game) as _
}

pub fn first_active_unit() -> *mut bw::Unit {
    read_var(VarId::FirstActiveUnit) as _
}

pub fn first_hidden_unit() -> *mut bw::Unit {
    read_var(VarId::FirstHiddenUnit) as _
}

pub fn first_fow_sprite() -> *mut bw::LoneSprite {
    read_var(VarId::FirstFowSprite) as _
}

pub fn first_lone_sprite() -> *mut bw::LoneSprite {
    read_var(VarId::FirstLoneSprite) as _
}

static GET_REGION: GlobalFunc<unsafe extern "C" fn(u32, u32) -> u32> = GlobalFunc::new();
pub fn get_region(x: u32, y: u32) -> u32 {
    unsafe { GET_REGION.get()(x, y) }
}

static DAT_REQUIREMENTS: GlobalFunc<unsafe extern "C" fn(u32, u32) -> *const u16> = GlobalFunc::new();
pub fn requirements(ty: u32, id: u32) -> *const u16 {
    unsafe { DAT_REQUIREMENTS.get()(ty, id) }
}

pub fn players() -> *mut bw::Player {
    read_var(VarId::Players) as _
}

pub fn map_tile_flags() -> *mut u32 {
    read_var(VarId::MapTileFlags) as _
}

pub fn get_iscript_bin() -> *mut u8 {
    read_var(VarId::IscriptBin) as _
}

pub fn sprite_hlines() -> *mut *mut bw::Sprite {
    read_var(VarId::SpriteHlines) as _
}

static UNIT_ARRAY_LEN: GlobalFunc<extern "C" fn(*mut *mut bw::Unit, *mut usize)> = GlobalFunc::new();
pub unsafe fn unit_array() -> (*mut bw::Unit, usize) {
    let mut size = 0usize;
    let mut ptr = null_mut();
    UNIT_ARRAY_LEN.get()(&mut ptr, &mut size);
    (ptr, size)
}

static ORDERS_DAT: AtomicPtr<bw_dat::DatTable> = AtomicPtr::new(null_mut());
pub fn orders_dat() -> *mut bw_dat::DatTable {
    ORDERS_DAT.load(Ordering::Relaxed)
}

static PRINT_TEXT: GlobalFunc<unsafe extern "C" fn(*const u8)> = GlobalFunc::new();
// Too common to be inlined. Would be better if PRINT_TEXT were changed to always be valid
// (But C ABI is still worse for binsize)
#[inline(never)]
pub fn print_text(msg: *const u8) {
    unsafe {
        if let Some(print) = PRINT_TEXT.get_opt() {
            print(msg);
        }
    }
}

pub fn rng_seed() -> Option<u32> {
    if opt_global!(VarId::RngSeed) >= 2 {
        Some(read_var(VarId::RngSeed) as u32)
    } else {
        None
    }
}

static CREATE_UNIT:
    GlobalFunc<unsafe extern "C" fn(u32, i32, i32, u32, *const u8) -> *mut bw::Unit> =
        GlobalFunc::new();
pub fn create_unit(id: u32, x: i32, y: i32, player: u32, skins: *const u8) -> *mut bw::Unit {
    unsafe {
        if let Some(create) = CREATE_UNIT.get_opt() {
            create(id, x, y, player, skins)
        } else {
            null_mut()
        }
    }
}

static FINISH_UNIT_PRE: GlobalFunc<extern "C" fn(*mut bw::Unit)> = GlobalFunc::new();
pub unsafe fn finish_unit_pre(unit: *mut bw::Unit) {
    (FINISH_UNIT_PRE.get())(unit)
}

static FINISH_UNIT_POST: GlobalFunc<extern "C" fn(*mut bw::Unit)> = GlobalFunc::new();
pub unsafe fn finish_unit_post(unit: *mut bw::Unit) {
    (FINISH_UNIT_POST.get())(unit)
}

static GIVE_AI: GlobalFunc<extern "C" fn(*mut bw::Unit)> = GlobalFunc::new();
pub unsafe fn give_ai(unit: *mut bw::Unit) {
    (GIVE_AI.get())(unit)
}

pub fn is_multiplayer() -> bool {
    read_var(VarId::IsMultiplayer) != 0
}

pub unsafe fn active_iscript_objects() -> (*mut bw::Unit, *mut bw::Bullet) {
    let arr = read_vars(&[VarId::ActiveIscriptUnit, VarId::ActiveIscriptBullet]);
    (arr[0] as *mut _, arr[1] as *mut _)
}

pub unsafe fn images_vector() -> *mut *mut bw::Image {
    read_var(VarId::ImagesVector) as *mut *mut bw::Image
}

pub unsafe fn selection_and_placment_images() -> [*mut bw::Image; 4] {
    let arr = read_vars(&[
        VarId::SelectionCircleImages,
        VarId::HpBarImages,
        VarId::PlacementImages,
        VarId::PlacementRects,
    ]);
    [
        arr[0] as *mut _,
        arr[1] as *mut _,
        arr[2] as *mut _,
        arr[3] as *mut _,
    ]
}

static ISSUE_ORDER: GlobalFunc<
    unsafe extern "C" fn(*mut bw::Unit, u32, u32, u32, *mut bw::Unit, u32),
> = GlobalFunc::new();

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

static ADD_OVERLAY_ISCRIPT: GlobalFunc<
    unsafe extern "C" fn(*mut bw::Image, u32, i32, i32, u32),
> = GlobalFunc::new();

pub unsafe fn add_overlay_iscript(
    base_image: *mut bw::Image,
    image: ImageId,
    x: i8,
    y: i8,
    above: bool,
) {
    ADD_OVERLAY_ISCRIPT.get()(base_image, image.0 as u32, x as i32, y as i32, above as u32)
}

static STEP_ISCRIPT: GlobalFunc<
    unsafe extern "C" fn(*mut bw::Image, *mut bw::Iscript, u32, *mut u32)
> = GlobalFunc::new();

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

static READ_FILE:
    GlobalFunc<unsafe extern "C" fn(*const u8, *mut usize) -> *mut u8> = GlobalFunc::new();
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
static PLACE_FINISHED_UNIT_CREEP: AtomicUsize = AtomicUsize::new(0);
static HIDE_UNIT: AtomicUsize = AtomicUsize::new(0);
static SHOW_UNIT: AtomicUsize = AtomicUsize::new(0);
static CREATE_LONE_SPRITE: AtomicUsize = AtomicUsize::new(0);

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
    load_func::<unsafe extern "C" fn(*mut bw::Unit)>(&KILL_UNIT)(unit)
}

pub unsafe fn unit_set_hp(unit: *mut bw::Unit, value: i32) {
    load_func::<unsafe extern "C" fn(*mut bw::Unit, i32)>(&UNIT_SET_HP)(unit, value)
}

pub unsafe fn give_unit(unit: *mut bw::Unit, player: u8) {
    load_func::<unsafe extern "C" fn(*mut bw::Unit, usize)>(&GIVE_UNIT)(unit, player as usize)
}

pub unsafe fn transform_unit(unit: *mut bw::Unit, id: UnitId) {
    load_func::<unsafe extern "C" fn(*mut bw::Unit, usize)>(&TRANSFORM_UNIT)(unit, id.0 as usize)
}

pub unsafe fn place_finished_unit_creep(unit_id: u32, x: i32, y: i32) {
    load_func::<unsafe extern "C" fn(u32, i32, i32)>(&PLACE_FINISHED_UNIT_CREEP)(unit_id, x, y)
}

pub unsafe fn hide_unit(unit: *mut bw::Unit) {
    load_func::<unsafe extern "C" fn(*mut bw::Unit)>(&HIDE_UNIT)(unit)
}

pub unsafe fn show_unit(unit: *mut bw::Unit) {
    load_func::<unsafe extern "C" fn(*mut bw::Unit)>(&SHOW_UNIT)(unit)
}

pub unsafe fn create_lone_sprite(
    id: SpriteId,
    x: i32,
    y: i32,
    player: u8,
) -> Option<NonNull<bw::LoneSprite>> {
    let ret = load_func::<unsafe extern "C" fn(u32, i32, i32, u32) -> *mut bw::LoneSprite>(
        &CREATE_LONE_SPRITE
    )(id.0 as u32, x, y, player as u32);
    NonNull::new(ret)
}

#[no_mangle]
pub unsafe extern "C" fn samase_plugin_init(api: *const PluginApi) {
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
    CRASH_WITH_MESSAGE.set((*api).crash_with_message);
    if (*api).version >= 43 {
        CREATE_EXT_UNIT_FIELD.set((*api).create_extended_unit_field);
        READ_EXT_UNIT_FIELD.set((*api).read_extended_unit_field);
        WRITE_EXT_UNIT_FIELD.set((*api).write_extended_unit_field);
    }
    if (*api).version >= 44 {
        MUTATE_DAT.set((*api).mutate_dat);
    }
    READ_VARS.set((*api).read_vars);
    WRITE_VARS.set((*api).write_vars);
    init_globals(api);

    GET_REGION.init(
        ((*api).get_region)().map(|x| mem::transmute(x)),
        "get_region",
    );
    DAT_REQUIREMENTS.init(
        ((*api).dat_requirements)().map(|x| mem::transmute(x)),
        "dat_requirements",
    );
    let read_file = ((*api).read_file)();
    READ_FILE.set(read_file);
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
    ORDERS_DAT.store(orders_dat as *mut _, Ordering::Relaxed);

    UNIT_ARRAY_LEN.set_required(((*api).unit_array_len)());

    PRINT_TEXT.set_required(((*api).print_text)());
    CREATE_UNIT.set_required(((*api).create_unit)());
    FINISH_UNIT_PRE.set_required(((*api).finish_unit_pre)());
    FINISH_UNIT_POST.set_required(((*api).finish_unit_post)());
    GIVE_AI.set_required(((*api).give_ai)());
    ISSUE_ORDER.set_required(((*api).issue_order)());
    ADD_OVERLAY_ISCRIPT.set_required(((*api).add_overlay_iscript)());
    STEP_ISCRIPT.set_required(((*api).step_iscript)());
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
    let result = ((*api).hook_step_objects)(crate::iscript::after_step_objects, 1);
    if result == 0 {
        fatal("Couldn't hook step_objects");
    }

    static FUNCS: &[(&AtomicUsize, FuncId)] = &[
        (&KILL_UNIT, FuncId::KillUnit),
        (&UNIT_SET_HP, FuncId::UnitSetHp),
        (&GIVE_UNIT, FuncId::GiveUnit),
        (&TRANSFORM_UNIT, FuncId::TransformUnit),
        (&PLACE_FINISHED_UNIT_CREEP, FuncId::PlaceFinishedUnitCreep),
        (&HIDE_UNIT, FuncId::HideUnit),
        (&SHOW_UNIT, FuncId::ShowUnit),
        (&CREATE_LONE_SPRITE, FuncId::CreateLoneSprite),
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
