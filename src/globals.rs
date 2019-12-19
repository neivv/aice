use std::ptr::null_mut;
use std::slice;
use std::sync::atomic::{AtomicUsize, Ordering};

use lazy_static::lazy_static;
use serde_derive::{Serialize, Deserialize};
use serde::{Serializer, Deserializer};

use bw_dat::Game;

use crate::bw;
use crate::iscript;
use crate::recurse_checked_mutex::{Mutex, MutexGuard};

lazy_static! {
    static ref GLOBALS: Mutex<Globals> = Mutex::new(Globals::new());
}

#[derive(Serialize, Deserialize)]
pub struct Globals {
    pub iscript_state: iscript::IscriptState,
}

impl Globals {
    fn new() -> Globals {
        Globals {
            iscript_state: iscript::IscriptState::default(),
        }
    }

    // Should only be called on hook start to prevent deadlocks.
    // Inline never since it keeps getting inlined and lazy_static init code is fat ;_;
    #[inline(never)]
    pub fn get(caller: &'static str) -> MutexGuard<'static, Globals> {
        GLOBALS.lock(caller)
    }
}

/// Since lobby map preview uses CreateUnit which runs a frame of iscript ._.
/// TODO make lobby preview jsut use a dummy script?
pub unsafe extern fn init_for_lobby_map_preview() -> crate::parse::Iscript {
    let mut globals = Globals::new();
    let iscript = iscript::load_iscript(true);
    globals.iscript_state = iscript::IscriptState::from_script(&iscript);
    *Globals::get("init") = globals;
    iscript
}

pub unsafe extern fn init_game() {
    let mut globals = Globals::new();
    let iscript = iscript::load_iscript(true);
    globals.iscript_state = iscript::IscriptState::from_script(&iscript);
    iscript::set_as_bw_script(iscript);
    *Globals::get("init") = globals;
}

pub unsafe extern fn save(set_data: unsafe extern fn(*const u8, usize)) {
    init_sprite_save_load();
    let globals = Globals::get("save");
    match bincode::serialize(&*globals) {
        Ok(o) => {
            set_data(o.as_ptr(), o.len());
        }
        Err(e) => {
            error!("Couldn't save game: {}", e);
            bw_print!("(Aice) Couldn't save game: {}", e);
        }
    }
}

pub unsafe extern fn load(ptr: *const u8, len: usize) -> u32 {
    init_sprite_save_load();
    let slice = slice::from_raw_parts(ptr, len);
    let data: Globals = match bincode::deserialize(slice) {
        Ok(o) => o,
        Err(e) => {
            error!("Couldn't load game: {}", e);
            return 0;
        }
    };
    let iscript = iscript::load_iscript(true);
    iscript::set_as_bw_script(iscript);
    *Globals::get("load") = data;
    1
}

fn init_sprite_save_load() {
    unsafe {
        let game = Game::from_ptr(bw::game());
        let hlines = crate::samase::sprite_hlines();
        let sprite = (0..(**game).map_height_tiles).filter_map(|x| {
            let ptr = *hlines.add(x as usize);
            if ptr.is_null() {
                None
            } else {
                Some(ptr)
            }
        }).next().unwrap_or(null_mut());
        SAVE_SPRITE_ARRAY.store(sprite as usize, Ordering::Relaxed);
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub struct SerializableSprite(pub *mut bw::Sprite);
static SAVE_SPRITE_ARRAY: AtomicUsize = AtomicUsize::new(0);

impl serde::Serialize for SerializableSprite {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let offset = (self.0 as usize).wrapping_sub(SAVE_SPRITE_ARRAY.load(Ordering::Relaxed));
        (offset as u32).serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for SerializableSprite {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        use serde::de::Error;
        let offset = u32::deserialize(deserializer)?;
        if offset != 0 {
            let ptr = (SAVE_SPRITE_ARRAY.load(Ordering::Relaxed) as usize)
                .wrapping_add(offset as usize) as *mut bw::Sprite;
            Ok(SerializableSprite(ptr))
        } else {
            Err(S::Error::custom(format!("Can't load sprite")))
        }
    }
}
