use std::slice;

use lazy_static::lazy_static;
use serde_derive::{Serialize, Deserialize};

use crate::iscript;
use crate::recurse_checked_mutex::{Mutex, MutexGuard};

lazy_static! {
    static ref GLOBALS: Mutex<Globals> = Mutex::new(Globals::new());
}

pub struct SendPtr<T>(pub *mut T);
unsafe impl<T> Send for SendPtr<T> {}

#[derive(Serialize, Deserialize)]
pub struct Globals {
}

impl Globals {
    fn new() -> Globals {
        Globals {
        }
    }

    // Should only be called on hook start to prevent deadlocks.
    // Inline never since it keeps getting inlined and lazy_static init code is fat ;_;
    #[inline(never)]
    pub fn get(caller: &'static str) -> MutexGuard<'static, Globals> {
        GLOBALS.lock(caller)
    }
}

pub unsafe extern fn init_game() {
    let iscript = iscript::load_iscript(true);
    iscript::set_as_bw_script(iscript);
    *Globals::get("init") = Globals::new();
}

pub unsafe extern fn save(set_data: unsafe extern fn(*const u8, usize)) {
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
