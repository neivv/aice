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
    /// This is reset on game init, so copy it as global
    pub player_lobby_color_choices: PlayerColorChoices,
}

#[derive(Serialize, Deserialize)]
pub enum PlayerColorChoices {
    /// (color, storm id).
    /// Index by player id but gets obsolete on game start randomization
    BeforeRandomization([(u8, u32); 8]),
    /// Index by player id.
    /// Storm id dropped, may not be reliable after loading save?
    AfterRandomization([u8; 8]),
}

impl PlayerColorChoices {
    pub fn get(&mut self, player: u8) -> u8 {
        self.fixup_post_random()
            .get(player as usize)
            .copied()
            .unwrap_or(0x16)
    }

    fn fixup_post_random(&mut self) -> &[u8; 8] {
        match self {
            PlayerColorChoices::BeforeRandomization(arr) => unsafe {
                let players = crate::samase::players();
                // TODO: Ai players.
                // Since they don't have anything that they can be uniquely identified
                // before randomization, they should just check player_color_rgba
                // and use that to match with picked colors.
                let mut fixed = [0x16u8; 8];
                for &(choice, storm_id) in arr.iter() {
                    if storm_id != !0 {
                        if let Some(player_id) =
                            (0..8).find(|&x| (*players.add(x)).storm_id == storm_id)
                        {
                            fixed[player_id] = choice;
                        }
                    }
                }
                *self = PlayerColorChoices::AfterRandomization(fixed);
                match self {
                    PlayerColorChoices::AfterRandomization(arr) => arr,
                    _ => &[0; 8],
                }
            }
            PlayerColorChoices::AfterRandomization(arr) => arr,
        }
    }
}

impl Globals {
    fn new() -> Globals {
        Globals {
            iscript_state: iscript::IscriptState::default(),
            player_lobby_color_choices: PlayerColorChoices::BeforeRandomization([(0, 0); 8]),
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
pub unsafe fn init_for_lobby_map_preview() -> crate::parse::Iscript {
    let mut globals = Globals::new();
    let iscript = iscript::load_iscript(true);
    globals.iscript_state = iscript::IscriptState::from_script(&iscript);
    *Globals::get("init") = globals;
    iscript
}

pub unsafe extern fn init_game() {
    let game = Game::from_ptr(bw::game());
    let players = crate::samase::players();
    let mut globals = Globals::new();
    let iscript = iscript::load_iscript(true);
    globals.iscript_state = iscript::IscriptState::from_script(&iscript);
    let mut arr = [(0, 0); 8];
    for i in 0..8 {
        let choice = (**game).scr_player_color_preference[i];
        let storm_id = (*players.add(i)).storm_id;
        arr[i] = (choice, storm_id);
    }
    globals.player_lobby_color_choices = PlayerColorChoices::BeforeRandomization(arr);
    iscript::set_as_bw_script(iscript);
    *Globals::get("init") = globals;
    iscript::rebuild_sprite_owners();
}

pub unsafe extern fn save(set_data: unsafe extern fn(*const u8, usize)) {
    init_sprite_save_load();
    let mut globals = Globals::get("save");
    // Not sure if keeping net player ids in save is a good idea, so force fixup on save.
    globals.player_lobby_color_choices.fixup_post_random();
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
    iscript::rebuild_sprite_owners();
    1
}

fn init_sprite_save_load() {
    unsafe {
        let game = Game::from_ptr(bw::game());
        let hlines = crate::samase::sprite_hlines();
        let (sprite, image) = (0..(**game).map_height_tiles).filter_map(|x| {
            let ptr = *hlines.add(x as usize);
            let sprite = bw_dat::Sprite::from_ptr(ptr)?;
            let image = sprite.images().next()?;
            Some((*sprite, *image))
        }).next().unwrap_or((null_mut(), null_mut()));
        SAVE_SPRITE_ARRAY.store(sprite as usize, Ordering::Relaxed);
        SAVE_IMAGE_ARRAY.store(image as usize, Ordering::Relaxed);
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub struct SerializableSprite(pub *mut bw::Sprite);

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub struct SerializableImage(pub *mut bw::Image);

static SAVE_SPRITE_ARRAY: AtomicUsize = AtomicUsize::new(0);
static SAVE_IMAGE_ARRAY: AtomicUsize = AtomicUsize::new(0);

impl serde::Serialize for SerializableSprite {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let offset = (self.0 as usize).wrapping_sub(SAVE_SPRITE_ARRAY.load(Ordering::Relaxed));
        let index = offset as isize / 0x28;
        (index as u32).serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for SerializableSprite {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        let index = u32::deserialize(deserializer)?;
        let offset = index as i32 as isize * 0x28;
        let ptr = (SAVE_SPRITE_ARRAY.load(Ordering::Relaxed) as usize)
            .wrapping_add(offset as usize) as *mut bw::Sprite;
        Ok(SerializableSprite(ptr))
    }
}

impl serde::Serialize for SerializableImage {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let offset = (self.0 as usize).wrapping_sub(SAVE_IMAGE_ARRAY.load(Ordering::Relaxed));
        let index = offset as isize / 0x40;
        (index as u32).serialize(serializer)
    }
}

impl<'de> serde::Deserialize<'de> for SerializableImage {
    fn deserialize<S: Deserializer<'de>>(deserializer: S) -> Result<Self, S::Error> {
        let index = u32::deserialize(deserializer)?;
        let offset = index as i32 as isize * 0x40;
        let ptr = (SAVE_IMAGE_ARRAY.load(Ordering::Relaxed) as usize)
            .wrapping_add(offset as usize) as *mut bw::Image;
        Ok(SerializableImage(ptr))
    }
}
