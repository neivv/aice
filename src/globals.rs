use std::ptr::null_mut;
use std::slice;
use std::sync::atomic::{AtomicUsize, Ordering};

use once_cell::sync::OnceCell;
use serde_derive::{Serialize, Deserialize};
use serde::{Serializer, Deserializer};

use bw_dat::Game;

use crate::bw;
use crate::iscript;
use crate::recurse_checked_mutex::{Mutex, MutexGuard};

static GLOBALS: OnceCell<Mutex<Globals>> = OnceCell::new();
static COLOR_CHOICES: Mutex<PreRandomizationColors> =
    Mutex::new(PreRandomizationColors([(0, 0); 8]));

#[derive(Serialize, Deserialize)]
pub struct Globals {
    pub iscript_state: iscript::IscriptState,
    /// This is reset on game init, so copy it as global
    pub player_lobby_color_choices: PlayerColorChoices,
}

/// (color, storm id).
/// Index by player id but gets obsolete on game start randomization
#[derive(Copy, Clone)]
struct PreRandomizationColors([(u8, u32); 8]);

/// Index by player id.
/// Storm id dropped, may not be reliable after loading save?
#[derive(Serialize, Deserialize)]
pub struct PlayerColorChoices([u8; 8]);

impl PlayerColorChoices {
    fn dummy() -> PlayerColorChoices {
        PlayerColorChoices([0x16; 8])
    }

    fn init(
        input: &PreRandomizationColors,
        players: *mut bw::Player,
    ) -> PlayerColorChoices {
        unsafe {
            // TODO: Ai players.
            // Since they don't have anything that they can be uniquely identified
            // before randomization, they should just check player_color_rgba
            // and use that to match with picked colors.
            let mut fixed = [0x16u8; 8];
            for &(choice, storm_id) in input.0.iter() {
                if storm_id != !0 {
                    if let Some(player_id) =
                        (0..8).find(|&x| (*players.add(x)).storm_id == storm_id)
                    {
                        fixed[player_id] = choice;
                    }
                }
            }
            PlayerColorChoices(fixed)
        }
    }

    pub fn get(&self, player: u8) -> u8 {
        self.0
            .get(player as usize)
            .copied()
            .unwrap_or(0x16)
    }
}

impl Globals {
    pub fn init() {
        GLOBALS.get_or_init(|| Mutex::new(Globals::new()));
    }

    fn new() -> Globals {
        Globals {
            iscript_state: iscript::IscriptState::default(),
            player_lobby_color_choices: PlayerColorChoices::dummy(),
        }
    }

    // Should only be called on hook start to prevent deadlocks.
    #[inline(never)]
    pub fn get(caller: &'static str) -> MutexGuard<'static, Globals> {
        GLOBALS.get_or_init(|| Mutex::new(Globals::new())).lock(caller)
    }
}

/// Since lobby map preview uses CreateUnit which runs a frame of iscript ._.
/// TODO make lobby preview jsut use a dummy script?
pub unsafe fn init_for_lobby_map_preview() -> crate::parse::Iscript {
    let mut globals = Globals::new();
    let iscript = iscript::load_iscript(true);
    globals.iscript_state = iscript::IscriptState::from_script(&iscript);
    *Globals::get("init") = globals;
    bw::init_game_start_vars();
    iscript
}

pub unsafe extern fn init_game() {
    let players = crate::samase::players();
    let mut globals = Globals::new();
    let iscript = iscript::load_iscript(true);
    globals.iscript_state = iscript::IscriptState::from_script(&iscript);
    iscript::set_as_bw_script(iscript);
    let colors = COLOR_CHOICES.lock("init").clone();
    globals.player_lobby_color_choices = PlayerColorChoices::init(&colors, players);
    *Globals::get("init") = globals;
    iscript::rebuild_sprite_owners();
    bw::init_game_start_vars();
}

pub unsafe extern fn on_game_loop() {
    let game = Game::from_ptr(bw::game());
    let players = crate::samase::players();
    let mut arr = [(0, 0); 8];
    for i in 0..8 {
        let choice = (**game).scr_player_color_preference[i];
        let storm_id = (*players.add(i)).storm_id;
        arr[i] = (choice, storm_id);
    }
    *COLOR_CHOICES.lock("game loop") = PreRandomizationColors(arr);
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

unsafe impl Send for SerializableSprite {}
unsafe impl Sync for SerializableSprite {}
unsafe impl Send for SerializableImage {}
unsafe impl Sync for SerializableImage {}

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
