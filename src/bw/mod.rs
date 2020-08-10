#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use crate::samase;

use bw_dat::{UnitId};

pub use bw_dat::structs::*;

pub fn orders_dat() -> &'static [DatTable] {
    unsafe {
        let dat = samase::orders_dat() as *const DatTable;
        std::slice::from_raw_parts(dat, 0x10)
    }
}

pub fn players() -> *mut Player {
    samase::players()
}

pub fn game() -> *mut Game {
    samase::game()
}

pub fn first_active_unit() -> *mut Unit {
    samase::first_active_unit()
}

pub fn first_hidden_unit() -> *mut Unit {
    samase::first_hidden_unit()
}

pub fn first_active_bullet() -> *mut Bullet {
    samase::first_active_bullet()
}

pub fn first_lone_sprite() -> *mut LoneSprite {
    samase::first_lone_sprite()
}

pub fn first_fow_sprite() -> *mut LoneSprite {
    samase::first_fow_sprite()
}

pub fn create_unit(id: UnitId, pos: &Point, player: u8) -> Option<bw_dat::Unit> {
    unsafe {
        let skins = [player, player];
        let result = samase::create_unit(
            id.0 as u32,
            pos.x as i32,
            pos.y as i32,
            player as u32,
            skins.as_ptr(),
        );
        bw_dat::Unit::from_ptr(result)
    }
}

pub fn finish_unit_pre(unit: bw_dat::Unit) {
    unsafe { samase::finish_unit_pre(*unit) }
}

pub fn finish_unit_post(unit: bw_dat::Unit) {
    unsafe { samase::finish_unit_post(*unit) }
}

whack_hooks!(stdcall, 0x00400000,
    0x00488AF0 => increment_death_scores(@edi *mut Unit, @edx u8);
    0x004465C0 => choose_placement_position(u32, u32, *mut Point, u32, @ecx *mut Unit) -> u32;
    0x00473FB0 => update_building_placement_state_hook(*mut Unit, u8, u32, u32, u16, u8, u8, u8, u8) -> u32;
    0x0047B090 => get_unit_name(@ecx u32) -> *const u8;
    0x00447980 => add_spending_request(u32,*mut libc::c_void,@eax u16, @ecx u32, @edx u8);
);

whack_funcs!(stdcall, init_funcs, 0x00400000,
    0x00473FB0 => update_building_placement_state(*mut Unit, u8, u32, u32, u16, u8, u8, u8, u8) -> u32;
    0x004936B0 => is_powered(u32, u32, u8, @eax u32) -> u32;
    0x004A34C0 => ping_minimap(u32, u32, u8);
);

whack_vars!(init_vars, 0x00400000,
    0x0057EE9C => player_name: [u8; 0x19];
    0x0057F0B4 => is_multiplayer: u8;
    0x006D1260 => tile_flags: *mut u32;
    0x00597208 => client_selection: [*mut Unit; 0xc];
);
