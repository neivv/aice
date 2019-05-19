#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use std::sync::atomic::Ordering;

use crate::samase;

pub use bw_dat::structs::*;

pub fn is_scr() -> bool {
    crate::IS_1161.load(Ordering::Relaxed) == false
}

pub fn pathing() -> *mut Pathing {
    samase::pathing()
}

pub fn players() -> *mut Player {
    samase::players()
}

pub fn get_region(pos: Point) -> Option<u16> {
    let Point {
        x,
        y,
    } = pos;
    unsafe {
        let game = game();
        let bounds = ((*game).map_width_tiles * 32, (*game).map_height_tiles * 32);
        if bounds.0 as i16 <= x || bounds.1 as i16 <= y {
            None
        } else {
            Some(samase::get_region(x as u32, y as u32) as u16)
        }
    }
}

pub fn game() -> *mut Game {
    samase::game()
}

pub fn point_from_rect(rect: Rect) -> Point {
    Point {
        x: rect.left + (rect.right - rect.left) / 2,
        y: rect.top + (rect.bottom - rect.top) / 2,
    }
}

pub fn first_active_unit() -> *mut Unit {
    samase::first_active_unit()
}

pub fn first_hidden_unit() -> *mut Unit {
    samase::first_hidden_unit()
}

// BW algorithm
pub fn distance(a: Point, b: Point) -> u32 {
    let x = (a.x as i32).wrapping_sub(b.x as i32).abs() as u32;
    let y = (a.y as i32).wrapping_sub(b.y as i32).abs() as u32;
    let (greater, lesser) = (x.max(y), x.min(y));
    if greater / 4 > lesser {
        greater
    } else {
        greater * 59 / 64 + lesser * 99 / 256
    }
}

pub fn rect_distance(a: &Rect, b: &Rect) -> u32 {
    let horizontal_overlap = a.left < b.right && a.right > b.left;
    let vertical_overlap = a.top < b.bottom && a.bottom > b.top;
    let x_diff = match horizontal_overlap {
        true => 0,
        false => match a.left < b.left {
            true => b.left - a.right,
            false => a.left - b.right,
        },
    };
    let y_diff = match vertical_overlap {
        true => 0,
        false => match a.top < b.top {
            true => b.top - a.bottom,
            false => a.top - b.bottom,
        },
    };

    distance(
        Point {
            x: 0,
            y: 0,
        },
        Point {
            x: x_diff,
            y: y_diff,
        },
    )
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
