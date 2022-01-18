#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]

use crate::samase;

use bw_dat::{OrderId, UnitId};

pub use bw_dat::structs::*;

pub fn orders_dat() -> &'static [DatTable] {
    unsafe {
        let dat = samase::orders_dat() as *const DatTable;
        std::slice::from_raw_parts(dat, 0x10)
    }
}

pub fn game() -> *mut Game {
    samase::game()
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

pub fn give_ai(unit: bw_dat::Unit) {
    unsafe { samase::give_ai(*unit) }
}

pub unsafe fn issue_order(
    unit: *mut Unit,
    order: OrderId,
    pos: Point,
    target: *mut Unit,
    fow_unit: UnitId,
) {
    samase::issue_order(unit, order, pos.x as u32, pos.y as u32, target, fow_unit)
}

pub fn active_iscript_objects() -> (*mut Unit, *mut Bullet) {
    unsafe {
        let mut buf = [std::ptr::null_mut(); 3];
        samase::active_iscript_objects(buf.as_mut_ptr(), std::ptr::null());
        (buf[1] as *mut Unit, buf[2] as *mut Bullet)
    }
}
