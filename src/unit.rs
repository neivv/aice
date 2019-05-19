use bw_dat::{Unit};

use crate::bw;

pub fn active_units() -> UnitListIter {
    UnitListIter(bw::first_active_unit())
}

pub fn hidden_units() -> UnitListIter {
    UnitListIter(bw::first_hidden_unit())
}

pub struct UnitListIter(*mut bw::Unit);

impl Iterator for UnitListIter {
    type Item = Unit;
    fn next(&mut self) -> Option<Unit> {
        unsafe {
            if self.0.is_null() {
                None
            } else {
                let result = Unit::from_ptr(self.0);
                self.0 = (*self.0).next;
                result
            }
        }
    }
}
