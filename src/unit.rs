use std::ptr::null_mut;

use bw_dat::{Unit, OrderId};

use crate::bw;

pub trait UnitExt {
    fn issue_secondary_order(self, order: OrderId);
}

impl UnitExt for Unit {
    fn issue_secondary_order(self, order: OrderId) {
        unsafe {
            // Don't reset the order if already executing the same order, which
            // seems to be good rule almost always..
            let mut issue = (**self).secondary_order != order.0;
            // However, for training fighters the order will stay permamently active in
            // state 3 / 4 (3 makes carriers heal their interceptors), so it must be allowed
            // to be reissued unless it is currently in training state (2).
            if !issue &&
                order == bw_dat::order::TRAIN_FIGHTER &&
                (**self).secondary_order_state != 2
            {
                issue = true;
            }
            if issue {
                (**self).secondary_order = order.0;
                (**self).secondary_order_state = 0;
                (**self).secondary_order_target = bw::PointAndUnit {
                    pos: bw::Point { x: 0, y: 0 },
                    unit: null_mut(),
                };
            }
        }
    }
}
