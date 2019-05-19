use std::fmt;
use std::mem;
use std::slice;

use bw_dat::{unit, OrderId, UnitId};

use crate::aiscript::Script;
use crate::bw;
use crate::game::Game;
use crate::rng::Rng;
use crate::unit::Unit;

// For reading aiscript.bin or bwscript.bin (or some other?) bytes
pub struct ScriptData {
    start: *const u8,
    pos: *const u8,
    script: *mut bw::AiScript,
}

impl ScriptData {
    pub unsafe fn new(script: *mut bw::AiScript) -> ScriptData {
        let script_bytes = match (*script).flags & 0x1 != 0 {
            false => bw::aiscript_bin(),
            true => bw::bwscript_bin(),
        };
        ScriptData {
            start: script_bytes,
            pos: script_bytes.offset((*script).pos as isize) as *const u8,
            script,
        }
    }

    pub unsafe fn read_player_match(&mut self, game: Game) -> PlayerMatch {
        let mut cont = true;
        let mut result = PlayerMatch {
            players: [false; 12],
        };
        let current_player = (*self.script).player as u8;
        while cont {
            let byte = self.read_u8();
            cont = byte & 0x80 != 0;
            let player = byte & 0x7f;
            match player {
                x @ 0...11 => result.players[x as usize] = true,
                13 => result.players[current_player as usize] = true,
                // Foes, allies
                14 | 15 => {
                    let allies = player == 15;
                    let players = (0..12)
                        .filter(|&x| x != current_player)
                        .filter(|&x| game.allied(current_player, x) == allies);
                    for player in players {
                        result.players[player as usize] = true;
                    }
                }
                // All players
                17 => result.players = [true; 12],
                // Forces
                18 | 19 | 20 | 21 => {
                    // Forces are 1-based
                    let force = 1 + player - 18;
                    for player in (0..8).filter(|&x| (*game.0).player_forces[x as usize] == force) {
                        result.players[player as usize] = true;
                    }
                }
                x => {
                    bw_print!("Unsupported player: {:x}", x);
                }
            };
        }
        result
    }

    pub fn read_bool_modifier(&mut self) -> BoolModifier {
        let val = self.read_u8();
        let action = match val >> 1 {
            0x20 => ModifierAction::Wait,
            0x40 => ModifierAction::Call,
            0x0 => ModifierAction::Jump,
            _ => {
                bw_print!("Unsupported modifier: {:x}", val);
                ModifierAction::Jump
            }
        };

        BoolModifier {
            action,
            value: val & 1 != 0,
        }
    }

    pub fn read_modifier(&mut self) -> TriggerModifier {
        let val = self.read_u8();
        let action = match val & 0xc0 {
            0x40 => ModifierAction::Wait,
            0x80 => ModifierAction::Call,
            0x0 => ModifierAction::Jump,
            _ => {
                bw_print!("Unsupported modifier: {:x}", val);
                ModifierAction::Jump
            }
        };
        let read_modifier = |ty, action| ReadModifier {
            ty,
            action,
        };
        TriggerModifier {
            ty: match val & 0x1f {
                // Matching triggers in chk
                0 => ModifierType::Read(read_modifier(ReadModifierType::AtLeast, action)),
                1 => ModifierType::Read(read_modifier(ReadModifierType::AtMost, action)),
                7 => ModifierType::Write(WriteModifier::Set),
                8 => ModifierType::Write(WriteModifier::Add),
                9 => ModifierType::Write(WriteModifier::Subtract),
                10 => ModifierType::Read(read_modifier(ReadModifierType::Exactly, action)),
                11 => ModifierType::Write(WriteModifier::Randomize),
                x => {
                    bw_print!("Unsupported modifier: {:x}", x);
                    ModifierType::Read(read_modifier(ReadModifierType::AtLeast, action))
                }
            },
        }
    }

    pub fn read_unit_match(&mut self) -> UnitMatch {
        let val = self.read_u16();
        if val > 0xff00 {
            let repeat = val & 0xff;
            let units = (0..repeat).map(|_| UnitId(self.read_u16())).collect();
            UnitMatch {
                units,
            }
        } else if val < 0x1000 {
            UnitMatch {
                units: vec![UnitId(val)],
            }
        } else {
            bw_print!("Invalid script encoding: unit match {:x}", val);
            UnitMatch {
                units: vec![],
            }
        }
    }

    pub fn read_position(&mut self) -> Position {
        let x = self.read_u16();
        let y = self.read_u16();
        if x == !0 {
            assert!(y < 255);
            let location = if y >= 255 {
                bw_print!("Invalid location id 0x{:x} used", y);
                bw::location(63)
            } else {
                bw::location(y as u8)
            };
            Position::from_rect32(&location.area)
        } else {
            // !1, !1 is used for inherit position in create_script
            Position::from_point(x as i16, y as i16)
        }
    }

    pub fn read_jump_pos(&mut self) -> u32 {
        let long_jumps = unsafe { (self.start as *const u32).read_unaligned() >= 0x10000 };
        if long_jumps {
            self.read_u32()
        } else {
            self.read_u16().into()
        }
    }

    pub fn read_u8(&mut self) -> u8 {
        self.read()
    }

    pub fn read_u16(&mut self) -> u16 {
        self.read()
    }

    pub fn read_u32(&mut self) -> u32 {
        self.read()
    }

    // Maybe not a good idea to inline(never) but saves 1k in binary size
    #[inline(never)]
    pub fn read<T: Copy>(&mut self) -> T {
        unsafe {
            let size = mem::size_of::<T>();
            let val = (self.pos as *const T).read_unaligned();
            self.pos = self.pos.add(size);
            (*self.script).pos = (*self.script).pos.wrapping_add(size as u32);
            val
        }
    }

    pub unsafe fn read_string(&mut self) -> &'static [u8] {
        let length = (0usize..).position(|x| *self.pos.add(x) == 0).unwrap_or(0);
        let val = slice::from_raw_parts(self.pos, length);
        self.pos = self.pos.add(length + 1);
        (*self.script).pos += length as u32 + 1;
        val
    }

    pub unsafe fn attack_to(&mut self) -> AttackTo {
        AttackTo {
            grouping: self.read_position(),
            target: self.read_position(),
        }
    }

    pub unsafe fn attack_timeout(&mut self) -> AttackTimeout {
        AttackTimeout {
            timeout: self.read_u32(),
        }
    }

    pub unsafe fn issue_order(&mut self) -> IssueOrder {
        IssueOrder {
            order: OrderId(self.read_u8()),
            limit: self.read_u16(),
            unit_id: self.read_unit_match(),
            source_area: {
                let mut pos = self.read_position();
                pos.extend_area(self.read_u16() as i16);
                pos
            },
            target_area: {
                let mut pos = self.read_position();
                pos.extend_area(self.read_u16() as i16);
                pos
            },
            target_units: self.read_unit_match(),
            flags: self.read_u16(),
        }
    }
}

#[derive(Eq, PartialEq)]
pub struct Position {
    pub center: bw::Point,
    pub area: bw::Rect,
}

impl Position {
    pub fn from_point(x: i16, y: i16) -> Position {
        Position {
            center: bw::Point {
                x,
                y,
            },
            area: bw::Rect {
                left: x,
                right: x.saturating_add(1),
                top: y,
                bottom: y.saturating_add(1),
            },
        }
    }

    pub fn from_rect32(rect: &bw::Rect32) -> Position {
        Position {
            center: bw::Point {
                x: (rect.left + (rect.right - rect.left) / 2) as i16,
                y: (rect.top + (rect.bottom - rect.top) / 2) as i16,
            },
            area: bw::Rect {
                left: rect.left as i16,
                right: rect.right as i16,
                top: rect.top as i16,
                bottom: rect.bottom as i16,
            },
        }
    }

    pub fn extend_area(&mut self, amt: i16) {
        self.area.left = self.area.left.saturating_sub(amt);
        self.area.right = self.area.right.saturating_add(amt);
        self.area.top = self.area.top.saturating_sub(amt);
        self.area.bottom = self.area.bottom.saturating_add(amt);
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let bw::Rect {
            left,
            right,
            top,
            bottom,
        } = self.area;

        if left == right - 1 && top == bottom - 1 {
            write!(f, "{}, {}", left, right)
        } else {
            write!(f, "{}, {}, {}, {}", left, top, right, bottom)
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct UnitMatch {
    units: Vec<UnitId>,
}

impl UnitMatch {
    pub fn iter_flatten_groups<'a>(&'a mut self) -> impl Iterator<Item = UnitId> + 'a {
        // Ineffective but w/e, simpler than ignoring duplicates
        if self.units.iter().any(|&x| x == unit::ANY_UNIT) {
            self.units = (0..unit::NONE.0).map(UnitId).collect();
        } else {
            let mut group_flags = 0;
            for &id in &self.units {
                group_flags |= match id {
                    unit::GROUP_MEN => 0x8,
                    unit::GROUP_BUILDINGS => 0x10,
                    unit::GROUP_FACTORIES => 0x20,
                    _ => 0x0,
                };
            }
            if group_flags != 0 {
                self.units.retain(|&unit_id| match unit_id {
                    x if x.0 >= unit::NONE.0 => false,
                    x => x.group_flags() & group_flags == 0,
                });
                let new_units = (0..unit::NONE.0)
                    .map(UnitId)
                    .filter(|x| x.group_flags() & group_flags != 0);
                self.units.extend(new_units);
            }
        }
        self.units.iter().cloned()
    }

    pub fn matches(&self, unit: &Unit) -> bool {
        self.units.iter().any(|&x| unit.matches_id(x))
    }

    pub fn get_one(&self) -> UnitId {
        self.units
            .iter()
            .cloned()
            .filter(|x| x.0 < unit::NONE.0)
            .next()
            .unwrap_or(UnitId(0))
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Copy, Serialize, Deserialize)]
pub struct PlayerMatch {
    players: [bool; 12],
}

impl PlayerMatch {
    pub fn matches(&self, player: u8) -> bool {
        self.players.get(player as usize).cloned().unwrap_or(false)
    }

    pub fn players<'a>(&'a self) -> impl Iterator<Item = u8> + 'a {
        self.players
            .iter()
            .enumerate()
            .filter(|x| *x.1 == true)
            .map(|x| x.0 as u8)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub enum ReadModifierType {
    AtLeast,
    AtMost,
    Exactly,
}

impl ReadModifierType {
    pub fn compare(&self, value: u32, constant: u32) -> bool {
        match self {
            ReadModifierType::AtLeast => value >= constant,
            ReadModifierType::AtMost => value <= constant,
            ReadModifierType::Exactly => value == constant,
        }
    }
}

impl ReadModifier {
    pub fn compare(&self, value: u32, constant: u32) -> bool {
        self.ty.compare(value, constant)
    }

    pub unsafe fn compare_and_act(
        &self,
        value: u32,
        constant: u32,
        script: *mut bw::AiScript,
        dest: u32,
        old_pos: u32,
    ) {
        let read_req = self.action.get_read_req();
        if self.ty.compare(value, constant) == read_req {
            self.action.do_action(script, dest, old_pos);
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub enum WriteModifier {
    Set,
    Add,
    Subtract,
    Randomize,
}

impl WriteModifier {
    pub fn apply(self, old: u32, operand: u32, rng: &mut Rng) -> u32 {
        match self {
            WriteModifier::Add => old.saturating_add(operand),
            WriteModifier::Subtract => old.saturating_sub(operand),
            WriteModifier::Set => operand,
            WriteModifier::Randomize => {
                if operand != 0 {
                    rng.synced_rand(0..operand)
                } else {
                    bw_print!("Cannot randomize with 0 cases");
                    !0
                }
            }
        }
    }
}

pub struct ReadModifier {
    pub ty: ReadModifierType,
    pub action: ModifierAction,
}

pub enum ModifierType {
    Read(ReadModifier),
    Write(WriteModifier),
}

pub struct TriggerModifier {
    pub ty: ModifierType,
}

#[derive(Copy, Clone, Debug)]
pub enum ModifierAction {
    Jump,
    Call,
    Wait,
}

impl ModifierAction {
    /// Whether the action should be done on comparision being false or true.
    pub fn get_read_req(&self) -> bool {
        match self {
            ModifierAction::Jump | ModifierAction::Call => true,
            ModifierAction::Wait => false,
        }
    }

    pub unsafe fn do_action(&self, script: *mut bw::AiScript, dest: u32, old_pos: u32) {
        match self {
            ModifierAction::Jump => {
                (*script).pos = dest;
            }
            ModifierAction::Call => {
                let ret = (*script).pos;
                (*script).pos = dest;
                (*Script::ptr_from_bw(script)).call_stack.push(ret);
            }
            ModifierAction::Wait => {
                (*script).pos = old_pos;
                (*script).wait = 30;
            }
        }
    }
}

pub struct BoolModifier {
    pub value: bool,
    pub action: ModifierAction,
}

pub struct AttackTo {
    pub grouping: Position,
    pub target: Position,
}

pub struct AttackTimeout {
    pub timeout: u32,
}

pub struct IssueOrder {
    pub order: OrderId,
    pub limit: u16,
    pub unit_id: UnitMatch,
    pub source_area: Position,
    pub target_area: Position,
    pub target_units: UnitMatch,
    pub flags: u16,
}

#[cfg(test)]
mod test {
    #[test]
    fn script_data_reading() {
        use byteorder::{WriteBytesExt, LE};

        let mut buf = vec![];
        buf.write_u32::<LE>(0x123).unwrap();
        buf.write_u32::<LE>(43242).unwrap();
        buf.write_u16::<LE>(12345).unwrap();
        for &c in b"test test \0".iter() {
            buf.push(c);
        }
        buf.write_u16::<LE>(941).unwrap();
        unsafe {
            let mut script: bw::AiScript = mem::zeroed();
            script.pos = 4;
            let mut read = ScriptData {
                start: buf.as_ptr(),
                pos: buf.as_ptr().add(4),
                script: &mut script,
            };
            assert_eq!(read.read_u32(), 43242);
            assert_eq!(read.read_u16(), 12345);
            assert_eq!(read.read_string(), b"test test ");
            assert_eq!(read.read_u16(), 941);
            assert_eq!(script.pos, buf.len() as u32);
        }
    }

    #[test]
    fn script_data_unit_match() {
        use byteorder::{WriteBytesExt, LE};

        let mut buf = vec![];
        buf.write_u32::<LE>(0x123).unwrap();
        buf.write_u16::<LE>(0x33).unwrap();
        buf.write_u16::<LE>(0xff04).unwrap();
        buf.write_u16::<LE>(0x123).unwrap();
        buf.write_u16::<LE>(0x110).unwrap();
        buf.write_u16::<LE>(0x30).unwrap();
        buf.write_u16::<LE>(0x70).unwrap();
        unsafe {
            let mut script: bw::AiScript = mem::zeroed();
            script.pos = 4;
            let mut read = ScriptData {
                start: buf.as_ptr(),
                pos: buf.as_ptr().add(4),
                script: &mut script,
            };
            assert_eq!(read.read_unit_match().units, vec![UnitId(0x33)]);
            let eq = vec![UnitId(0x123), UnitId(0x110), UnitId(0x30), UnitId(0x70)];
            assert_eq!(read.read_unit_match().units, eq);
            assert_eq!(script.pos, buf.len() as u32);
        }
    }

    #[test]
    fn script_data_long_jumps() {
        use byteorder::{WriteBytesExt, LE};

        let mut buf = vec![];
        buf.write_u32::<LE>(0x123456).unwrap();
        buf.write_u32::<LE>(0x12345678).unwrap();
        buf.write_u32::<LE>(0x33113322).unwrap();
        unsafe {
            let mut script: bw::AiScript = mem::zeroed();
            script.pos = 4;
            let mut read = ScriptData {
                start: buf.as_ptr(),
                pos: buf.as_ptr().add(4),
                script: &mut script,
            };
            assert_eq!(read.read_jump_pos(), 0x12345678);
            assert_eq!(read.read_jump_pos(), 0x33113322);
            assert_eq!(script.pos, buf.len() as u32);
        }
    }

    #[test]
    fn script_data_short_jumps() {
        use byteorder::{WriteBytesExt, LE};

        let mut buf = vec![];
        buf.write_u32::<LE>(0x1234).unwrap();
        buf.write_u32::<LE>(0x12345678).unwrap();
        buf.write_u32::<LE>(0x33113322).unwrap();
        unsafe {
            let mut script: bw::AiScript = mem::zeroed();
            script.pos = 4;
            let mut read = ScriptData {
                start: buf.as_ptr(),
                pos: buf.as_ptr().add(4),
                script: &mut script,
            };
            assert_eq!(read.read_jump_pos(), 0x5678);
            assert_eq!(read.read_jump_pos(), 0x1234);
            assert_eq!(read.read_jump_pos(), 0x3322);
            assert_eq!(read.read_jump_pos(), 0x3311);
            assert_eq!(script.pos, buf.len() as u32);
        }
    }
}
