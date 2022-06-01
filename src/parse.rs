//! Notes on iscript encoding:
//!
//! Every point at which BW-handled commands switch to aice commands, is marked with opcode
//! 0xcc in iscript. Aice uses a BW offset -> Aice offset map so it does not need any other
//! information than 0xcc to let BW transfer command via iscript hook.
//!
//! Every iscript id is compiled to have all animations pointing to a AnimationInit instruction.
//! Aice is able to determine which part to start executing from by looking at header offset
//! and animation id in bw::Image.
//!
//! To make things really dumb, each header is just compiled to
//! `u32 0x4550_4353 (magic); u32 0x1b (animation count)` without actually allocating any
//! space for animations (As 1000 iscript ids with 0x1c animations would be almsot past size limit
//! alread). Instead, it is just made sure that offset 0x00 (Pointer to header list) has 0xcc
//! in low byte, and that offsets 0x4550 and 0x4353 are reserved 0xcc AnimationInit. Fun working
//! with that =)

mod anytype_expr;
mod print_format;
mod ref_builder;
#[cfg(test)] mod test;

use std::collections::hash_map::Entry;
use std::convert::TryFrom;
use std::fmt;
use std::mem;
use std::rc::Rc;

use bstr::{ByteSlice, BString, BStr};
use bumpalo::Bump;
use byteorder::{ByteOrder, ReadBytesExt, WriteBytesExt, LittleEndian, LE};
use fxhash::{FxHashMap, FxHashSet};
use quick_error::quick_error;
use serde_derive::{Serialize, Deserialize};

use bw_dat::expr::{self, Expr, CustomBoolExpr, CustomIntExpr};

use anytype_expr::{AnyTypeExpr, AnyTypeParser};
use ref_builder::UnitRefBuilder;

pub use anytype_expr::{AnyTypeRef, read_any_type};
pub use print_format::{FormatStrings, FormatStringId};

pub type IntExpr = CustomIntExpr<ExprState>;
pub type IntExprTree = bw_dat::expr::IntExprTree<ExprState>;
pub type BoolExpr = CustomBoolExpr<ExprState>;
pub type BoolExprTree = bw_dat::expr::BoolExprTree<ExprState>;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct ExprState;

impl expr::CustomState for ExprState {
    type IntExt = Int;
    type BoolExt = Bool;
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Int {
    Variable(PlaceId, [Option<Box<IntExpr>>; 4]),
    Default(Box<(IntExpr, IntExpr)>),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Bool {
    HasFlingy,
    HasBullet,
    Has(UnitRefId),
    Default(Box<(BoolExpr, BoolExpr)>),
}

enum SetExpr<'a> {
    Int(IntExpr),
    AnyType(AnyTypeExpr<'a>),
}

struct ExprParser<'a, 'b> {
    variables: &'b CompilerVariables<'a>,
    parser: &'b mut ParserExprs,
}

impl<'b, 'c> expr::CustomParser for ExprParser<'b, 'c> {
    type State = ExprState;
    fn parse_int<'a>(&mut self, input: &'a [u8]) -> Option<(Int, &'a [u8])> {
        let mut out = [None, None, None, None];
        match self.parser.parse_place_expr(input, &mut out, self.variables) {
            Ok((var_id, rest)) => {
                return Some((Int::Variable(var_id, out), rest));
            }
            Err(CanRecoverError::No(e)) => {
                self.parser.set_current_error(e);
            }
            Err(CanRecoverError::Yes(_)) => (),
        }
        // Special case player / speed to flingy.player and flingy.speed
        // so that they're also usable from bullet iscripts
        if input.starts_with(b"player") {
            let rest = expect_token(input, b"player").ok()?;
            const VAR: PlaceId = PlaceId::new_flingy(FlingyVar::Player, UnitRefId::this());
            return Some((Int::Variable(VAR, out), rest));
        }
        if input.starts_with(b"speed") {
            let rest = expect_token(input, b"speed").ok()?;
            const VAR: PlaceId = PlaceId::new_flingy(FlingyVar::Speed, UnitRefId::this());
            return Some((Int::Variable(VAR, out), rest));
        }
        None
    }

    fn parse_operator<'a>(&mut self, input: &'a [u8]) -> Option<(u16, &'a [u8])> {
        let compare = b"default ";
        if input.starts_with(compare) {
            Some((0, &input[compare.len()..]))
        } else {
            None
        }
    }

    fn apply_operator(
        &mut self,
        left: Expr<Self::State>,
        right: Expr<Self::State>,
        _oper: u16,
    ) -> Result<Expr<Self::State>, &'static str> {
        match (left, right) {
            (Expr::Int(a), Expr::Int(b)) => {
                if !is_optional_expr(&a) {
                    return Err("Left side of default must be an optional expression");
                }
                let a = IntExpr::from_tree(a);
                let b = IntExpr::from_tree(b);
                Ok(Expr::Int(IntExprTree::Custom(Int::Default(Box::new((a, b))))))
            }
            (Expr::Bool(a), Expr::Bool(b)) => {
                if !is_optional_expr(&a) {
                    return Err("Left side of default must be an optional expression");
                }
                let a = BoolExpr::from_tree(a);
                let b = BoolExpr::from_tree(b);
                Ok(Expr::Bool(BoolExprTree::Custom(Bool::Default(Box::new((a, b))))))
            }
            _ => {
                Err("Default expression has different left/right side types")
            }
        }
    }

    fn parse_bool<'a>(&mut self, input: &'a [u8]) -> Option<(Bool, &'a [u8])> {
        let (word, rest) = split_first_token(input)?;
        if word == b"has" {
            let result = expect_token(rest, b"(")
                .and_then(|rest| {
                    let (ref_id, rest) = self.parser.unit_refs.parse(rest)?;
                    let rest = expect_token(rest, b")")?;
                    Ok((Bool::Has(ref_id), rest))
                });
            match result {
                Ok(o) => Some(o),
                Err(e) => {
                    self.parser.set_current_error(e);
                    None
                }
            }
        } else if word == b"sprite" {
            let rest = expect_token(rest, b".").ok()?;
            let (next, rest) = split_first_token(rest)?;
            if next == b"has_flingy" {
                Some((Bool::HasFlingy, rest))
            } else if word == b"has_unit" {
                let ref_id = UnitRefId::this();
                Some((Bool::Has(ref_id), rest))
            } else if word == b"has_bullet" {
                Some((Bool::HasBullet, rest))
            } else {
                None
            }
        } else {
            None
        }
    }
}

fn is_optional_expr<T: ExprTree>(t: &T) -> bool {
    t.walk(&mut |part| {
        match part {
            IntOrBool::Int(i) => match i {
                IntExprTree::Custom(Int::Default(pair)) => {
                    if is_optional_expr(pair.1.inner()) {
                        WalkResult::Stop(true)
                    } else {
                        WalkResult::Skip
                    }
                }
                IntExprTree::Custom(Int::Variable(place, _)) => {
                    if place.is_optional_expr() {
                        WalkResult::Stop(true)
                    } else {
                        WalkResult::Continue
                    }
                }
                _ => WalkResult::Continue,
            },
            IntOrBool::Bool(i) => match i {
                BoolExprTree::Custom(Bool::Default(pair)) => {
                    if is_optional_expr(pair.1.inner()) {
                        WalkResult::Stop(true)
                    } else {
                        WalkResult::Skip
                    }
                }
                _ => WalkResult::Continue,
            },
        }
    }).unwrap_or(false)
}

trait ExprTree {
    fn walk<F: FnMut(IntOrBool) -> WalkResult<R>, R>(&self, func: &mut F) -> Option<R>;
}

enum WalkResult<R> {
    Continue,
    Skip,
    Stop(R),
}

enum IntOrBool<'a> {
    Int(&'a IntExprTree),
    Bool(&'a BoolExprTree),
}

impl ExprTree for IntExprTree {
    fn walk<F: FnMut(IntOrBool) -> WalkResult<R>, R>(&self, cb: &mut F) -> Option<R> {
        let result = cb(IntOrBool::Int(self));
        if let WalkResult::Skip = result {
            return None;
        }
        if let WalkResult::Stop(s) = result {
            return Some(s);
        }
        match *self {
            IntExprTree::Add(ref pair) | IntExprTree::Sub(ref pair) |
                IntExprTree::Mul(ref pair) | IntExprTree::Div(ref pair) |
                IntExprTree::Modulo(ref pair) | IntExprTree::BitXor(ref pair) |
                IntExprTree::BitAnd(ref pair) | IntExprTree::BitOr(ref pair) |
                IntExprTree::LeftShift(ref pair) | IntExprTree::RightShift(ref pair) =>
            {
                for op in &[&pair.0, &pair.1] {
                    if let Some(s) = op.walk(cb) {
                        return Some(s);
                    }
                }
            }
            IntExprTree::Not(ref expr) => {
                if let Some(s) = expr.walk(cb) {
                    return Some(s);
                }
            }
            IntExprTree::Func(ref fun) => {
                for arg in fun.args.iter() {
                    if let Some(s) = arg.walk(cb) {
                        return Some(s);
                    }
                }
            }
            IntExprTree::Custom(Int::Variable(_, ref args)) => {
                for arg in args {
                    if let Some(ref arg) = arg {
                        if let Some(s) = arg.inner().walk(cb) {
                            return Some(s);
                        }
                    }
                }
            }
            IntExprTree::Custom(Int::Default(ref pair)) => {
                for op in &[&pair.0, &pair.1] {
                    if let Some(s) = op.inner().walk(cb) {
                        return Some(s);
                    }
                }
            }
            IntExprTree::Integer(_) => (),
        }
        None
    }
}

impl ExprTree for BoolExprTree {
    fn walk<F: FnMut(IntOrBool) -> WalkResult<R>, R>(&self, cb: &mut F) -> Option<R> {
        let result = cb(IntOrBool::Bool(self));
        if let WalkResult::Skip = result {
            return None;
        }
        if let WalkResult::Stop(s) = result {
            return Some(s);
        }
        match *self {
            BoolExprTree::And(ref pair) | BoolExprTree::Or(ref pair) |
                BoolExprTree::EqualBool(ref pair) =>
            {
                for op in &[&pair.0, &pair.1] {
                    if let Some(s) = op.walk(cb) {
                        return Some(s);
                    }
                }
            }
            BoolExprTree::LessThan(ref pair) | BoolExprTree::LessOrEqual(ref pair) |
                BoolExprTree::GreaterThan(ref pair) | BoolExprTree::GreaterOrEqual(ref pair) |
                BoolExprTree::EqualInt(ref pair) =>
            {
                for op in &[&pair.0, &pair.1] {
                    if let Some(s) = op.walk(cb) {
                        return Some(s);
                    }
                }
            }
            BoolExprTree::Not(ref single) => {
                if let Some(s) = single.walk(cb) {
                    return Some(s);
                }
            }
            BoolExprTree::Func(ref fun) => {
                for arg in fun.args.iter() {
                    if let Some(s) = arg.walk(cb) {
                        return Some(s);
                    }
                }
            }
            BoolExprTree::Custom(Bool::HasFlingy) | BoolExprTree::Custom(Bool::Has(..)) |
                BoolExprTree::Custom(Bool::HasBullet) => (),
            BoolExprTree::Custom(Bool::Default(ref pair)) => {
                for op in &[&pair.0, &pair.1] {
                    if let Some(s) = op.inner().walk(cb) {
                        return Some(s);
                    }
                }
            }
        }
        None
    }
}

pub mod aice_op {
    pub const JUMP_TO_BW: u8 = 0x00;
    pub const ANIMATION_START: u8 = 0x01;
    pub const IF: u8 = 0x02;
    pub const PRE_END: u8 = 0x03;
    pub const SET: u8 = 0x04;
    pub const SET_ORDER_WEAPON: u8 = 0x05;
    pub const CLEAR_ATTACKING_FLAG: u8 = 0x06;
    pub const CREATE_UNIT: u8 = 0x07;
    pub const CALL: u8 = 0x08;
    pub const RETURN: u8 = 0x09;
    pub const IF_CALL: u8 = 0x0a;
    pub const PLAY_FRAME: u8 = 0x0b;
    pub const SIGORDER: u8 = 0x0c;
    pub const ORDER_DONE: u8 = 0x0d;
    pub const ISSUE_ORDER: u8 = 0x0e;
    pub const IMG_ON: u8 = 0x0f;
    pub const FIRE_WEAPON: u8 = 0x10;
    pub const PRINT: u8 = 0x11;
    pub const SET_COPY: u8 = 0x12;
}

quick_error! {
    #[derive(Debug)]
    pub enum Error {
        Trailing(msg: BString) {
            display("Trailing characters '{}'", msg)
        }
        UnknownCommand(msg: BString) {
            display("Unknown command '{}'", msg)
        }
        CannotParse(msg: BString, as_what: &'static str) {
            display("Cannot parse '{}' as {}", msg, as_what)
        }
        Dynamic(msg: String) {
            display("{}", msg)
        }
        Msg(msg: &'static str) {
            display("{}", msg)
        }
        Internal(msg: &'static str) {
            display("Internal error; {}", msg)
        }
        Expr(e: bw_dat::expr::Error) {
            from()
            display("Couldn't parse expression: {}", e)
        }
        Overflow {
            display("Integer overflow")
        }
        Expected(text: BString, what: &'static [u8]) {
            display("Expected '{}', got '{}'", what.as_bstr(), text)
        }
    }
}

enum CanRecoverError {
    /// The caller can try parsing something else
    Yes(Error),
    /// The caller should definitely just return this error
    No(Error)
}

impl From<Error> for CanRecoverError {
    fn from(error: Error) -> Self {
        CanRecoverError::No(error)
    }
}

impl From<CanRecoverError> for Error {
    fn from(error: CanRecoverError) -> Self {
        match error {
            CanRecoverError::Yes(error) | CanRecoverError::No(error) => error,
        }
    }
}

impl From<std::num::TryFromIntError> for Error {
    fn from(_: std::num::TryFromIntError) -> Self {
        Error::Overflow
    }
}

pub const AICE_COMMAND: u8 = 0xcc;

static ANIMATION_NAMES: &[(&str, u8)] = &[
    ("Init", 0x00),
    ("Death", 0x01),
    ("GndAttkInit", 0x02),
    ("AirAttkInit", 0x03),
    ("Unused1", 0x04),
    ("GndAttkRpt", 0x05),
    ("AirAttkRpt", 0x06),
    ("CastSpell", 0x07),
    ("GndAttkToIdle", 0x08),
    ("AirAttkToIdle", 0x09),
    ("Unused2", 0x0a),
    ("Walking", 0x0b),
    ("WalkingToIdle", 0x0c),
    ("SpecialState1", 0x0d),
    ("SpecialState2", 0x0e),
    ("AlmostBuilt", 0x0f),
    ("Built", 0x10),
    ("Landing", 0x11),
    ("LiftOff", 0x12),
    ("IsWorking", 0x13),
    ("WorkingToIdle", 0x14),
    ("WarpIn", 0x15),
    ("Unused3", 0x16),
    ("StarEditInit", 0x17),
    ("Disable", 0x18),
    ("Burrow", 0x19),
    ("UnBurrow", 0x1a),
    ("Enable", 0x1b),
];

pub fn animation_name(anim: u8) -> Option<&'static str> {
    ANIMATION_NAMES.get(anim as usize).map(|x| x.0)
}

enum BwCommandParam {
    U8,
    I8,
    U16,
    U16VarLen,
    Label,
}

const fn bw_cmd(byte: u8, params: &'static [BwCommandParam]) -> CommandPrototype {
    CommandPrototype::BwCommand(byte, params, false)
}

const fn bw_cmd_final(byte: u8, params: &'static [BwCommandParam]) -> CommandPrototype {
    CommandPrototype::BwCommand(byte, params, true)
}

static COMMANDS: &[(&[u8], CommandPrototype)] = {
    use CommandPrototype::*;
    use BwCommandParam::*;
    &[
        (b"playfram", PlayFram),
        (b"playframtile", bw_cmd(0x01, &[U16])),
        (b"sethorpos", bw_cmd(0x02, &[I8])),
        (b"setvertpos", bw_cmd(0x03, &[I8])),
        (b"setpos", bw_cmd(0x04, &[I8, I8])),
        (b"wait", bw_cmd(0x05, &[U8])),
        (b"waitrand", bw_cmd(0x06, &[U8, U8])),
        (b"goto", bw_cmd_final(0x07, &[Label])),
        (b"imgol", bw_cmd(0x08, &[U16, I8, I8])),
        (b"imgul", bw_cmd(0x09, &[U16, I8, I8])),
        (b"imgolorig", bw_cmd(0x0a, &[U16])),
        (b"switchul", bw_cmd(0x0b, &[U16])),
        (b"imgoluselo", bw_cmd(0x0d, &[U16, I8, I8])),
        (b"imguluselo", bw_cmd(0x0e, &[U16, I8, I8])),
        (b"sprol", bw_cmd(0x0f, &[U16, I8, I8])),
        (b"highsprol", bw_cmd(0x10, &[U16, I8, I8])),
        (b"lowsprul", bw_cmd(0x11, &[U16, I8, I8])),
        (b"spruluselo", bw_cmd(0x13, &[U16, I8, I8])),
        (b"sprul", bw_cmd(0x14, &[U16, I8, I8])),
        (b"sproluselo", bw_cmd(0x15, &[U16, U8])),
        (b"end", End),
        (b"setflipstate", bw_cmd(0x17, &[U8])),
        (b"playsnd", bw_cmd(0x18, &[U16])),
        (b"playsndrand", bw_cmd(0x19, &[U16VarLen])),
        (b"playsndbtwn", bw_cmd(0x1a, &[U16, U16])),
        (b"domissiledmg", bw_cmd(0x1b, &[])),
        (b"attackmelee", bw_cmd(0x1c, &[U16VarLen])),
        (b"followmaingraphic", bw_cmd(0x1d, &[])),
        (b"randcondjmp", bw_cmd(0x1e, &[U8, Label])),
        (b"turnccwise", bw_cmd(0x1f, &[U8])),
        (b"turncwise", bw_cmd(0x20, &[U8])),
        (b"turn1cwise", bw_cmd(0x21, &[])),
        (b"turnrand", bw_cmd(0x22, &[U8])),
        (b"setspawnframe", bw_cmd(0x23, &[U8])),
        (b"sigorder", Sigorder),
        (b"attackwith", bw_cmd(0x25, &[U8])),
        (b"attack", bw_cmd(0x26, &[])),
        (b"castspell", bw_cmd(0x27, &[])),
        (b"useweapon", bw_cmd(0x28, &[U8])),
        (b"move", bw_cmd(0x29, &[U8])),
        (b"gotorepeatattk", GotoRepeatAttk),
        (b"engframe", bw_cmd(0x2b, &[U8])),
        (b"engset", bw_cmd(0x2c, &[U8])),
        (b"__2d", bw_cmd(0x2d, &[])),
        (b"nobrkcodestart", bw_cmd(0x2e, &[])),
        (b"nobrkcodeend", bw_cmd(0x2f, &[])),
        (b"ignorerest", bw_cmd(0x30, &[])),
        (b"attkshiftproj", bw_cmd(0x31, &[U8])),
        (b"tmprmgraphicstart", bw_cmd(0x32, &[])),
        (b"tmprmgraphicend", bw_cmd(0x33, &[])),
        (b"setfldirect", bw_cmd(0x34, &[U8])),
        (b"call", Call),
        (b"return", Return),
        (b"setflspeed", bw_cmd(0x37, &[U16])),
        (b"creategasoverlays", bw_cmd(0x38, &[U8])),
        (b"pwrupcondjmp", bw_cmd(0x39, &[Label])),
        (b"trgtrangecondjmp", bw_cmd(0x3a, &[U16, Label])),
        (b"trgtarccondjmp", bw_cmd(0x3b, &[U16, U16, Label])),
        (b"curdirectcondjmp", bw_cmd(0x3c, &[U16, U16, Label])),
        (b"imgulnextid", bw_cmd(0x3d, &[I8, I8])),
        (b"liftoffcondjmp", bw_cmd(0x3f, &[Label])),
        (b"warpoverlay", bw_cmd(0x40, &[U16])),
        (b"orderdone", OrderDone),
        (b"grdsprol", bw_cmd(0x42, &[U16, I8, I8])),
        (b"__43", bw_cmd(0x43, &[])),
        (b"dogrddamage", bw_cmd(0x44, &[])),
        (b"if", If),
        (b"set", Set),
        (b"fireweapon", FireWeapon),
        (b"create_unit", CreateUnit),
        (b"issue_order", IssueOrder),
        (b"imgul_on", ImgulOn),
        (b"imgol_on", ImgolOn),
        (b"print", Print),
    ]
};

static UNIT_VARS: &[(&[u8], UnitVar)] = {
    use self::UnitVar::*;
    &[
        (b"death_timer", DeathTimer),
        (b"matrix_timer", MatrixTimer),
        (b"matrix_hitpoints", MatrixHp),
        (b"stim_timer", StimTimer),
        (b"ensnare_timer", EnsnareTimer),
        (b"lockdown_timer", LockdownTimer),
        (b"irradiate_timer", IrradiateTimer),
        (b"stasis_timer", StasisTimer),
        (b"plague_timer", PlagueTimer),
        (b"maelstrom_timer", MaelstormTimer),
        (b"is_blind", IsBlind),
        (b"hitpoints", Hitpoints),
        (b"shields", Shields),
        (b"energy", Energy),
        (b"max_hitpoints", MaxHitpoints),
        (b"max_shields", MaxShields),
        (b"max_energy", MaxEnergy),
        (b"mineral_cost", MineralCost),
        (b"gas_cost", GasCost),
        (b"supply_cost", SupplyCost),
        (b"resources", Resources),
        (b"hangar_count_inside", HangarCountInside),
        (b"hangar_count_outside", HangarCountOutside),
        (b"loaded_count", LoadedCount),
        (b"current_upgrade", CurrentUpgrade),
        (b"current_tech", CurrentTech),
        (b"build_queue", BuildQueue),
        (b"remaining_build_time", RemainingBuildTime),
        (b"remaining_research_time", RemainingResearchTime),
        (b"overlay_size", OverlaySize),
        (b"unit_id", UnitId),
        (b"kills", Kills),
        (b"carried_resource_amount", CarriedResourceAmount),
        (b"ground_cooldown", GroundCooldown),
        (b"air_cooldown", AirCooldown),
        (b"spell_cooldown", SpellCooldown),
        (b"order", Order),
        (b"order_timer", OrderTimer),
        (b"order_state", OrderState),
        (b"rank_increase", RankIncrease),
        (b"mine_amount", MineAmount),
    ]
};

static FLINGY_VARS: &[(&[u8], FlingyVar)] = {
    use self::FlingyVar::*;
    &[
        (b"move_target_x", MoveTargetX),
        (b"move_target_y", MoveTargetY),
        (b"position_x", PositionX),
        (b"position_y", PositionY),
        (b"facing_direction", FacingDirection),
        (b"movement_direction", MovementDirection),
        (b"target_direction", TargetDirection),
        (b"turn_speed", TurnSpeed),
        (b"acceleration", Acceleration),
        (b"top_speed", TopSpeed),
        (b"speed", Speed),
        (b"player", Player),
        (b"flingy_id", FlingyId),
    ]
};

static BULLET_VARS: &[(&[u8], BulletVar)] = {
    use self::BulletVar::*;
    &[
        (b"weapon_id", WeaponId),
        (b"death_timer", BulletVar::DeathTimer),
        (b"state", State),
        (b"bounces_remaining", BouncesRemaining),
        (b"order_target_x", OrderTargetX),
        (b"order_target_y", OrderTargetY),
    ]
};

static IMAGE_VARS: &[(&[u8], ImageVar)] = {
    use self::ImageVar::*;
    &[
        (b"drawfunc", Drawfunc),
        (b"drawfunc_param", DrawfuncParam),
        (b"displayed_frame", Frame),
        (b"frame", BaseFrame),
    ]
};

static GAME_VARS: &[(&[u8], GameVar)] = {
    use self::GameVar::*;
    &[
        (b"deaths", Deaths),
        (b"kills", Kills),
        (b"upgrade_level", UpgradeLevel),
        (b"upgrade_limit", UpgradeLimit),
        (b"tech_level", TechLevel),
        (b"tech_availability", TechAvailability),
        (b"unit_availability", UnitAvailability),
        (b"alliance", Alliance),
        (b"shared_vision", SharedVision),
        (b"minerals", Minerals),
        (b"gas", Gas),
        (b"zerg_supply_max", ZergSupplyMax),
        (b"zerg_supply_used", ZergSupplyUsed),
        (b"zerg_supply_provided", ZergSupplyProvided),
        (b"terran_supply_max", TerranSupplyMax),
        (b"terran_supply_used", TerranSupplyUsed),
        (b"terran_supply_provided", TerranSupplyProvided),
        (b"protoss_supply_max", ProtossSupplyMax),
        (b"protoss_supply_used", ProtossSupplyUsed),
        (b"protoss_supply_provided", ProtossSupplyProvided),
        (b"location", LocationLeft),
        (b"units_total", UnitsTotal),
        (b"units_produced", UnitsProduced),
        (b"units_owned", UnitsOwned),
        (b"units_lost", UnitsLost),
        (b"units_killed", UnitsKilled),
        (b"units_score", UnitsScore),
        (b"units_killed_score", UnitsKilledScore),
        (b"buildings_total", BuildingsTotal),
        (b"buildings_constructed", BuildingsConstructed),
        (b"buildings_owned", BuildingsOwned),
        (b"buildings_lost", BuildingsLost),
        (b"buildings_razed", BuildingsRazed),
        (b"buildings_score", BuildingsScore),
        (b"buildings_razed_score", BuildingsRazedScore),
        (b"factories_constructed", FactoriesConstructed),
        (b"factories_owned", FactoriesOwned),
        (b"factories_lost", FactoriesLost),
        (b"factories_razed", FactoriesRazed),
        (b"custom_score", CustomScore),
        (b"player_color_choice", PlayerColorChoice),
    ]
};

#[derive(Copy, Clone)]
enum CommandPrototype {
    BwCommand(u8, &'static [BwCommandParam], bool),
    If,
    Set,
    End,
    FireWeapon,
    GotoRepeatAttk,
    CreateUnit,
    IssueOrder,
    Call,
    Return,
    PlayFram,
    Sigorder,
    OrderDone,
    ImgulOn,
    ImgolOn,
    Print,
}

pub struct Iscript {
    pub bw_data: Box<[u8]>,
    pub aice_data: Vec<u8>,
    pub bw_aice_cmd_offsets: FxHashMap<u16, u32>,
    pub headers: Vec<ResolvedHeader>,
    pub conditions: Vec<Rc<BoolExpr>>,
    pub int_expressions: Vec<Rc<IntExpr>>,
    pub global_count: u32,
    pub line_info: LineInfo,
    unit_refs: Vec<Vec<UnitObject>>,
    sprite_local_sets: VecOfVecs<SpriteLocalSetId, (u32, u32)>,
    pub format_strings: FormatStrings,
}

pub enum AnyExpr {
    Int(u32),
    Bool(u32),
    UnitRef(UnitRefId),
}

struct VecOfVecs<Key, T> {
    data: Vec<T>,
    offsets_lens: Vec<(u32, u32)>,
    key: std::marker::PhantomData<*const Key>,
}

trait VecIndex: Sized {
    fn index(&self) -> Option<usize>;
    fn from_index(index: u32) -> Option<Self>;
}

#[derive(Copy, Clone, Debug)]
pub enum UnitRefParts<'a> {
    Single(UnitObject),
    Many(&'a [UnitObject]),
}

impl Iscript {
    pub fn unit_ref_object(&self, unit: UnitRefId) -> UnitRefParts<'_> {
        if unit.0 < UnitObject::_Last as u16 {
            UnitRefParts::Single(unsafe { mem::transmute(unit.0 as u8) })
        } else {
            let index = (unit.0 - UnitObject::_Last as u16) as usize;
            UnitRefParts::Many(&self.unit_refs[index])
        }
    }

    /// Returns list of `(spritelocal id, int expr id)`
    pub fn sprite_local_set(&self, id: SpriteLocalSetId) -> &[(u32, u32)] {
        self.sprite_local_sets.get_or_empty(id)
    }

    pub fn is_int_var_place(&self, place: PlaceId) -> bool {
        match place.place() {
            Place::Global(..) => true,
            Place::SpriteLocal(..) => true,
            _ => false,
        }
    }
}

impl<Key: VecIndex, T> VecOfVecs<Key, T> {
    fn new() -> VecOfVecs<Key, T> {
        VecOfVecs {
            data: Vec::new(),
            offsets_lens: Vec::new(),
            key: Default::default(),
        }
    }

    fn get_or_empty(&self, id: Key) -> &[T] {
        if let Some(idx) = id.index() {
            let (offset, len) = self.offsets_lens[idx];
            &self.data[(offset as usize)..][..(len as usize)]
        } else {
            &[]
        }
    }

    fn build<F: FnMut() -> Result<Option<T>, Error>>(&mut self, mut cb: F) -> Result<Key, Error> {
        let start = u32::try_from(self.data.len())?;
        let id = u32::try_from(self.offsets_lens.len())
            .ok()
            .and_then(|x| Key::from_index(x as u32))
            .ok_or_else(|| Error::Overflow)?;
        let mut len = 0u32;
        while let Some(data) = cb()? {
            self.data.push(data);
            len = len.checked_add(1).ok_or_else(|| Error::Overflow)?;
        }
        self.offsets_lens.push((start, len));
        Ok(id)
    }
}

pub struct LineInfo {
    bw: LineInfoLookup,
    aice: LineInfoLookup,
}

const LOOKUP_LINEAR_BLOCK_SIZE: usize = 64;

// Assuming that this struct is only used if the user's script has errors.
// If the pos is found in `sorted`, then that's the result.
// Otherwise will have to search linearly from N * 64 .. (N + 1) * 64
// Note that relative[N * 64] is already the next one from sorted.
// (1 sorted + 64 relative entries = 65 total)
struct LineInfoLookup {
    // Sorted by `pos`
    sorted: Vec<SortedLookupEntry>,
    relative: Vec<RelativeLookupEntry>,
}

#[derive(Debug)]
struct SortedLookupEntry {
    pos: u32,
    line: u32,
}

#[derive(Debug)]
struct RelativeLookupEntry {
    // These are relative to previous MainLookupEntry.
    pos: u8,
    line: i8,
}

struct LineInfoBuilder {
    bw: LineInfoLookupBuilder,
    aice: LineInfoLookupBuilder,
}

struct LineInfoLookupBuilder {
    sorted: Vec<SortedLookupEntry>,
    relative: Vec<RelativeLookupEntry>,
    previous_pos: u32,
    previous_line: u32,
}

impl LineInfo {
    pub fn pos_to_line(&self, pos: CodePosition) -> u32 {
        match pos.to_enum() {
            CodePos::Bw(pos) => self.bw.pos_to_line(pos as u32),
            CodePos::Aice(pos) => self.aice.pos_to_line(pos),
        }
    }
}

impl LineInfoLookup {
    pub fn pos_to_line(&self, pos: u32) -> u32 {
        let index = match self.sorted.binary_search_by_key(&pos, |x| x.pos) {
            Ok(o) => return self.sorted[o].line,
            Err(e) => if e == 0 {
                return 0;
            } else {
                e - 1
            },
        };
        if let Some(sorted) = self.sorted.get(index) {
            let mut current_line = sorted.line as i32;
            let mut current_pos = sorted.pos;
            let start = index.wrapping_mul(LOOKUP_LINEAR_BLOCK_SIZE);
            let end = start.wrapping_add(LOOKUP_LINEAR_BLOCK_SIZE).min(self.relative.len());
            if let Some(relative) = self.relative.get(start..end) {
                for rel in relative {
                    current_line = current_line.wrapping_add(rel.line as i32);
                    current_pos = current_pos.wrapping_add(rel.pos as u32);
                    if current_pos >= pos {
                        return current_line as u32;
                    }
                }
            }
        }
        0
    }

    #[cfg(test)]
    pub fn line_to_pos(&self, line: u32) -> Option<usize> {
        // Note: Lines being sorted is not actually guaranteed, but pretty sure
        // it works -- since this is just test helper it should be fiine...
        let index = match self.sorted.binary_search_by_key(&line, |x| x.line) {
            Ok(o) => return Some(self.sorted[o].pos as usize),
            Err(e) => e.checked_sub(1)?,
        };
        let sorted = self.sorted.get(index)?;

        let mut current_line = sorted.line as i32;
        let mut current_pos = sorted.pos;
        let start = index.wrapping_mul(LOOKUP_LINEAR_BLOCK_SIZE);
        let end = start.wrapping_add(LOOKUP_LINEAR_BLOCK_SIZE).min(self.relative.len());
        if let Some(relative) = self.relative.get(start..end) {
            for rel in relative {
                current_line = current_line.wrapping_add(rel.line as i32);
                current_pos = current_pos.wrapping_add(rel.pos as u32);
                if current_line >= line as i32 {
                    if current_line == line as i32 {
                        return Some(current_pos as usize);
                    } else {
                        return None;
                    }
                }
            }
        }
        None
    }
}

impl LineInfoBuilder {
    pub fn new() -> LineInfoBuilder {
        LineInfoBuilder {
            bw: LineInfoLookupBuilder {
                sorted: Vec::with_capacity(30_000 / LOOKUP_LINEAR_BLOCK_SIZE),
                relative: Vec::with_capacity(30_000),
                previous_pos: 0,
                previous_line: 0,
            },
            aice: LineInfoLookupBuilder {
                sorted: Vec::with_capacity(10_000 / LOOKUP_LINEAR_BLOCK_SIZE),
                relative: Vec::with_capacity(10_000),
                previous_pos: 0,
                previous_line: 0,
            },
        }
    }

    pub fn finish(self) -> LineInfo {
        LineInfo {
            bw: self.bw.finish(),
            aice: self.aice.finish(),
        }
    }
}

impl LineInfoLookupBuilder {
    /// Implicit assertion is that `pos` always increases
    pub fn push(&mut self, pos: u32, line: u32) {
        let relative_len = self.relative.len();
        let sorted_len = self.sorted.len();
        let previous_pos = self.previous_pos;
        let previous_line = self.previous_line;
        self.previous_pos = pos;
        self.previous_line = line;
        if relative_len == sorted_len * LOOKUP_LINEAR_BLOCK_SIZE {
            self.sorted.push(SortedLookupEntry {
                pos,
                line,
            });
        } else {
            let mut relative_pos = pos - previous_pos;
            let mut relative_line = line.wrapping_sub(previous_line) as i32;
            let end = sorted_len * LOOKUP_LINEAR_BLOCK_SIZE;
            // If relative doesn't fit in 8bit, add dummy entries.
            // If the dummy entries end up filling the linear block then
            // stop and add sorted entry.
            while self.relative.len() != end {
                let large_line = relative_line < -128 || relative_line > 127;
                let large_pos = relative_pos > 255;
                let (next_line, next_pos) = match (large_line, large_pos) {
                    (false, false) => break,
                    (true, true) if relative_line < 0 => (-128, 255),
                    (true, true) => (127, 255),
                    (false, true) => (0, 255),
                    (true, false) if relative_line < 0 => (-128, 0),
                    (true, false) => (127, 0),
                };
                self.relative.push(RelativeLookupEntry {
                    line: next_line,
                    pos: next_pos,
                });
                relative_line -= next_line as i32;
                relative_pos -= next_pos as u32;
            }
            if self.relative.len() == end {
                self.sorted.push(SortedLookupEntry {
                    pos,
                    line,
                });
            } else {
                self.relative.push(RelativeLookupEntry {
                    line: relative_line as i8,
                    pos: relative_pos as u8,
                });
            }
        }
    }

    pub fn finish(self) -> LineInfoLookup {
        LineInfoLookup {
            sorted: self.sorted,
            relative: self.relative,
        }
    }
}

pub struct ResolvedHeader {
    pub iscript_id: u16,
    pub animations: Vec<Option<CodePosition>>,
}

struct Header<'a> {
    iscript_id: u32,
    animations: Vec<Option<Label<'a>>>,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
struct Label<'a>(&'a BStr);

struct Parser<'a> {
    parsing_header: Option<Header<'a>>,
    headers: Vec<Header<'a>>,
    // true if the previous command didn't force end execution
    animation_name_to_index: FxHashMap<&'static [u8], u8>,
    command_map: FxHashMap<&'static [u8], CommandPrototype>,
    exprs: ParserExprs,
    format_strings: FormatStrings,
}

struct ParserExprs {
    unit_refs: UnitRefBuilder,
    current_error: Option<Error>,
    // These maps stay constant so maybe they should be in separate ParserConstantData
    unit_vars: FxHashMap<&'static [u8], UnitVar>,
    flingy_vars: FxHashMap<&'static [u8], FlingyVar>,
    game_vars: FxHashMap<&'static [u8], GameVar>,
    bullet_vars: FxHashMap<&'static [u8], BulletVar>,
    image_vars: FxHashMap<&'static [u8], ImageVar>,
}

impl<'a> Parser<'a> {
    fn new() -> Parser<'a> {
        Parser {
            parsing_header: None,
            headers: Vec::with_capacity(1024),
            animation_name_to_index: ANIMATION_NAMES
                .iter()
                .map(|x| (x.0.as_bytes(), x.1))
                .collect(),
            command_map: COMMANDS.iter().cloned().collect(),
            exprs: ParserExprs {
                unit_refs: UnitRefBuilder::new(),
                current_error: None,
                unit_vars: UNIT_VARS.iter().cloned().collect(),
                flingy_vars: FLINGY_VARS.iter().cloned().collect(),
                bullet_vars: BULLET_VARS.iter().cloned().collect(),
                game_vars: GAME_VARS.iter().cloned().collect(),
                image_vars: IMAGE_VARS.iter().cloned().collect(),
            },
            format_strings: FormatStrings::new(),
        }
    }

    /// Separates blocks out of the text, parses headers.
    fn parse_text(
        &mut self,
        text: &'a [u8],
        compiler: &mut Compiler<'a>,
    ) -> Result<ParseStage1<'a>, Vec<ErrorWithLine>> {
        let mut errors = Vec::new();
        let mut ctx = TextParseContext {
            text,
            block_stack: Vec::with_capacity(4),
            line_number: 0,
            error_line_number: 0,
            current_block: (Block {
                lines: Vec::with_capacity(32000),
                parent: BlockId(!0),
                children: Vec::new(),
            }, BlockId(0), 1),
            is_continuing_commands: false,
        };
        let mut stage1 = ParseStage1 {
            blocks: Blocks {
                blocks: vec![Block::dummy()],
                root_blocks: vec![BlockId(0)],
            },
            sprite_local_sets: VecOfVecs::new(),
            variable_types: FxHashMap::with_capacity_and_hasher(32, Default::default()),
        };
        while let Some(line) = ctx.next_line() {
            ctx.error_line_number = ctx.line_number;
            if line == b"}" {
                if let Some((block, index, start_line)) = ctx.block_stack.pop() {
                    stage1.blocks.blocks[(ctx.current_block.1).0 as usize] =
                        mem::replace(&mut ctx.current_block.0, block);
                    ctx.current_block.1 = index;
                    ctx.current_block.2 = start_line;
                } else {
                    errors.push(ErrorWithLine {
                        error: Error::Msg("Unexpected '}'"),
                        line: Some(ctx.line_number),
                    });
                }
            } else {
                if let Err(error) = self.parse_line(line, compiler, &mut stage1, &mut ctx) {
                    errors.push(ErrorWithLine {
                        error,
                        line: Some(ctx.error_line_number),
                    });
                }
            }
        }
        if !ctx.block_stack.is_empty() {
            errors.push(ErrorWithLine {
                error: Error::Msg("Unterminated '{'"),
                line: Some(ctx.current_block.2),
            });
            for &(_, _, line) in ctx.block_stack.iter().skip(1).rev() {
                errors.push(ErrorWithLine {
                    error: Error::Msg("Unterminated '{'"),
                    line: Some(line),
                });
            }
        }
        if ctx.is_continuing_commands {
            errors.push(ErrorWithLine {
                error: Error::Msg("Last command does not terminate script"),
                line: Some(ctx.line_number),
            });
        }
        stage1.blocks.blocks[(ctx.current_block.1).0 as usize] = ctx.current_block.0;
        if errors.is_empty() {
            Ok(stage1)
        } else {
            Err(errors)
        }
    }

    fn parse_line(
        &mut self,
        line: &'a [u8],
        compiler: &mut Compiler<'a>,
        stage1: &mut ParseStage1<'a>,
        ctx: &mut TextParseContext<'a>,
    ) -> Result<(), Error> {
        if line == b".headerend" {
            return if let Some(header) = self.parsing_header.take() {
                if header.iscript_id == !0 {
                    return Err(Error::Msg("Header missing iscript ID"));
                }
                if header.animations.is_empty() {
                    return Err(Error::Msg("Header had no animations"));
                }
                self.headers.push(header);
                Ok(())
            } else {
                Err(Error::Msg("Unexpected .headerend"))
            };
        }
        if let Some(ref mut header) = self.parsing_header {
            match parse_header_opcode(line, &self.animation_name_to_index)? {
                HeaderOpcode::Type => (),
                HeaderOpcode::IscriptId(id) => {
                    header.iscript_id = id as u32;
                }
                HeaderOpcode::Animation(index, label) => {
                    let index = index as usize;
                    if header.animations.get(index).and_then(|x| x.as_ref()).is_some() {
                        return Err(Error::Msg("Duplicate animation"));
                    }
                    if header.animations.len() <= index {
                        header.animations.resize_with(index + 1, || None);
                    }
                    header.animations[index] = Some(label);
                }
            }
            return Ok(());
        }
        if line == b".headerstart" {
            if ctx.is_continuing_commands {
                return Err(Error::Msg("Previous script hasn't terminated"));
            }
            self.parsing_header = Some(Header {
                iscript_id: !0,
                animations: Vec::with_capacity(10),
            });
            Ok(())
        } else {
            self.parse_line_command(line, compiler, stage1, ctx)
        }
    }

    fn parse_line_command(
        &mut self,
        line: &'a [u8],
        compiler: &mut Compiler<'a>,
        stage1: &mut ParseStage1<'a>,
        ctx: &mut TextParseContext<'a>,
    ) -> Result<(), Error> {
        let line_number = ctx.line_number;
        if let Some((label, block_start)) = parse_label(line)? {
            ctx.current_block.0.lines.push(PartiallyParsedLine {
                ty: BlockLine::Label(label),
                line_number,
            });
            if block_start {
                stage1.blocks.start_inline_block(ctx);
            }
            return Ok(());
        }
        if line == b"{" {
            stage1.blocks.start_inline_block(ctx);
            return Ok(());
        }
        let command = line.fields().next().ok_or_else(|| Error::Msg("Empty line???"))?;
        let rest = (&line[command.len()..]).trim_start();
        match self.command_map.get(&command) {
            Some(&command) => {
                let mut linked = LinkedBlock::None;
                ctx.is_continuing_commands = true;
                match command {
                    CommandPrototype::BwCommand(_, commands, ends_flow) => {
                        mark_required_bw_labels(rest, commands, compiler, ctx)?;
                        ctx.is_continuing_commands = !ends_flow;
                    }
                    CommandPrototype::End | CommandPrototype::Return => {
                        ctx.is_continuing_commands = false;
                    }
                    CommandPrototype::Set => {
                        let place = self.exprs.var_name_type_from_set_place(rest)?;
                        if let Some((var_name, ty)) = place {
                            add_set_decl(&mut stage1.variable_types, var_name, ty)?;
                        }
                    }
                    CommandPrototype::CreateUnit | CommandPrototype::FireWeapon => {
                        if ends_with_tokens(rest, &[b"with", b"{"]) {
                            let main_line_no = ctx.error_line_number;
                            let id = self.parse_spritelocal_set(stage1, ctx)?;
                            linked = LinkedBlock::SpriteLocals(id);
                            for &(text, line_no) in stage1.sprite_local_sets.get_or_empty(id) {
                                ctx.error_line_number = line_no;
                                let (name, rest) = split_first_token(text)
                                    .ok_or_else(|| Error::Msg("Expected var name"))?;
                                // Checking this early for better errors
                                let _ = expect_token(rest, b"=")?;
                                add_set_decl(
                                    &mut stage1.variable_types,
                                    name,
                                    VariableType::SpriteLocal,
                                )?;
                            }
                            ctx.error_line_number = main_line_no;
                        }
                    }
                    _ => (),
                }
                ctx.current_block.0.lines.push(PartiallyParsedLine {
                    ty: BlockLine::Command(command, rest, linked),
                    line_number,
                });
            }
            None => return Err(Error::UnknownCommand(command.into())),
        }
        Ok(())
    }

    /// Reads lines until '}' and adds them as a new set to stage1.sprite_local_sets.
    fn parse_spritelocal_set(
        &mut self,
        stage1: &mut ParseStage1<'a>,
        ctx: &mut TextParseContext<'a>,
    ) -> Result<SpriteLocalSetId, Error> {
        let start_line = ctx.error_line_number;
        stage1.sprite_local_sets.build(|| {
            if let Some(line) = ctx.next_line() {
                if line == b"}" {
                    Ok(None)
                } else {
                    Ok(Some((line, ctx.line_number)))
                }
            } else {
                ctx.error_line_number = start_line;
                Err(Error::Msg("Unterminated {"))
            }
        })
    }

    fn parse_line_add_label(
        &mut self,
        line: &PartiallyParsedLine<'a>,
        block_scope: &BlockScope<'a, '_>,
        compiler: &mut Compiler<'a>,
    ) -> Result<(), Error> {
        match line.ty {
            BlockLine::Label(label) => {
                compiler.add_label(label, block_scope)?;
            }
            BlockLine::Command(command, _, _) => {
                match command {
                    CommandPrototype::BwCommand(..) => compiler.flow_to_bw(),
                    CommandPrototype::End => compiler.flow_to_bw(),
                    _ => compiler.flow_to_aice(),
                }
            }
            _ => (),
        }
        Ok(())
    }

    /// Adds code for the line.
    fn parse_line_pass3(
        &mut self,
        line: &PartiallyParsedLine<'a>,
        block_scope: &BlockScope<'a, '_>,
        compiler: &mut Compiler<'a>,
        stage1: &ParseStage1<'a>,
        ctx: &mut CompileContext<'_>,
    ) -> Result<(), Error> {
        compiler.set_current_line(line.line_number as u32);
        match line.ty {
            BlockLine::Label(label) => {
                compiler.set_label_position_to_current(label, block_scope);
                Ok(())
            }
            BlockLine::Command(command, rest, linked) => match command {
                CommandPrototype::BwCommand(op, params, ends_flow) => {
                    compiler.add_bw_command(op, rest, params, ends_flow, block_scope)?;
                    Ok(())
                }
                CommandPrototype::If => {
                    let (condition, rest) =
                        parse_bool_expr(rest, &mut self.exprs, &compiler.variables)?;
                    let mut tokens = rest.fields();
                    let next = tokens.next();
                    let is_call = match next {
                        Some(b"goto") => false,
                        Some(b"call") => true,
                        _ => {
                            return Err(Error::Dynamic(
                                format!("Expected 'goto' or 'call', got {:?}", next)
                            ));
                        }
                    };
                    let label = tokens.next()
                        .and_then(|l| parse_label_ref(l))
                        .ok_or_else(|| Error::Msg("Expected label after goto"))?;
                    compiler.add_if(condition, label, block_scope, is_call)
                }
                CommandPrototype::Call => {
                    let mut tokens = rest.fields();
                    let label = tokens.next()
                        .and_then(|l| parse_label_ref(l))
                        .ok_or_else(|| Error::Msg("Expected label"))?;
                    compiler.add_call(label, block_scope)
                }
                CommandPrototype::Return => {
                    compiler.add_aice_command(aice_op::RETURN);
                    Ok(())
                }
                CommandPrototype::FireWeapon => {
                    let exprs = &mut self.exprs;
                    let (expr, rest) = parse_int_expr(rest, exprs, &compiler.variables)?;
                    let spritelocals = parse_with(rest, exprs, stage1, compiler, ctx, linked)?;
                    let id = compiler.expressions.int_expr_id(expr);
                    compiler.add_aice_command(aice_op::FIRE_WEAPON);
                    compiler.aice_bytecode.write_u32::<LE>(id).unwrap();
                    compiler.aice_bytecode.write_u32::<LE>(spritelocals.0).unwrap();
                    // castspell
                    compiler.add_bw_code(&[0x27]);
                    compiler.add_aice_command_u32(aice_op::SET_ORDER_WEAPON, !0);
                    Ok(())
                }
                CommandPrototype::PlayFram => {
                    let (expr, rest) = parse_int_expr(rest, &mut self.exprs, &compiler.variables)?;
                    if !rest.is_empty() {
                        return Err(
                            Error::Dynamic(format!("Trailing characters '{}'", rest.as_bstr()))
                        );
                    }
                    let id = compiler.expressions.int_expr_id(expr);
                    compiler.add_aice_command_u32(aice_op::PLAY_FRAME, id);
                    Ok(())
                }
                CommandPrototype::Sigorder | CommandPrototype::OrderDone => {
                    let mut tokens = rest.fields();
                    let flags = tokens.next().and_then(|x| parse_u8(x));
                    let has_next = tokens.next().is_some();
                    let flags = match (flags, has_next) {
                        (Some(s), false) => s,
                        _ => return Err(Error::Dynamic(format!(
                            "Expected single integer paramete, got '{}'", rest.as_bstr(),
                        ))),
                    };
                    let op = match command {
                        CommandPrototype::Sigorder => aice_op::SIGORDER,
                        _ => aice_op::ORDER_DONE,
                    };
                    compiler.add_aice_command_u8(op, flags);
                    Ok(())
                }
                CommandPrototype::GotoRepeatAttk => {
                    compiler.add_aice_command(aice_op::CLEAR_ATTACKING_FLAG);
                    Ok(())
                }
                CommandPrototype::CreateUnit => {
                    let vars = &compiler.variables;
                    let exprs = &mut self.exprs;
                    let (unit_id, rest) = parse_int_expr(rest, exprs, vars)?;
                    let (x, rest) = parse_int_expr(rest, exprs, vars)?;
                    let (y, rest) = parse_int_expr(rest, exprs, vars)?;
                    let (player, rest) = parse_int_expr(rest, exprs, vars)?;
                    let spritelocals = parse_with(rest, exprs, stage1, compiler, ctx, linked)?;
                    let expr = &mut compiler.expressions;
                    let unit_id = expr.int_expr_id(unit_id);
                    let x = expr.int_expr_id(x);
                    let y = expr.int_expr_id(y);
                    let player = expr.int_expr_id(player);
                    compiler.add_aice_command(aice_op::CREATE_UNIT);
                    compiler.aice_bytecode.write_u32::<LE>(unit_id).unwrap();
                    compiler.aice_bytecode.write_u32::<LE>(x).unwrap();
                    compiler.aice_bytecode.write_u32::<LE>(y).unwrap();
                    compiler.aice_bytecode.write_u32::<LE>(player).unwrap();
                    compiler.aice_bytecode.write_u32::<LE>(spritelocals.0).unwrap();
                    Ok(())
                }
                CommandPrototype::IssueOrder => {
                    let (order_id, rest) = parse_u8_rest(rest)?;
                    let vars = &compiler.variables;
                    let exprs = &mut self.exprs;
                    let (x, rest) = parse_int_expr(rest, exprs, vars)?;
                    let (y, rest) = parse_int_expr(rest, exprs, vars)?;
                    if !rest.is_empty() {
                        return Err(
                            Error::Dynamic(format!("Trailing characters '{}'", rest.as_bstr()))
                        );
                    }
                    let expr = &mut compiler.expressions;
                    let x = expr.int_expr_id(x);
                    let y = expr.int_expr_id(y);
                    compiler.add_aice_command(aice_op::ISSUE_ORDER);
                    compiler.aice_bytecode.write_u8(order_id).unwrap();
                    compiler.aice_bytecode.write_u32::<LE>(x).unwrap();
                    compiler.aice_bytecode.write_u32::<LE>(y).unwrap();
                    Ok(())
                }
                CommandPrototype::End => {
                    compiler.add_aice_command(aice_op::PRE_END);
                    compiler.flow_to_bw();
                    Ok(())
                }
                CommandPrototype::Set => {
                    ctx.expr_buffer[0] = None;
                    let vars = &compiler.variables;
                    let exprs = &mut self.exprs;
                    let (place, if_uninit, rest) =
                        exprs.parse_set_place(rest, &mut ctx.expr_buffer, vars)?;
                    let (expr, rest) = parse_set_expr(
                        place,
                        rest,
                        exprs,
                        vars,
                        &mut ctx.anytype_parser,
                    )?;
                    if !rest.is_empty() {
                        return Err(
                            Error::Dynamic(format!("Trailing characters '{}'", rest.as_bstr()))
                        );
                    }
                    compiler.add_set(
                        &mut ctx.write_buffer,
                        place,
                        if_uninit,
                        expr,
                        &mut ctx.expr_buffer,
                    )?;
                    Ok(())
                }
                CommandPrototype::ImgulOn | CommandPrototype::ImgolOn => {
                    // op, flags, target, image, x, y
                    // Flags 0x1 = Imgol, 0x2 = image is u16 const, 0x4 = x is i8 const
                    // 0x8 = y is i8 const
                    // Otherwise u32 expressions
                    let (unit_ref, rest) = self.exprs.parse_unit_ref_rest(rest)?;
                    let vars = &compiler.variables;
                    let exprs = &mut self.exprs;
                    let (image, rest) = parse_int_expr(rest, exprs, vars)?;
                    let (x, rest) = parse_int_expr(rest, exprs, vars)?;
                    let (y, rest) = parse_int_expr(rest, exprs, vars)?;
                    if !rest.is_empty() {
                        return Err(
                            Error::Dynamic(format!("Trailing characters '{}'", rest.as_bstr()))
                        );
                    }

                    let mut flags = match command {
                        CommandPrototype::ImgulOn => 0,
                        _ => 0x1,
                    };
                    let exprs = &mut compiler.expressions;
                    let image = if let Some(c) = is_constant_expr(&image) {
                        flags |= 0x2;
                        u16::try_from(c)
                            .map(|x| x as u32)
                            .map_err(|_| Error::Msg("Image ID must be between 0 and 65535"))?
                    } else {
                        exprs.int_expr_id(image)
                    };
                    let x = if let Some(c) = is_constant_expr(&x) {
                        flags |= 0x4;
                        u8::try_from(c)
                            .map(|x| x as u32)
                            .or_else(|_| i8::try_from(c).map(|x| x as u32))
                            .map_err(|_| Error::Msg("X must be between -128 and 255"))?
                    } else {
                        exprs.int_expr_id(x)
                    };
                    let y = if let Some(c) = is_constant_expr(&y) {
                        flags |= 0x8;
                        u8::try_from(c)
                            .map(|x| x as u32)
                            .or_else(|_| i8::try_from(c).map(|x| x as u32))
                            .map_err(|_| Error::Msg("Y must be between -128 and 255"))?
                    } else {
                        exprs.int_expr_id(y)
                    };

                    compiler.add_aice_command(aice_op::IMG_ON);
                    compiler.aice_bytecode.write_u8(flags).unwrap();
                    compiler.aice_bytecode.write_u16::<LE>(unit_ref.0).unwrap();
                    if flags & 0x2 == 0 {
                        compiler.aice_bytecode.write_u32::<LE>(image).unwrap();
                    } else {
                        compiler.aice_bytecode.write_u16::<LE>(image as u16).unwrap();
                    }
                    if flags & 0x4 == 0 {
                        compiler.aice_bytecode.write_u32::<LE>(x).unwrap();
                    } else {
                        compiler.aice_bytecode.write_u8(x as u8).unwrap();
                    }
                    if flags & 0x8 == 0 {
                        compiler.aice_bytecode.write_u32::<LE>(y).unwrap();
                    } else {
                        compiler.aice_bytecode.write_u8(y as u8).unwrap();
                    }
                    Ok(())
                }
                CommandPrototype::Print => {
                    let format_id = self.format_strings.parse(rest, &mut self.exprs, compiler)?;
                    compiler.add_aice_command_u32(aice_op::PRINT, format_id.0);
                    Ok(())
                }
            }
            _ => Ok(())
        }
    }
}

impl ParserExprs {
    fn clear_current_error(&mut self) {
        self.current_error = None;
    }

    fn set_current_error(&mut self, error: Error) {
        self.current_error = Some(error);
    }

    fn parse_unit_ref_rest<'text>(
        &mut self,
        text: &'text [u8],
    ) -> Result<(UnitRefId, &'text [u8]), Error> {
        self.unit_refs.parse(text)
    }

    fn var_name_type_from_set_place<'a>(
        &mut self,
        text: &'a [u8],
    ) -> Result<Option<(&'a [u8], VariableType)>, Error> {
        let (mut place, mut rest) = split_first_token_brace_aware(text)
            .ok_or_else(|| CanRecoverError::Yes(Error::Msg("Expected place")))?;
        if place == b"if_uninit" {
            let (place2, rest2) = split_first_token_brace_aware(rest)
                .ok_or_else(|| CanRecoverError::Yes(Error::Msg("Expected place")))?;
            place = place2;
            rest = rest2;
        }
        let ty = match place {
            x if x == b"global" => VariableType::Global,
            x if x == b"spritelocal" => VariableType::SpriteLocal,
            _ => return Ok(None),
        };
        let (name, _rest) = field_name_from_projection(rest)?;
        Ok(Some((name, ty)))
    }

    fn parse_place_expr<'text, 'a>(
        &mut self,
        text: &'text [u8],
        var_out: &mut [Option<Box<IntExpr>>; 4],
        variable_decls: &CompilerVariables<'a>,
    ) -> Result<(PlaceId, &'text [u8]), CanRecoverError> {
        let (mut bw, rest) = self.parse_place_expr_pre_params(text, variable_decls)?;
        let var_count = bw.place().var_count();
        if var_count == 0 {
            if rest.get(0).copied() == Some(b'(') {
                return Err(Error::Dynamic(
                    format!("Place '{}' does not accept parameters", text.as_bstr())
                ).into());
            }
            Ok((bw, rest))
        } else {
            let mut params = expect_token(rest, b"(")?;
            for i in 0..var_count {
                let (expr, rest) = parse_int_expr_allow_opt(params, self, variable_decls)?;
                if i != var_count - 1 {
                    let rest = expect_token(rest, b",")?;
                    params = rest;
                } else {
                    params = rest;
                }
                var_out[i as usize] = Some(Box::new(expr));
            }
            params = match expect_token(params, b")") {
                Ok(o) => o,
                Err(_) => {
                    return Err(Error::Dynamic(
                        format!("Too many parameters for place: '{}'", params.as_bstr())
                    ).into());
                }
            };
            // Parse what location coord it actually is
            if matches!(bw.place(), Place::Game(GameVar::LocationLeft)) {
                params = expect_token(params, b".")?;
                let (next, rest) = split_first_token(params)
                    .ok_or_else(|| Error::Msg("Expected location coord"))?;
                params = rest;
                bw = match next {
                    b"left" => PlaceId::new_game(GameVar::LocationLeft),
                    b"top" => PlaceId::new_game(GameVar::LocationTop),
                    b"right" => PlaceId::new_game(GameVar::LocationRight),
                    b"bottom" => PlaceId::new_game(GameVar::LocationBottom),
                    _ => return Err(Error::Dynamic(
                        format!("Invalid location coord '{}'", next.as_bstr())
                    ).into()),
                };
            }
            Ok((bw, params))
        }
    }

    /// Returns (place, if_uninit, rest)
    fn parse_set_place<'a>(
        &mut self,
        mut text: &'a [u8],
        result_variables: &mut [Option<Box<IntExpr>>; 4],
        variable_decls: &CompilerVariables<'a>,
    ) -> Result<(PlaceId, bool, &'a [u8]), Error> {
        let mut uninit = false;
        if let Some((first, rest)) = split_first_token(text) {
            if first == b"if_uninit" {
                text = rest;
                uninit = true;
            }
        }
        let (place, rest) = split_first_token_brace_aware(text)
            .ok_or_else(|| CanRecoverError::Yes(Error::Msg("Expected place")))?;
        let text = match place {
            x if x == b"global" => rest,
            x if x == b"spritelocal" => rest,
            _ => text,
        };
        let (expr, rest) = self.parse_place_expr(text, result_variables, variable_decls)?;
        let rest = expect_token(rest, b"=")?;
        if uninit {
            match expr.place() {
                Place::SpriteLocal(..) => (),
                _ => return Err(
                    Error::Msg("`if_uninit` can only be used for spritelocal variables")
                ),
            }
        }
        Ok((expr, uninit, rest))
    }

    /// Parses `unit.hitpoints`, `unit.target.target.parent.hitpoints`, `game.deaths`, etc.
    /// `place` must be already parsed to be just the text before () params of a place.
    fn parse_place_expr_pre_params<'text>(
        &mut self,
        place: &'text [u8],
        variable_decls: &CompilerVariables<'_>,
    ) -> Result<(PlaceId, &'text [u8]), CanRecoverError> {
        let error = || {
            CanRecoverError::Yes(
                Error::Dynamic(format!("Unknown place/variable type '{}'", place.as_bstr()))
            )
        };

        let (first, rest) = split_first_token(place)
            .ok_or_else(error)?;
        if rest.get(0).copied() != Some(b'.') {
            if let Some(&var_id) = variable_decls.variables.get(first) {
                return Ok((var_id, rest));
            }
        }
        let rest_dot = rest;
        let rest = expect_token(rest, b".")
            .map_err(CanRecoverError::Yes)?;
        match first {
            b"flingy" => {
                let (field, rest) = split_first_token(rest)
                    .ok_or_else(error)?;
                return self.flingy_vars.get(field)
                    .map(|&var| (PlaceId::new_flingy(var, UnitRefId::this()), rest))
                    .ok_or_else(error);
            }
            b"bullet" => {
                let (field, rest) = split_first_token(rest)
                    .ok_or_else(error)?;
                match self.bullet_vars.get(field) {
                    Some(&var) => return Ok((PlaceId::new_bullet(var), rest)),
                    // keep going for bullet.target.x etc
                    None => (),
                }
            }
            b"image" => {
                let (field, rest) = split_first_token(rest)
                    .ok_or_else(error)?;
                return self.image_vars.get(field)
                    .map(|&var| (PlaceId::new_image(var), rest))
                    .ok_or_else(error);
            }
            b"game" => {
                let (field, rest) = split_first_token(rest)
                    .ok_or_else(error)?;
                return self.game_vars.get(field)
                    .map(|&var| (PlaceId::new_game(var), rest))
                    .ok_or_else(error);
            }
            _ => (),
        }

        let (unit_ref, rest) = self.unit_refs.parse_pre_split(first, rest, rest_dot)?;
        let rest = expect_token(rest, b".")?;
        let (field, rest) = split_first_token(rest)
            .ok_or_else(error)?;
        let place = self.unit_vars.get(field.as_bytes()).copied()
            .map(|var| PlaceId::new_unit(var, unit_ref))
            .or_else(|| {
                self.flingy_vars.get(field.as_bytes()).copied()
                    .map(|var| PlaceId::new_flingy(var, unit_ref))
            })
            .or_else(|| {
                variable_decls.variables.get(field.as_bytes())
                    .and_then(|x| match x.place() {
                        Place::SpriteLocal(_, id) => PlaceId::new_spritelocal(id, unit_ref),
                        _ => None,
                    })
            })
            .ok_or_else(|| {
                Error::Dynamic(format!("Unknown variable name '{}'", field.as_bstr()))
            })?;
        Ok((place, rest))
    }
}

fn is_constant_expr(expr: &IntExpr) -> Option<i32> {
    if let IntExprTree::Integer(c) = *expr.inner() {
        Some(c)
    } else {
        None
    }
}

struct TextParseContext<'a> {
    // Block, index, block start line
    block_stack: Vec<(Block<'a>, BlockId, usize)>,
    current_block: (Block<'a>, BlockId, usize),
    text: &'a [u8],
    line_number: usize,
    /// Current line for error reporting.
    error_line_number: usize,
    is_continuing_commands: bool,
}

impl<'a> TextParseContext<'a> {
    pub fn next_line(&mut self) -> Option<&'a [u8]> {
        loop {
            let text = self.text;
            if text.is_empty() {
                return None;
            }
            self.line_number = self.line_number.wrapping_add(1);
            let line_end = text.find_byte(b'\n').unwrap_or_else(|| text.len());
            let mut line = &text[..line_end];
            self.text = text.get(line_end + 1..).unwrap_or_else(|| b"");
            // Comment
            if let Some(comment_start) = line.find_byte(b'#') {
                line = &line[..comment_start];
            };
            let line = line.trim();
            if !line.is_empty() {
                return Some(line);
            }
        }
    }
}

/// Splits out last part of x.y.z.x chain
fn field_name_from_projection<'a>(text: &'a [u8]) -> Result<(&'a [u8], &'a [u8]), Error> {
    let (mut result, mut rest) = split_first_token(text)
        .ok_or_else(|| Error::Msg("Expected variable name"))?;
    while rest.get(0).copied() == Some(b'.') {
        let (a, b) = split_first_token(expect_token(rest, b".")?)
            .ok_or_else(|| Error::Msg("Expected variable name after final '.'"))?;
        result = a;
        rest = b;
    }
    Ok((result, rest))
}

fn split_first_token_brace_aware(text: &[u8]) -> Option<(&[u8], &[u8])> {
    let start = text.bytes().position(|x| x != b' ' && x != b'\t')?;
    let mut brace_depth = 0u32;
    let is_word_token = is_word_char(text[0]);
    let mut end = start.wrapping_add(1);
    while let Some(&byte) = text.get(end) {
        if byte == b'(' {
            brace_depth = brace_depth.wrapping_add(1);
        } else if brace_depth == 0 && (is_word_char(byte) != is_word_token || byte == b')') {
            break;
        } else if byte == b')' {
            brace_depth = brace_depth.saturating_sub(1);
        }
        end = end.wrapping_add(1);
    }
    let first = &text[start..end];
    Some(match text.bytes().skip(end).position(|x| x != b' ' && x != b'\t') {
        Some(s) => (first, &text[end + s..]),
        None => (first, b""),
    })
}

fn is_word_char(val: u8) -> bool {
    matches!(val, b'a' ..= b'z' | b'A' ..= b'Z' | b'_' | b'0' ..= b'9')
}

fn split_first_token(text: &[u8]) -> Option<(&[u8], &[u8])> {
    debug_assert!(text.trim().len() == text.len(), "Untrimmed input {}", text.as_bstr());
    let &first = text.first()?;
    let end = if !is_word_char(first) {
        1
    } else {
        text.bytes().skip(1).position(|x| !is_word_char(x))
            .map(|x| 1usize.wrapping_add(x))
            .unwrap_or(text.len())
    };
    let first = &text[..end];
    Some(match text.bytes().skip(end).position(|x| x != b' ' && x != b'\t') {
        Some(s) => (first, &text[end + s..]),
        None => (first, b""),
    })
}

fn expect_token<'a>(text: &'a [u8], token: &'static [u8]) -> Result<&'a [u8], Error> {
    split_first_token(text)
        .filter(|x| x.0 == token)
        .map(|x| x.1)
        .ok_or_else(|| Error::Expected(text.into(), token))
}

fn split_last_token(text: &[u8]) -> Option<(&[u8], &[u8])> {
    debug_assert!(text.trim().len() == text.len(), "Untrimmed input {}", text.as_bstr());
    let &last = text.last()?;
    let rev_end = if !is_word_char(last) {
        1
    } else {
        text.bytes().rev().skip(1).position(|x| !is_word_char(x))
            .map(|x| 1usize.wrapping_add(x))
            .unwrap_or(text.len())
    };
    let start = text.len() - rev_end;
    let first = &text[start..];
    Some(match text.bytes().rev().skip(rev_end).position(|x| x != b' ' && x != b'\t') {
        Some(s) => (first, &text[..text.len() - rev_end - s]),
        None => (first, b""),
    })
}

fn ends_with_tokens(mut text: &[u8], tokens: &[&[u8]]) -> bool {
    for &token in tokens.iter().rev() {
        match split_last_token(text) {
            Some((tok, rest)) if tok == token => {
                text = rest;
            }
            _ => return false,
        }
    }
    true
}

fn parse_bool_expr_allow_opt<'a, 'b, 'c>(
    text: &'a [u8],
    parser: &'c mut ParserExprs,
    variable_decls: &'c CompilerVariables<'b>,
) -> Result<(BoolExpr, &'a [u8]), Error> {
    let mut parser = ExprParser {
        variables: variable_decls,
        parser,
    };
    CustomBoolExpr::parse_part_custom(text.as_ref(), &mut parser)
        .map(|(a, b)| (a, b))
        .map_err(|e| Error::from(e))
}

fn parse_bool_expr<'a, 'b, 'c>(
    text: &'a [u8],
    parser: &'c mut ParserExprs,
    variable_decls: &'c CompilerVariables<'b>,
) -> Result<(BoolExpr, &'a [u8]), Error> {
    let (expr, rest) = parse_bool_expr_allow_opt(text, parser, variable_decls)?;
    if is_optional_expr(expr.inner()) {
        let expr_string_len = text.len() - rest.len();
        let expr_string = &text[..expr_string_len];
        return Err(Error::Dynamic(
            format!("Expression '{}' needs `default`", expr_string.as_bstr())
        ));
    }
    Ok((expr, rest))
}

fn parse_int_expr_allow_opt<'a, 'b, 'c>(
    text: &'a [u8],
    parser: &'c mut ParserExprs,
    variable_decls: &'c CompilerVariables<'b>,
) -> Result<(IntExpr, &'a [u8]), Error> {
    let mut parser = ExprParser {
        variables: variable_decls,
        parser,
    };
    CustomIntExpr::parse_part_custom(text.as_ref(), &mut parser)
        .map(|(a, b)| (a, b))
        .map_err(|e| Error::from(e))
}

fn parse_int_expr<'a, 'b, 'c>(
    text: &'a [u8],
    parser: &'c mut ParserExprs,
    variable_decls: &'c CompilerVariables<'b>,
) -> Result<(IntExpr, &'a [u8]), Error> {
    let (expr, rest) = parse_int_expr_allow_opt(text, parser, variable_decls)?;
    if is_optional_expr(expr.inner()) {
        let expr_string_len = text.len() - rest.len();
        let expr_string = &text[..expr_string_len];
        return Err(Error::Dynamic(
            format!("Expression '{}' needs `default`", expr_string.as_bstr())
        ));
    }
    Ok((expr, rest))
}

fn parse_any_expr_allow_opt<'a, 'b, 'c>(
    text: &'a [u8],
    parser: &'c mut ParserExprs,
    compiler: &'c mut Compiler<'b>,
) -> Result<(AnyExpr, &'a [u8]), Error> {
    let int_result = parse_int_expr_allow_opt(text, parser, &compiler.variables);
    if int_result.is_ok() {
        if let Ok(x) = int_result {
            let expr = compiler.expressions.int_expr_id(x.0);
            return Ok((AnyExpr::Int(expr), x.1));
        }
    }
    let int_err = parser.current_error.take().unwrap_or(int_result.unwrap_err());
    let bool_result = parse_bool_expr_allow_opt(text, parser, &compiler.variables);
    if bool_result.is_ok() {
        if let Ok(x) = bool_result {
            let expr = compiler.expressions.bool_expr_id(x.0);
            return Ok((AnyExpr::Bool(expr), x.1));
        }
    }
    let bool_err = parser.current_error.take().unwrap_or(bool_result.unwrap_err());
    let unit_ref_result = parser.parse_unit_ref_rest(text);
    if unit_ref_result.is_ok() {
        if let Ok(x) = unit_ref_result {
            return Ok((AnyExpr::UnitRef(x.0), x.1));
        }
    }
    let unit_err = parser.current_error.take().unwrap_or(unit_ref_result.unwrap_err());
    Err(Error::Dynamic(
        format!(
            "Cannot parse '{}' as any kind of expression:\n\
            Int parsing error: {}\n\
            Bool parsing error: {}\n\
            Unit ref parsing error: {}",
            text.as_bstr(), int_err, bool_err, unit_err,
        )
    ))
}

fn parse_set_expr<'a, 'b, 'c, 'bump>(
    dest_place: PlaceId,
    text: &'a [u8],
    parser: &'c mut ParserExprs,
    variable_decls: &'c CompilerVariables<'b>,
    anytype_parser: &mut AnyTypeParser<'bump>,
) -> Result<(SetExpr<'bump>, &'a [u8]), Error> {
    if dest_place.is_variable() {
        if let Some((expr, rest)) =
            anytype_parser.try_parse_anytype_expr(text, parser, variable_decls)?
        {
            if rest.is_empty() {
                return Ok((SetExpr::AnyType(expr), rest));
            }
        }
    }
    parse_int_expr(text, parser, variable_decls)
        .map(|x| (SetExpr::Int(x.0), x.1))
}

/// Parses `with { spritelocal1 = expr, ... }`
/// Accepts also empty input for empty set of spritelocals.
fn parse_with<'a, 'b, 'c>(
    text: &'a [u8],
    parser: &'c mut ParserExprs,
    stage1: &ParseStage1<'b>,
    compiler: &'c mut Compiler<'b>,
    ctx: &mut CompileContext<'_>,
    linked: LinkedBlock,
) -> Result<SpriteLocalSetId, Error> {
    if text.is_empty() {
        return Ok(SpriteLocalSetId(0));
    }

    let parser_id = match linked {
        LinkedBlock::SpriteLocals(id) => id,
        _ => return Err(Error::Internal("Linked id not spritelocal")),
    };
    let (_, rest) = split_first_token(text)
        .filter(|x| x.0 == b"with")
        .ok_or_else(|| {
            Error::Dynamic(format!("Expected 'with' or end of line; got '{}'", text.as_bstr()))
        })?;
    let rest = expect_token(rest, b"{")?;
    if !rest.is_empty() {
        return Err(Error::Trailing(rest.into()));
    }

    let prev_line = ctx.error_line;
    let mut input_sets = stage1.sprite_local_sets.get_or_empty(parser_id);
    let vars = &compiler.variables;
    let exprs = &mut compiler.expressions;
    let id = compiler.sprite_local_sets.build(|| {
        let (&(text, line_no), rest) = match input_sets.split_first() {
            Some(s) => s,
            None => return Ok(None),
        };
        input_sets = rest;
        ctx.error_line = line_no;
        let (var_name, rest) = split_first_token(text)
            .ok_or_else(|| Error::Msg("Expected variable name"))?;
        let rest = expect_token(rest, b"=")?;
        let (expr, rest) = parse_int_expr(rest, parser, vars)?;
        if !rest.is_empty() {
            return Err(Error::Trailing(rest.into()));
        }
        let expr = exprs.int_expr_id(expr);
        let var = vars.variables.get(var_name)
            .and_then(|x| match x.place() {
                Place::SpriteLocal(_, x) => Some(x),
                _ => None,
            })
            .ok_or_else(|| Error::Internal("Var not added"))?;
        Ok(Some((var, expr)))
    })?;
    ctx.error_line = prev_line;
    Ok(id)
}

fn mark_required_bw_labels<'a>(
    text: &'a [u8],
    params: &[BwCommandParam],
    compiler: &mut Compiler<'a>,
    ctx: &mut TextParseContext<'a>,
) -> Result<(),  Error> {
    let mut tokens = text.fields();
    for param in params {
        let arg = tokens.next()
            .ok_or_else(|| Error::Dynamic(format!("Expected {} arguments", params.len())))?;
        if let BwCommandParam::Label = param {
            let label = parse_label_ref(arg).ok_or_else(|| Error::Msg("Expected label"))?;
            compiler.require_bw_label(label, ctx);
        }
    }
    Ok(())
}

fn parse_header_opcode<'a>(
    line: &'a [u8],
    animation_name_to_index: &FxHashMap<&'static [u8], u8>,
) -> Result<HeaderOpcode<'a>, Error> {
    let mut tokens = line.fields();
    let first = tokens.next().ok_or_else(|| Error::Msg("???"))?;
    if first == b"Type" {
        Ok(HeaderOpcode::Type)
    } else if first == b"IsId" {
        let id = tokens.next()
            .and_then(|x| parse_u16(x))
            .ok_or_else(|| Error::Msg("Expected iscript ID"))?;
        Ok(HeaderOpcode::IscriptId(id))
    } else {
        let label = tokens.next().ok_or_else(|| Error::Msg("Expected label"))?;
        if let Some(next) = tokens.next() {
            return Err(Error::Trailing(next.into()));
        }
        let animation = match animation_name_to_index.get(first) {
            Some(&x) => x,
            None => {
                return Err(Error::Dynamic(format!("Unknown animation '{}'", first.as_bstr())))
            }
        };
        Ok(HeaderOpcode::Animation(animation, Label(label.into())))
    }
}

/// Parses line in form "Label:", or returns None for other command
/// Bool is true if the label is followed by '{'
fn parse_label<'a>(line: &'a [u8]) -> Result<Option<(Label<'a>, bool)>, Error> {
    let mut tokens = line.fields();
    let first = tokens.next().ok_or_else(|| Error::Msg("???"))?;
    if first.ends_with(b":") {
        let label = &first[..first.len() - 1];
        if let Some(label) = parse_label_ref(label) {
            if let Some(next) = tokens.next() {
                if next == b"{" {
                    if let Some(next) = tokens.next() {
                        Err(Error::Trailing(next.into()))
                    } else {
                        Ok(Some((label, true)))
                    }
                } else {
                    Err(Error::Trailing(next.into()))
                }
            } else {
                Ok(Some((label, false)))
            }
        } else {
            Err(Error::Msg("Invalid characters in label name"))
        }
    } else {
        Ok(None)
    }
}

/// Parses label name (as in argument)
fn parse_label_ref<'a>(label: &'a [u8]) -> Option<Label<'a>> {
    if label.bytes().all(|x| x.is_ascii_alphanumeric() || x == b'_') {
        Some(Label(label.into()))
    } else {
        None
    }
}

fn parse_u16(text: &[u8]) -> Option<u16> {
    let (base, text) = match text.starts_with(b"0x") {
        true => (16, &text[2..]),
        false => (10, text),
    };
    u16::from_str_radix(text.to_str().ok()?, base).ok()
}

fn parse_u8(text: &[u8]) -> Option<u8> {
    let (base, text) = match text.starts_with(b"0x") {
        true => (16, &text[2..]),
        false => (10, text),
    };
    u8::from_str_radix(text.to_str().ok()?, base).ok()
}

fn parse_u8_rest(text: &[u8]) -> Result<(u8, &[u8]), Error> {
    let space = text.iter().position(|&x| x == b' ' || x == b'\t')
        .unwrap_or(text.len());
    let (text, rest) = text.split_at(space);
    let rest_pos = rest.iter().position(|&x| x != b' ' && x != b'\t')
        .unwrap_or(text.len());
    let (base, text) = match text.starts_with(b"0x") {
        true => (16, &text[2..]),
        false => (10, text),
    };
    let val = text.to_str().ok()
        .and_then(|x| u8::from_str_radix(x, base).ok())
        .ok_or_else(|| Error::CannotParse(text.into(), "uint8"))?;
    Ok((val, &rest[rest_pos..]))
}

/// Supports both u8 and i8 forms, so returns as u8
fn parse_i8(text: &[u8]) -> Option<u8> {
    let (negative, text) = match text.starts_with(b"-") {
        true => (true, &text[1..]),
        false => (false, text),
    };
    let as_u8 = parse_u8(text)?;
    if negative && as_u8 <= 128 {
        Some(0u8.wrapping_sub(as_u8))
    } else if !negative {
        Some(as_u8)
    } else {
        None
    }
}

enum HeaderOpcode<'a> {
    Type,
    IscriptId(u16),
    Animation(u8, Label<'a>),
}

struct Compiler<'a> {
    bw_bytecode: Vec<u8>,
    aice_bytecode: Vec<u8>,
    bw_offsets_in_aice: Vec<u32>,
    bw_offsets_in_bw: Vec<u16>,
    bw_code_flow_ends: Vec<u16>,
    /// Collects where bw_bytecode has been finally placed when compiled.
    /// (start_offset_in_final_bin, length)
    bw_code_parts: Vec<(u16, u16)>,
    labels: FxHashMap<(Label<'a>, BlockId), CodePosition>,
    needed_label_positions: Vec<(CodePosition, Label<'a>, BlockId)>,
    bw_required_labels: BwRequiredLabels<'a>,
    bw_aice_cmd_offsets: Vec<(u16, u32)>,
    sprite_local_sets: VecOfVecs<SpriteLocalSetId, (u32, u32)>,
    expressions: CompilerExprs,
    variables: CompilerVariables<'a>,
    flow_in_bw: bool,
    current_line: u32,
    line_info: LineInfoBuilder,
}

struct CompilerVariables<'a> {
    variables: FxHashMap<&'a [u8], PlaceId>,
    variable_counts: [u32; 2],
}

struct CompilerExprs {
    conditions: Vec<Rc<BoolExpr>>,
    existing_conditions: FxHashMap<Rc<BoolExpr>, u32>,
    int_expressions: Vec<Rc<IntExpr>>,
    existing_int_expressions: FxHashMap<Rc<IntExpr>, u32>,
}

/// Reusable buffers
struct CompileContext<'b> {
    expr_buffer: [Option<Box<IntExpr>>; 4],
    write_buffer: Vec<u8>,
    error_line: usize,
    anytype_parser: AnyTypeParser<'b>,
}

struct BwRequiredLabels<'a> {
    set: FxHashSet<(Label<'a>, BlockId)>,
}

impl<'a> BwRequiredLabels<'a> {
    fn insert(&mut self, label: Label<'a>, block_id: BlockId) {
        self.set.insert((label, block_id));
    }

    fn contains(&self, label: Label<'a>, scope: &BlockScope<'a, '_>) -> bool {
        self.set.contains(&(label, scope.block_id()))
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum FlingyVar {
    MoveTargetX,
    MoveTargetY,
    PositionX,
    PositionY,
    FacingDirection,
    MovementDirection,
    TargetDirection,
    TurnSpeed,
    Acceleration,
    TopSpeed,
    Speed,
    // Not really a flingy var but too lazy to implement entity just for this
    Player,
    FlingyId,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BulletVar {
    State,
    DeathTimer,
    WeaponId,
    BouncesRemaining,
    OrderTargetX,
    OrderTargetY,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum UnitVar {
    DeathTimer,
    MatrixTimer,
    MatrixHp,
    StimTimer,
    EnsnareTimer,
    LockdownTimer,
    IrradiateTimer,
    StasisTimer,
    PlagueTimer,
    MaelstormTimer,
    IsBlind,
    Hitpoints,
    MaxHitpoints,
    Shields,
    MaxShields,
    Energy,
    MaxEnergy,
    MineralCost,
    GasCost,
    SupplyCost,
    Resources,
    HangarCountInside,
    HangarCountOutside,
    LoadedCount,
    CurrentUpgrade,
    CurrentTech,
    BuildQueue,
    RemainingBuildTime,
    RemainingResearchTime,
    OverlaySize,
    UnitId,
    Kills,
    CarriedResourceAmount,
    GroundCooldown,
    AirCooldown,
    SpellCooldown,
    Order,
    OrderTimer,
    OrderState,
    RankIncrease,
    MineAmount,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum UnitObject {
    This,
    UnitParent,
    Nuke,
    CurrentlyBuilding,
    UnitTarget,
    Transport,
    Addon,
    Subunit,
    LinkedNydus,
    Powerup,
    RallyTarget,
    IrradiatedBy,
    BulletParent,
    BulletTarget,
    BulletPreviousBounceTarget,
    _Last,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ImageVar {
    Drawfunc,
    DrawfuncParam,
    Frame,
    BaseFrame,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum GameVar {
    Deaths,
    Kills,
    UpgradeLevel,
    UpgradeLimit,
    TechLevel,
    TechAvailability,
    UnitAvailability,
    Alliance,
    SharedVision,
    Minerals,
    Gas,
    ZergSupplyMax,
    TerranSupplyMax,
    ProtossSupplyMax,
    ZergSupplyUsed,
    TerranSupplyUsed,
    ProtossSupplyUsed,
    ZergSupplyProvided,
    TerranSupplyProvided,
    ProtossSupplyProvided,
    LocationLeft,
    LocationTop,
    LocationRight,
    LocationBottom,
    // Note: Score vars get casted based on index, their ordering should not be edited
    UnitsTotal,
    UnitsProduced,
    UnitsOwned,
    UnitsLost,
    UnitsKilled,
    UnitsScore,
    UnitsKilledScore,
    BuildingsTotal,
    BuildingsConstructed,
    BuildingsOwned,
    BuildingsLost,
    BuildingsRazed,
    BuildingsScore,
    BuildingsRazedScore,
    FactoriesConstructed,
    FactoriesOwned,
    FactoriesLost,
    FactoriesRazed,
    CustomScore,
    PlayerColorChoice,
}

/// 0xc000_0000 == tag
/// tag 0 = global, 1 = spritelocal, 2 = bw
/// SpriteLocal 0x2000_0000 = set if_uninit
///     spritelocal 0x1fff_0000 = object ref id
/// bw second tag: 0x3c00_0000, 0 = flingy, 1 = bullet, 2 = unit, 3 = image, 4 = game
///     bw 0x0000_00ff = var id
///     bw 0x00ff_ff00 = object ref id (For units, flingies)
#[derive(Copy, Clone, Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct PlaceId(pub u32);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum VariableType {
    Global,
    SpriteLocal,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Place {
    Global(u32),
    SpriteLocal(UnitRefId, u32),
    Flingy(UnitRefId, FlingyVar),
    Bullet(BulletVar),
    Unit(UnitRefId, UnitVar),
    Image(ImageVar),
    Game(GameVar),
}

/// Anything below `UnitObject::_Last` is just that without extra projection,
/// otherwise iscript.unit_refs[x - UnitObject::_Last]
#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
pub struct UnitRefId(pub u16);

impl UnitRefId {
    pub const fn this() -> UnitRefId {
        UnitRefId(0)
    }

    pub fn is_this(self) -> bool {
        self.0 == UnitObject::This as u16
    }
}

impl PlaceId {
    const fn new_flingy(x: FlingyVar, unit: UnitRefId) -> PlaceId {
        PlaceId((x as u32) | ((unit.0 as u32) << 8) | ((2 << 3) << 27))
    }
    const fn new_bullet(x: BulletVar) -> PlaceId {
        PlaceId((x as u32) | (((2 << 3) + 1) << 27))
    }
    const fn new_unit(x: UnitVar, unit: UnitRefId) -> PlaceId {
        PlaceId((x as u32) | ((unit.0 as u32) << 8) | (((2 << 3) + 2) << 27))
    }
    const fn new_image(x: ImageVar) -> PlaceId {
        PlaceId((x as u32) | (((2 << 3) + 3) << 27))
    }
    const fn new_game(x: GameVar) -> PlaceId {
        PlaceId((x as u32) | (((2 << 3) + 4) << 27))
    }
    fn new_global(id: u32) -> Option<PlaceId> {
        if id <= 0x0fff_ffff {
            Some(PlaceId(id))
        } else {
            None
        }
    }
    fn new_spritelocal(id: u32, unit: UnitRefId) -> Option<PlaceId> {
        if id <= 0x0000_ffff {
            let tag_shifted = 1u32 << 30;
            let unit_shifted = if unit.0 <= 0x1fff {
                (unit.0 as u32) << 16
            } else {
                return None;
            };
            Some(PlaceId(id | tag_shifted | unit_shifted))
        } else {
            None
        }
    }

    pub fn place(self) -> Place {
        let id = self.0 & !0xc000_0000;
        match self.0 >> 30 {
            0 => Place::Global(id),
            1 => Place::SpriteLocal(
                UnitRefId((id >> 16) as u16 & 0x1fff),
                id & 0xffff,
            ),
            2 | _ => match (self.0 >> 27) & 0x7 {
                0 => Place::Flingy(
                    UnitRefId((id >> 8) as u16),
                    unsafe { std::mem::transmute(id as u8) },
                ),
                1 => Place::Bullet(unsafe { std::mem::transmute(id as u8) }),
                2 => Place::Unit(
                    UnitRefId((id >> 8) as u16),
                    unsafe { std::mem::transmute(id as u8) },
                ),
                3 => Place::Image(unsafe { std::mem::transmute(id as u8) }),
                4 | _ => Place::Game(unsafe { std::mem::transmute(id as u8) }),
            },
        }
    }

    pub fn if_uninit(self) -> bool {
        self.0 & 0xe000_0000 == 0x6000_0000
    }

    fn is_optional_expr(self) -> bool {
        match self.0 >> 30 {
            1 => (self.0 >> 16) & 0x1fff != 0,
            2 => self.0 & 0xffff00 != 0 && matches!((self.0 >> 27) & 0x7, 0 | 2),
            _ => false,
        }
    }

    fn is_variable(self) -> bool {
        let tag = self.0 >> 30;
        tag == 0 || tag == 1
    }
}

impl Place {
    /// Places such as game.deaths(player, unit_id) have variables
    /// (player and unit_id), return those.
    pub fn var_count(self) -> u32 {
        use GameVar::*;
        match self {
            Place::Game(var) => match var {
                Deaths | Kills | UpgradeLevel | UpgradeLimit | TechLevel | TechAvailability |
                    UnitAvailability | Alliance | SharedVision => 2,
                Minerals | Gas | ZergSupplyMax | TerranSupplyMax | ProtossSupplyMax |
                    ZergSupplyUsed | TerranSupplyUsed | ProtossSupplyUsed | ZergSupplyProvided |
                    TerranSupplyProvided | ProtossSupplyProvided | LocationLeft | LocationTop |
                    LocationRight | LocationBottom | UnitsTotal | UnitsProduced | UnitsOwned |
                    UnitsLost | UnitsKilled | UnitsScore | UnitsKilledScore | BuildingsTotal |
                    BuildingsConstructed | BuildingsOwned | BuildingsLost | BuildingsRazed |
                    BuildingsScore | BuildingsRazedScore | FactoriesConstructed | FactoriesOwned |
                    FactoriesLost | FactoriesRazed | CustomScore | PlayerColorChoice => 1,
            },
            Place::Unit(_, var) => match var {
                UnitVar::BuildQueue => 1,
                _ => 0,
            },
            _ => 0,
        }
    }
}

#[derive(Copy, Clone, Serialize, Deserialize, Debug)]
pub struct CodePosition(pub u32);

#[derive(Serialize, Deserialize)]
pub enum CodePos {
    Bw(u16),
    Aice(u32),
}

impl fmt::Display for CodePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            CodePos::Aice(off) => write!(f,"Aice:{:04x}", off),
            CodePos::Bw(off) => write!(f,"Bw:{:04x}", off),
        }
    }
}

impl CodePosition {
    pub fn bw(val: u16) -> CodePosition {
        CodePosition(val as u32)
    }

    pub fn aice(val: u32) -> CodePosition {
        CodePosition(val | 0x8000_0000)
    }

    pub fn to_enum(self) -> CodePos {
        if self.0 & 0x8000_0000 != 0 {
            CodePos::Aice(self.0 & 0x7fff_ffff)
        } else {
            CodePos::Bw(self.0 as u16)
        }
    }

    pub fn if_bw_pos(self) -> Option<u16> {
        if self.0 & 0x8000_0000 != 0 {
            None
        } else {
            Some(self.0 as u16)
        }
    }

    fn is_unknown(self) -> bool {
        self.0 == 0xffff || self.0 == 0xffff_ffff
    }
}

impl<'a> Compiler<'a> {
    fn new() -> Compiler<'a> {
        Compiler {
            bw_aice_cmd_offsets: Vec::with_capacity(1024),
            bw_bytecode: Vec::with_capacity(32768),
            aice_bytecode: Vec::with_capacity(32768),
            bw_offsets_in_aice: Vec::with_capacity(4096),
            bw_offsets_in_bw: Vec::with_capacity(4096),
            bw_code_flow_ends: Vec::with_capacity(1024),
            bw_code_parts: Vec::with_capacity(4),
            labels: FxHashMap::with_capacity_and_hasher(1024, Default::default()),
            needed_label_positions: Vec::with_capacity(128),
            bw_required_labels: BwRequiredLabels {
                set: FxHashSet::with_capacity_and_hasher(1024, Default::default()),
            },
            expressions: CompilerExprs {
                conditions: Vec::with_capacity(16),
                existing_conditions:
                    FxHashMap::with_capacity_and_hasher(16, Default::default()),
                int_expressions: Vec::with_capacity(16),
                existing_int_expressions:
                    FxHashMap::with_capacity_and_hasher(16, Default::default()),
            },
            sprite_local_sets: VecOfVecs::new(),
            variables: CompilerVariables {
                variables: FxHashMap::default(),
                variable_counts: [0; 2],
            },
            flow_in_bw: true,
            current_line: 0,
            line_info: LineInfoBuilder::new(),
        }
    }

    fn set_current_line(&mut self, line: u32) {
        self.current_line = line;
    }

    fn code_position(&self) -> CodePosition {
        if self.flow_in_bw {
            CodePosition(self.bw_bytecode.len() as u32)
        } else {
            CodePosition(self.aice_bytecode.len() as u32 | 0x8000_0000)
        }
    }

    fn add_variables(
        &mut self,
        variables: &FxHashMap<&'a [u8], VariableType>,
    ) -> Result<(), Error> {
        let vars = &mut self.variables;
        vars.variables.reserve(variables.len());
        for (&name, &ty) in variables {
            let id = vars.variable_counts[ty as usize] as u32;
            vars.variable_counts[ty as usize] += 1;
            let place_id = match ty {
                VariableType::Global => PlaceId::new_global(id),
                VariableType::SpriteLocal => PlaceId::new_spritelocal(id, UnitRefId::this()),
            };
            let place_id = place_id.ok_or_else(|| Error::Overflow)?;
            vars.variables.insert(name, place_id);
        }
        Ok(())
    }

    /// Makes `label` located at current position.
    fn add_label(&mut self, label: Label<'a>, scope: &BlockScope<'a, '_>) -> Result<(), Error> {
        let bw_required = self.bw_required_labels.contains(label, scope);

        if bw_required {
            self.flow_to_bw();
        }
        match self.labels.entry((label, scope.block_id())) {
            Entry::Vacant(e) => {
                if self.flow_in_bw {
                    e.insert(CodePosition(0xffff));
                } else {
                    e.insert(CodePosition(0xffff_ffff));
                }
                Ok(())
            }
            Entry::Occupied(_) => {
                Err(Error::Dynamic(format!("Duplicate label '{}'", label.0)))
            }
        }
    }

    fn require_bw_label(&mut self, label: Label<'a>, ctx: &mut TextParseContext<'a>) {
        self.bw_required_labels.insert(label, ctx.current_block.1);
        for &(_, parent_id, _) in &ctx.block_stack {
            self.bw_required_labels.insert(label, parent_id);
        }
    }

    fn set_label_position_to_current<'s>(&'s mut self, label: Label<'a>, scope: &BlockScope<'a, '_>) {
        if self.bw_required_labels.contains(label, scope) {
            if !self.flow_in_bw {
                self.emit_aice_jump_to_bw(self.bw_bytecode.len() as u16);
                self.flow_in_bw = true;
            }
        }
        let code_position = self.code_position();
        for block_id in scope.this_and_ancestor_ids() {
            if let Some(label) = self.labels.get_mut(&(label, block_id)) {
                *label = code_position;
                break;
            }
        }
    }

    fn label_location(&self, label: Label<'a>, scope: &BlockScope<'a, '_>) -> Option<CodePosition> {
        scope.this_and_ancestor_ids()
            .filter_map(|block_id| self.labels.get(&(label, block_id)))
            .cloned()
            .next()
    }

    fn add_bw_code(&mut self, code: &[u8]) {
        if !self.flow_in_bw {
            self.emit_aice_jump_to_bw(self.bw_bytecode.len() as u16);
            self.flow_in_bw = true;
        }
        self.line_info.bw.push(self.bw_bytecode.len() as u32, self.current_line);
        self.bw_bytecode.extend_from_slice(code);
    }

    fn add_aice_code(&mut self, code: &[u8]) {
        self.line_info.aice.push(self.aice_bytecode.len() as u32, self.current_line);
        self.aice_bytecode.extend_from_slice(code);
    }

    fn emit_aice_jump_to_bw(&mut self, to: u16) {
        self.bw_offsets_in_aice.push(self.aice_bytecode.len() as u32 + 1);
        self.add_aice_code(&[aice_op::JUMP_TO_BW, to as u8, (to >> 8) as u8]);
    }

    fn emit_bw_jump_to_aice(&mut self, to: u32) {
        self.bw_aice_cmd_offsets.push((self.bw_bytecode.len() as u16, to));
        self.bw_bytecode.push(AICE_COMMAND);
    }

    fn flow_to_bw(&mut self) {
        self.flow_in_bw = true;
    }

    fn flow_to_aice(&mut self) {
        self.flow_in_bw = false;
    }

    fn add_flow_to_aice(&mut self) {
        if self.flow_in_bw {
            self.emit_bw_jump_to_aice(self.aice_bytecode.len() as u32);
            self.flow_in_bw = false;
        }
    }

    fn add_set(
        &mut self,
        write_buffer: &mut Vec<u8>,
        place: PlaceId,
        if_uninit: bool,
        value: SetExpr<'_>,
        place_vars: &mut [Option<Box<IntExpr>>; 4],
    ) -> Result<(), Error> {
        self.add_flow_to_aice();
        let var_id = if if_uninit {
            place.0 | 0x2000_0000
        } else {
            place.0
        };
        write_buffer.clear();
        match value {
            SetExpr::Int(value) => {
                let index = self.expressions.int_expr_id(value);
                write_buffer.push(aice_op::SET);
                write_buffer.write_u32::<LE>(var_id).unwrap();
                write_buffer.write_u32::<LE>(index as u32).unwrap();
                for var in place_vars.iter_mut()
                    .take_while(|x| x.is_some()).filter_map(|x| x.take())
                {
                    let index = self.expressions.int_expr_id(*var);
                    write_buffer.write_u32::<LE>(index as u32).unwrap();
                }
            }
            SetExpr::AnyType(ref ty) => {
                write_buffer.push(aice_op::SET_COPY);
                write_buffer.write_u32::<LE>(var_id).unwrap();
                anytype_expr::write_any_type(write_buffer, ty);
            }
        }
        self.add_aice_code(&write_buffer);
        Ok(())
    }

    fn add_if(
        &mut self,
        condition: BoolExpr,
        dest: Label<'a>,
        block_scope: &BlockScope<'a, '_>,
        is_call: bool,
    ) -> Result<(), Error> {
        self.add_flow_to_aice();
        let index = self.expressions.bool_expr_id(condition);
        let pos = self.label_location(dest, block_scope)
            .ok_or_else(|| Error::Dynamic(format!("Label '{}' not defined", dest.0)))?;

        let mut buffer = [0u8; 9];
        buffer[0] = if is_call { aice_op::IF_CALL } else { aice_op::IF };
        LittleEndian::write_u32(&mut buffer[1..], index as u32);
        LittleEndian::write_u32(&mut buffer[5..], pos.0);
        let offset = self.aice_bytecode.len() as u32 + 5;
        if pos.is_unknown() {
            let block_id = block_scope.block_id();
            self.needed_label_positions.push((CodePosition::aice(offset), dest, block_id));
        }
        if pos.if_bw_pos().is_some() {
            self.bw_offsets_in_aice.push(offset);
        }
        self.add_aice_code(&buffer);
        Ok(())
    }

    fn add_call(
        &mut self,
        dest: Label<'a>,
        block_scope: &BlockScope<'a, '_>
    ) -> Result<(), Error> {
        self.add_flow_to_aice();
        let pos = self.label_location(dest, block_scope)
            .ok_or_else(|| Error::Dynamic(format!("Label '{}' not defined", dest.0)))?;

        let mut buffer = [0u8; 5];
        buffer[0] = aice_op::CALL;
        LittleEndian::write_u32(&mut buffer[1..], pos.0);
        let offset = self.aice_bytecode.len() as u32 + 1;
        if pos.is_unknown() {
            let block_id = block_scope.block_id();
            self.needed_label_positions.push((CodePosition::aice(offset), dest, block_id));
        }
        if pos.if_bw_pos().is_some() {
            self.bw_offsets_in_aice.push(offset);
        }
        self.add_aice_code(&buffer);
        Ok(())
    }

    fn add_aice_command(&mut self, byte: u8) {
        self.add_flow_to_aice();
        self.add_aice_code(&[byte]);
    }

    fn add_aice_command_u8(&mut self, byte: u8, param: u8) {
        self.add_flow_to_aice();
        self.add_aice_code(&[byte, param]);
    }

    fn add_aice_command_u32(&mut self, byte: u8, param: u32) {
        self.add_flow_to_aice();
        let mut buffer = [0u8; 5];
        buffer[0] = byte;
        LittleEndian::write_u32(&mut buffer[1..], param);
        self.add_aice_code(&buffer);
    }

    fn add_bw_command(
        &mut self,
        byte: u8,
        param_text: &'a [u8],
        params: &[BwCommandParam],
        ends_flow: bool,
        block_scope: &BlockScope<'a, '_>,
    ) -> Result<(), Error> {
        let code = &mut [0u8; 16];
        let code_len = code.len();
        code[0] = byte;
        let mut out = &mut code[1..];
        let mut tokens = param_text.fields();
        for param in params {
            let arg = tokens.next()
                .ok_or_else(|| Error::Dynamic(format!("Expected {} arguments", params.len())))?;
            match param {
                BwCommandParam::I8 => {
                    let val = parse_i8(arg).ok_or_else(|| Error::Msg("Expected I8"))?;
                    (&mut out).write_u8(val).unwrap();
                }
                BwCommandParam::U8 => {
                    let val = parse_u8(arg).ok_or_else(|| Error::Msg("Expected U8"))?;
                    (&mut out).write_u8(val).unwrap();
                }
                BwCommandParam::U16 => {
                    let val = parse_u16(arg).ok_or_else(|| Error::Msg("Expected U16"))?;
                    (&mut out).write_u16::<LE>(val).unwrap();
                }
                BwCommandParam::U16VarLen => {
                    let amt = parse_u8(arg).ok_or_else(|| Error::Msg("Expected U8"))?;
                    (&mut out).write_u8(amt).unwrap();
                    for _ in 0..amt {
                        let arg = tokens.next().ok_or_else(|| Error::Msg("Expected more arguments"))?;
                        let val = parse_u16(arg).ok_or_else(|| Error::Msg("Expected U16"))?;
                        (&mut out).write_u16::<LE>(val).unwrap();
                    }
                }
                BwCommandParam::Label => {
                    let label = parse_label_ref(arg).ok_or_else(|| Error::Msg("Expected label"))?;
                    let pos = self.label_location(label, block_scope)
                        .ok_or_else(|| Error::Dynamic(format!("Label '{}' not defined", label.0)))?;
                    if let Some(pos) = pos.if_bw_pos() {
                        let offset_in_command = code_len - out.len();
                        let offset = (self.bw_bytecode.len() + offset_in_command) as u16;
                        self.bw_offsets_in_bw.push(offset);
                        (&mut out).write_u16::<LE>(pos).unwrap();
                        if pos == 0xffff {
                            let pos = CodePosition::bw(offset);
                            let block_id = block_scope.block_id();
                            self.needed_label_positions.push((pos, label, block_id));
                        }
                    } else {
                        panic!("BW jumped label was not in BW?");
                    }
                }
            }
        }
        let len = code_len - out.len();
        if tokens.next().is_some() {
            return Err(Error::Dynamic(format!("Expected {} arguments", params.len())));
        }
        self.add_bw_code(&code[..len]);
        if ends_flow {
            self.bw_code_flow_ends.push(self.bw_bytecode.len() as u16);
        }
        Ok(())
    }

    fn fixup_missing_label_positions(&mut self, blocks: &Blocks<'a>) {
        for &(pos, label, block_id) in &self.needed_label_positions {
            let label_pos = self.label_location(label, &blocks.scope(block_id))
                .unwrap_or_else(|| panic!("Label {} disappeared?", label.0));
            let mut arr = match pos.to_enum() {
                CodePos::Bw(offset) => &mut self.bw_bytecode[offset as usize..],
                CodePos::Aice(offset) => &mut self.aice_bytecode[offset as usize..],
            };
            let _ = match label_pos.to_enum() {
                CodePos::Bw(offset) => arr.write_u16::<LE>(offset),
                CodePos::Aice(offset) => arr.write_u32::<LE>(offset | 0x8000_0000),
            };
        }
    }

    fn compile(
        mut self,
        blocks: &Blocks<'a>,
        headers: &[Header<'a>],
        unit_refs: Vec<Vec<UnitObject>>,
        format_strings: FormatStrings,
    ) -> Result<Iscript, Error> {
        self.fixup_missing_label_positions(blocks);

        if self.bw_bytecode.len() >= 0x10000 {
            return Err(Error::Msg("Iscript too large"));
        }
        debug_assert!(self.bw_code_flow_ends.windows(2).all(|x| x[0] < x[1]));

        let mut bw_data = Vec::with_capacity(32768);
        bw_data.resize(0x1c, AICE_COMMAND);
        bw_data[0x4] = 0x16; // Global end command
        // Global infloop. Wait 0x40 goto loop
        bw_data[0x5] = 0x05;
        bw_data[0x6] = 0x40;
        bw_data[0x7] = 0x07;
        bw_data[0x8] = 0x05;
        bw_data[0x9] = 0x00;
        bw_data.extend((0..headers.len()).flat_map(|_| [
            0x53, 0x43, 0x50, 0x45, 0x1b, 0x00, 0x1b, 0x00,
        ].iter().cloned()));
        // Add animation offsets for the last script
        for _ in 0..0x1c * 2{
            bw_data.push(0x1b);
            bw_data.push(0x00);
        }
        if bw_data.len() > 0x4353 {
            // FIXME
            return Err(Error::Msg("Too many iscript ids"));
        }
        let write_len = 0x4353 - bw_data.len();
        let pos = self.write_bw_code(&mut bw_data, 0, write_len);
        bw_data.push(AICE_COMMAND);
        let write_len = 0x4550 - bw_data.len();
        let pos = self.write_bw_code(&mut bw_data, pos, write_len);
        bw_data.push(AICE_COMMAND);
        self.write_bw_code_rest(&mut bw_data, pos);

        self.fixup_bw_offsets(&mut bw_data);

        if bw_data.len() > 0x10000 {
            return Err(Error::Msg("Iscript too large"));
        }

        // Write header list.
        // Pad so the header list is located at offset 0x__cc
        let padding_length = if bw_data.len() as u8 <= AICE_COMMAND {
            (AICE_COMMAND - bw_data.len() as u8) as usize
        } else {
            0x100 - (bw_data.len() as u8 - AICE_COMMAND) as usize
        };
        bw_data.extend((0..padding_length).map(|_| AICE_COMMAND));
        if bw_data.len() > 0x10000 {
            return Err(Error::Msg("Iscript too large"));
        }
        let header_list_offset = bw_data.len() as u32;
        (&mut bw_data[0..]).write_u32::<LE>(header_list_offset).unwrap();
        for (i, header) in headers.iter().enumerate() {
            bw_data.write_u16::<LE>(header.iscript_id as u16).unwrap();
            bw_data.write_u16::<LE>(0x1c + i as u16 * 8).unwrap();
        }
        bw_data.write_u16::<LE>(0xffff).unwrap();
        bw_data.write_u16::<LE>(0x0000).unwrap();

        // Write aice animation start cmd and link the few places to it
        let anim_start_cmd_offset = self.aice_bytecode.len() as u32;
        self.aice_bytecode.push(aice_op::ANIMATION_START);
        let mut bw_aice_cmd_offsets = self.bw_aice_cmd_offsets.iter()
            .map(|&(bw, aice)| (bw_code_offset_to_data_offset(&self.bw_code_parts, bw), aice))
            .collect::<FxHashMap<u16, u32>>();
        bw_aice_cmd_offsets.insert(0x0000, anim_start_cmd_offset);
        bw_aice_cmd_offsets.insert(0x001b, anim_start_cmd_offset);
        bw_aice_cmd_offsets.insert(0x4353, anim_start_cmd_offset);
        bw_aice_cmd_offsets.insert(0x4550, anim_start_cmd_offset);

        let root_scope = blocks.scope(BlockId(0));
        let headers = headers.iter().map(|header| {
            let animations = header.animations.iter().map(|&label| match label {
                None => Ok(None),
                Some(label) => {
                    if label.0 == &b"[NONE]"[..] {
                        Ok(None)
                    } else {
                        self.label_location(label, &root_scope).ok_or_else(|| {
                            Error::Dynamic(format!("Header label '{}' not found", label.0))
                        }).map(|pos| Some(match pos.to_enum() {
                            CodePos::Bw(pos) => CodePosition::bw(
                                bw_code_offset_to_data_offset(&self.bw_code_parts, pos)
                            ),
                            CodePos::Aice(pos) => CodePosition::aice(pos),
                        }))
                    }
                }
            }).collect::<Result<Vec<_>, Error>>()?;
            Ok(ResolvedHeader {
                iscript_id: u16::try_from(header.iscript_id)
                    .map_err(|_| Error::Msg("Broken header"))?,
                animations,
            })
        }).collect::<Result<Vec<_>, Error>>()?;
        Ok(Iscript {
            bw_data: bw_data.into_boxed_slice(),
            aice_data: self.aice_bytecode,
            conditions: self.expressions.conditions,
            int_expressions: self.expressions.int_expressions,
            sprite_local_sets: self.sprite_local_sets,
            bw_aice_cmd_offsets,
            headers,
            global_count: self.variables.variable_counts[VariableType::Global as usize],
            line_info: self.line_info.finish(),
            unit_refs,
            format_strings,
        })
    }

    fn fixup_bw_offsets(&mut self, bw_data: &mut [u8]) {
        let code_parts = &self.bw_code_parts;
        // Old offset -> offset in bw_data
        for &offset in &self.bw_offsets_in_bw {
            let offset = bw_code_offset_to_data_offset(code_parts, offset) as usize;
            let old = (&bw_data[offset..]).read_u16::<LE>().unwrap();
            let new = bw_code_offset_to_data_offset(code_parts, old);
            (&mut bw_data[offset..]).write_u16::<LE>(new).unwrap();
        }
        for &offset in &self.bw_offsets_in_aice {
            let offset = offset as usize;
            let old = (&self.aice_bytecode[offset..]).read_u16::<LE>().unwrap();
            let new = bw_code_offset_to_data_offset(code_parts, old);
            (&mut self.aice_bytecode[offset..]).write_u16::<LE>(new).unwrap();
        }
    }

    fn write_bw_code(&mut self, out: &mut Vec<u8>, start: usize, length: usize) -> usize {
        let write_length = if start + length > self.bw_bytecode.len() {
            self.bw_bytecode.len() - start
        } else {
            let index = self.bw_code_flow_ends.binary_search(&((start + length) as u16))
                .unwrap_or_else(|e| e);
            if index == 0 {
                0
            } else {
                self.bw_code_flow_ends[index - 1] as usize - start
            }
        };
        assert!(write_length <= length);
        self.bw_code_parts.push((out.len() as u16, write_length as u16));
        out.extend_from_slice(&self.bw_bytecode[start..start + write_length]);
        out.extend((write_length..length).map(|_| AICE_COMMAND));
        start + write_length
    }

    fn write_bw_code_rest(&mut self, out: &mut Vec<u8>, start: usize) {
        let write_length = self.bw_bytecode.len() - start;
        if write_length == 0 {
            return;
        }
        self.bw_code_parts.push((out.len() as u16, write_length as u16));
        out.extend_from_slice(&self.bw_bytecode[start..start + write_length]);
    }
}

#[cfg(test)]
impl<'a> CompilerVariables<'a> {
    fn for_test(input: &[(&'a [u8], VariableType)]) -> Self {
        let mut result = CompilerVariables {
            variables: Default::default(),
            variable_counts: [0, 0],
        };
        for &(name, ty) in input {
            let id = result.variable_counts[ty as usize] as u32;
            result.variable_counts[ty as usize] += 1;
            let place_id = match ty {
                VariableType::Global => PlaceId::new_global(id),
                VariableType::SpriteLocal => PlaceId::new_spritelocal(id, UnitRefId::this()),
            };
            let place_id = place_id.ok_or_else(|| Error::Overflow).unwrap();
            result.variables.insert(name, place_id);
        }
        result
    }
}

impl CompilerExprs {
    fn int_expr_id(&mut self, value: IntExpr) -> u32 {
        match self.existing_int_expressions.get(&value).cloned() {
            Some(s) => s,
            None => {
                let index = self.int_expressions.len() as u32;
                let rc = Rc::new(value);
                self.int_expressions.push(rc.clone());
                self.existing_int_expressions.insert(rc, index);
                index
            }
        }
    }

    fn bool_expr_id(&mut self, value: BoolExpr) -> u32 {
        match self.existing_conditions.get(&value).cloned() {
            Some(s) => s,
            None => {
                let index = self.conditions.len() as u32;
                let rc = Rc::new(value);
                self.conditions.push(rc.clone());
                self.existing_conditions.insert(rc, index);
                index
            }
        }
    }
}

fn bw_code_offset_to_data_offset(code_parts: &[(u16, u16)], offset: u16) -> u16 {
    let mut pos = 0;
    for &(start, len) in code_parts {
        if offset < pos + len {
            return start + (offset - pos)
        }
        pos += len;
    }
    panic!("Offset {:x} not in bw code", offset);
}


#[derive(Debug)]
pub struct ErrorWithLine {
    pub error: Error,
    pub line: Option<usize>,
}

/// Results of the first parse pass.
/// Immutable after that parse.
struct ParseStage1<'a> {
    blocks: Blocks<'a>,
    /// Unparsed lines of `var_name = expr` and line number.
    sprite_local_sets: VecOfVecs<SpriteLocalSetId, (&'a [u8], usize)>,
    variable_types: FxHashMap<&'a [u8], VariableType>,
}

struct Blocks<'a> {
    blocks: Vec<Block<'a>>,
    /// Indices of blocks that aren't inlined
    root_blocks: Vec<BlockId>,
}

/// Borrow of all blocks with specific block selected.
struct BlockScope<'a, 'b> {
    blocks: &'b Blocks<'a>,
    block: BlockId,
}

impl<'a: 'b, 'b> BlockScope<'a, 'b> {
    fn block_id(&self) -> BlockId {
        self.block
    }

    fn this_and_ancestor_ids(&self) -> ThisAndAncestorIds<'a, 'b> {
        ThisAndAncestorIds {
            blocks: self.blocks,
            next: self.block,
        }
    }
}

struct ThisAndAncestorIds<'a, 'b> {
    blocks: &'b Blocks<'a>,
    next: BlockId,
}

impl<'a, 'b> Iterator for ThisAndAncestorIds<'a, 'b> {
    type Item = BlockId;
    fn next(&mut self) -> Option<Self::Item> {
        if self.next.0 == u32::max_value() {
            None
        } else {
            let next = self.next;
            self.next = self.blocks.blocks[next.0 as usize].parent;
            Some(next)
        }
    }
}

struct Block<'a> {
    lines: Vec<PartiallyParsedLine<'a>>,
    /// Used for label scopes.
    parent: BlockId,
    children: Vec<BlockId>,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct BlockId(u32);

/// Index to sprite_local_sets in Blocks / Compiler / Iscript. 1-based; 0 may be
/// used for None.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct SpriteLocalSetId(pub u32);

impl VecIndex for SpriteLocalSetId {
    #[inline(always)]
    fn index(&self) -> Option<usize> {
        (self.0 as usize).checked_sub(1)
    }

    #[inline(always)]
    fn from_index(index: u32) -> Option<Self> {
        index.checked_add(1).map(SpriteLocalSetId)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum LinkedBlock {
    SpriteLocals(SpriteLocalSetId),
    None,
}

enum BlockLine<'a> {
    /// Normal line (Command, rest, linked_block if @{})
    Command(CommandPrototype, &'a [u8], LinkedBlock),
    /// Execution switches to another block (e.g. not @{} block)
    InlineBlock(BlockId),
    Label(Label<'a>),
}

struct PartiallyParsedLine<'a> {
    ty: BlockLine<'a>,
    // For error reporting
    line_number: usize,
}

impl<'a> Block<'a> {
    fn dummy() -> Block<'a> {
        Block {
            lines: Vec::new(),
            parent: BlockId(0),
            children: Vec::new(),
        }
    }
}

impl<'a> Blocks<'a> {
    fn scope<'b>(&'b self, id: BlockId) -> BlockScope<'a, 'b> {
        BlockScope {
            blocks: self,
            block: id,
        }
    }

    fn iter_block_lines<'b, F>(&'b self, id: BlockId, mut callback: F)
    where F: FnMut(&BlockScope<'a, 'b>, &PartiallyParsedLine<'a>),
    {
        let mut stack = vec![(id, 0)];
        while let Some((id, start)) = stack.pop() {
            let block = &self.blocks[id.0 as usize];
            let mut pos = start;
            let scope = self.scope(id);
            'current_block_loop: while pos < block.lines.len() {
                let line = &block.lines[pos];
                pos += 1;
                if let BlockLine::InlineBlock(child_id) = line.ty {
                    stack.push((id, pos));
                    stack.push((child_id, 0));
                    break 'current_block_loop;
                }
                callback(&scope, line);
            }
        }
    }

    fn start_inline_block(&mut self, ctx: &mut TextParseContext<'a>) {
        let block_id = BlockId(self.blocks.len() as u32);
        self.blocks.push(Block::dummy());
        assert!(self.blocks.len() < u32::max_value() as usize);
        ctx.current_block.0.children.push(block_id);
        let new_block = Block {
            lines: Vec::with_capacity(64),
            parent: ctx.current_block.1,
            children: Vec::new(),
        };
        ctx.current_block.0.lines.push(PartiallyParsedLine {
            ty: BlockLine::InlineBlock(block_id),
            line_number: ctx.line_number,
        });
        ctx.block_stack.push((
            mem::replace(&mut ctx.current_block.0, new_block),
            mem::replace(&mut ctx.current_block.1, block_id),
            mem::replace(&mut ctx.current_block.2, ctx.line_number),
        ));
    }
}

fn add_set_decl<'a>(
    variable_types: &mut FxHashMap<&'a [u8], VariableType>,
    var_name: &'a [u8],
    ty: VariableType,
) -> Result<(), Error> {
    let old = variable_types.entry(var_name)
        .or_insert(ty);
    if *old != ty {
        Err(Error::Dynamic(format!(
            "Conflicting variable type for '{}', was previously used with {:?}",
            var_name.as_bstr(), old,
        )))
    } else {
        Ok(())
    }
}

pub fn compile_iscript_txt(text: &[u8]) -> Result<Iscript, Vec<ErrorWithLine>> {
    let mut compiler = Compiler::new();
    let mut parser = Parser::new();
    let mut errors = Vec::new();

    let stage1 = parser.parse_text(text, &mut compiler)?;
    let blocks = &stage1.blocks;
    if let Err(error) = compiler.add_variables(&stage1.variable_types) {
        errors.push(ErrorWithLine {
            error,
            line: None,
        });
        return Err(errors);
    }
    for &block_id in &blocks.root_blocks {
        blocks.iter_block_lines(block_id, |block, line| {
            parser.exprs.clear_current_error();
            if let Err(error) = parser.parse_line_add_label(line, &block, &mut compiler) {
                let error = parser.exprs.current_error.take().unwrap_or(error);
                errors.push(ErrorWithLine {
                    error,
                    line: Some(line.line_number),
                });
            }
        });
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    let bump = Bump::new();
    let ctx = &mut CompileContext {
        expr_buffer: [None, None, None, None],
        write_buffer: Vec::with_capacity(64),
        error_line: 0,
        anytype_parser: AnyTypeParser::new(&bump),
    };

    for &block_id in &blocks.root_blocks {
        blocks.iter_block_lines(block_id, |block, line| {
            parser.exprs.clear_current_error();
            ctx.error_line = line.line_number;
            if let Err(error) =
                parser.parse_line_pass3(line, &block, &mut compiler, &stage1, ctx)
            {
                let error = parser.exprs.current_error.take().unwrap_or(error);
                errors.push(ErrorWithLine {
                    error,
                    line: Some(ctx.error_line),
                });
            }
        });
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    let script = match compiler.compile(
        &blocks,
        &parser.headers,
        parser.exprs.unit_refs.finish(),
        parser.format_strings,
    ) {
        Ok(o) => o,
        Err(error) => {
            errors.push(ErrorWithLine {
                error,
                line: None,
            });
            return Err(errors);
        }
    };
    Ok(script)
}
