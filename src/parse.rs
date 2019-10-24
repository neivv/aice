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

use std::collections::hash_map::Entry;
use std::convert::TryFrom;
use std::fmt;
use std::mem;
use std::rc::Rc;

use bstr::{ByteSlice, BString, BStr};
use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use fxhash::{FxHashMap, FxHashSet};
use quick_error::quick_error;
use serde_derive::{Serialize, Deserialize};

use bw_dat::expr::{self, CustomBoolExpr, CustomIntExpr};

pub type IntExpr = CustomIntExpr<ExprState>;
pub type BoolExpr = CustomBoolExpr<ExprState>;

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct ExprState;

impl expr::CustomState for ExprState {
    type IntExt = Int;
    type BoolExt = Bool;
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Int {
    Variable(PlaceId),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Bool {
}

struct ExprParser<'a> {
    compiler: &'a Compiler<'a>,
    parser: &'a Parser<'a>,
}

impl<'b> expr::CustomParser for ExprParser<'b> {
    type State = ExprState;
    fn parse_int<'a>(&mut self, input: &'a [u8]) -> Option<(Int, &'a [u8])> {
        let word_end = input.iter().position(|&x| x == b' ')
            .unwrap_or(input.len());
        let word = &input[..word_end];
        if let Some(&var) = self.compiler.variables.get(word) {
            return Some((Int::Variable(var), &input[word_end..]));
        }
        if let Some(&var) = self.parser.bw_places.get(word) {
            return Some((Int::Variable(var), &input[word_end..]));
        }
        None
    }

    fn parse_bool<'a>(&mut self, _input: &'a [u8]) -> Option<(Bool, &'a [u8])> {
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
        Dynamic(msg: String) {
            display("{}", msg)
        }
        Msg(msg: &'static str) {
            display("{}", msg)
        }
        Expr(e: bw_dat::expr::Error) {
            from()
            display("Couldn't parse expression: {}", e)
        }
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
        (b"playfram", bw_cmd(0x00, &[U16])),
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
        (b"sigorder", bw_cmd(0x24, &[U8])),
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
        (b"call", bw_cmd(0x35, &[Label])),
        (b"return", bw_cmd_final(0x36, &[])),
        (b"setflspeed", bw_cmd(0x37, &[U16])),
        (b"creategasoverlays", bw_cmd(0x38, &[U8])),
        (b"pwrupcondjmp", bw_cmd(0x39, &[Label])),
        (b"trgtrangecondjmp", bw_cmd(0x3a, &[U16, Label])),
        (b"trgtarccondjmp", bw_cmd(0x3b, &[U16, U16, Label])),
        (b"curdirectcondjmp", bw_cmd(0x3c, &[U16, U16, Label])),
        (b"imgulnextid", bw_cmd(0x3d, &[I8, I8])),
        (b"liftoffcondjmp", bw_cmd(0x3f, &[Label])),
        (b"warpoverlay", bw_cmd(0x40, &[U16])),
        (b"orderdone", bw_cmd(0x41, &[U8])),
        (b"grdsprol", bw_cmd(0x42, &[U16, I8, I8])),
        (b"__43", bw_cmd(0x43, &[])),
        (b"dogrddamage", bw_cmd(0x44, &[])),
        (b"if", If),
        (b"set", Set),
        (b"fireweapon", FireWeapon),
    ]
};

static BW_PLACES: &[(&[u8], PlaceId)] = {
    use self::FlingyVar::*;
    use self::BulletVar::*;
    const fn flingy(x: FlingyVar) -> PlaceId {
        PlaceId::new_flingy(x)
    }
    const fn bullet(x: BulletVar) -> PlaceId {
        PlaceId::new_bullet(x)
    }
    &[
        (b"flingy.move_target_x", flingy(MoveTargetX)),
        (b"flingy.move_target_y", flingy(MoveTargetY)),
        (b"flingy.position_x", flingy(PositionX)),
        (b"flingy.position_y", flingy(PositionY)),
        (b"flingy.facing_direction", flingy(FacingDirection)),
        (b"flingy.movement_direction", flingy(MovementDirection)),
        (b"flingy.target_direction", flingy(TargetDirection)),
        (b"flingy.turn_speed", flingy(TurnSpeed)),
        (b"flingy.acceleration", flingy(Acceleration)),
        (b"flingy.top_speed", flingy(TopSpeed)),
        (b"flingy.speed", flingy(Speed)),
        (b"bullet.weapon_id", bullet(WeaponId)),
        (b"bullet.death_timer", bullet(DeathTimer)),
        (b"bullet.state", bullet(State)),
        (b"bullet.bounces_remaining", bullet(BouncesRemaining)),
        (b"bullet.order_target_x", bullet(OrderTargetX)),
        (b"bullet.order_target_y", bullet(OrderTargetY)),
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
}

pub struct Iscript {
    pub bw_data: Box<[u8]>,
    pub aice_data: Vec<u8>,
    pub bw_aice_cmd_offsets: FxHashMap<u16, u32>,
    pub headers: Vec<ResolvedHeader>,
    pub conditions: Vec<Rc<BoolExpr>>,
    pub int_expressions: Vec<Rc<IntExpr>>,
    pub global_count: u32,
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
    variable_types: FxHashMap<&'a [u8], VariableType>,
    bw_places: FxHashMap<&'static [u8], PlaceId>,
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
            variable_types: FxHashMap::with_capacity_and_hasher(32, Default::default()),
            bw_places: BW_PLACES.iter().cloned().collect(),
        }
    }

    /// Separates blocks out of the text, parses headers.
    fn parse_text(
        &mut self,
        text: &'a [u8],
        compiler: &mut Compiler<'a>,
    ) -> Result<Blocks<'a>, Vec<ErrorWithLine>> {
        let mut errors = Vec::new();
        let mut ctx = TextParseContext {
            text,
            block_stack: Vec::with_capacity(4),
            line_number: 0,
            current_block: (Block {
                lines: Vec::with_capacity(32000),
                parent: BlockId(!0),
                children: Vec::new(),
            }, BlockId(0), 1),
            is_continuing_commands: false,
        };
        let mut blocks = Blocks {
            blocks: vec![Block::dummy()],
            root_blocks: vec![BlockId(0)],
        };
        while !ctx.text.is_empty() {
            ctx.line_number += 1;
            let line_end = ctx.text.find_byte(b'\n').unwrap_or_else(|| ctx.text.len());
            let mut line = &ctx.text[..line_end];
            ctx.text = ctx.text.get(line_end + 1..).unwrap_or_else(|| b"");
            // Comment
            if let Some(comment_start) = line.find_byte(b'#') {
                line = &line[..comment_start];
            };
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            if line == b"}" {
                if let Some((block, index, start_line)) = ctx.block_stack.pop() {
                    blocks.blocks[(ctx.current_block.1).0 as usize] =
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
                if let Err(error) = self.parse_line(line, compiler, &mut blocks, &mut ctx) {
                    errors.push(ErrorWithLine {
                        error,
                        line: Some(ctx.line_number),
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
        blocks.blocks[(ctx.current_block.1).0 as usize] = ctx.current_block.0;
        if errors.is_empty() {
            Ok(blocks)
        } else {
            Err(errors)
        }
    }

    fn parse_line(
        &mut self,
        line: &'a [u8],
        compiler: &mut Compiler<'a>,
        blocks: &mut Blocks<'a>,
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
            self.parse_line_command(line, compiler, blocks, ctx)
        }
    }

    fn parse_line_command(
        &mut self,
        line: &'a [u8],
        compiler: &mut Compiler<'a>,
        blocks: &mut Blocks<'a>,
        ctx: &mut TextParseContext<'a>,
    ) -> Result<(), Error> {
        if let Some((label, block_start)) = parse_label(line)? {
            ctx.current_block.0.lines.push(PartiallyParsedLine {
                ty: BlockLine::Label(label),
                line_number: ctx.line_number,
            });
            if block_start {
                blocks.start_inline_block(ctx);
            }
            return Ok(());
        }
        if line == b"{" {
            blocks.start_inline_block(ctx);
            return Ok(());
        }
        let command = line.fields().next().ok_or_else(|| Error::Msg("Empty line???"))?;
        let rest = (&line[command.len()..]).trim_start();
        match self.command_map.get(&command) {
            Some(&command) => {
                match command {
                    CommandPrototype::BwCommand(_, commands, ends_flow) => {
                        mark_required_bw_labels(rest, commands, compiler, ctx)?;
                        ctx.is_continuing_commands = !ends_flow;
                    }
                    CommandPrototype::End => {
                        ctx.is_continuing_commands = false;
                    }
                    CommandPrototype::Set => {
                        let (place, _rest) = self.parse_set_place(rest)?;
                        if let ParsedPlace::Variable(ty, var_name) = place {
                            self.add_set_decl(var_name, ty)?;
                        }
                        ctx.is_continuing_commands = true;
                    }
                    _ => {
                        ctx.is_continuing_commands = true;
                    }
                }
                ctx.current_block.0.lines.push(PartiallyParsedLine {
                    ty: BlockLine::Command(command, rest, None),
                    line_number: ctx.line_number,
                });
            }
            None => return Err(Error::UnknownCommand(command.into())),
        }
        Ok(())
    }

    fn add_set_decl(&mut self, var_name: &'a [u8], ty: VariableType) -> Result<(), Error> {
        let old = self.variable_types.entry(var_name)
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
    ) -> Result<(), Error> {
        match line.ty {
            BlockLine::Label(label) => {
                compiler.set_label_position_to_current(label, block_scope);
                Ok(())
            }
            BlockLine::Command(command, rest, _) => match command {
                CommandPrototype::BwCommand(op, params, ends_flow) => {
                    compiler.add_bw_command(op, rest, params, ends_flow, block_scope)?;
                    Ok(())
                }
                CommandPrototype::If => {
                    let (condition, rest) = parse_bool_expr(rest, self, &compiler)?;
                    let mut tokens = rest.fields();
                    let next = tokens.next();
                    if next != Some(b"goto") {
                        return Err(Error::Dynamic(format!("Expected 'goto', got {:?}", next)));
                    }
                    let label = tokens.next()
                        .and_then(|l| parse_label_ref(l))
                        .ok_or_else(|| Error::Msg("Expected label after goto"))?;
                    compiler.add_if(condition, label, block_scope)
                }
                CommandPrototype::FireWeapon => {
                    let (expr, rest) = parse_int_expr(rest, self, &compiler)?;
                    if !rest.is_empty() {
                        return Err(
                            Error::Dynamic(format!("Trailing characters '{}'", rest.as_bstr()))
                        );
                    }
                    let id = compiler.int_expr_id(expr);
                    compiler.add_aice_command_u32(aice_op::SET_ORDER_WEAPON, id);
                    // castspell
                    compiler.add_bw_code(&[0x27]);
                    compiler.add_aice_command_u32(aice_op::SET_ORDER_WEAPON, !0);
                    Ok(())
                }
                CommandPrototype::GotoRepeatAttk => {
                    compiler.add_aice_command(aice_op::CLEAR_ATTACKING_FLAG);
                    Ok(())
                }
                CommandPrototype::End => {
                    compiler.add_aice_command(aice_op::PRE_END);
                    compiler.flow_to_bw();
                    Ok(())
                }
                CommandPrototype::Set => {
                    let (place, rest) = self.parse_set_place(rest)?;
                    let (expr, rest) = parse_int_expr(rest, self, &compiler)?;
                    if !rest.is_empty() {
                        return Err(
                            Error::Dynamic(format!("Trailing characters '{}'", rest.as_bstr()))
                        );
                    }
                    compiler.add_set(place, expr);
                    Ok(())
                }
            }
            _ => Ok(())
        }
    }

    fn parse_set_place(&self, text: &'a [u8]) -> Result<(ParsedPlace<'a>, &'a [u8]), Error> {
        let (place, rest) = split_first_token(text)
            .ok_or_else(|| Error::Msg("Expected place"))?;
        let ty = match place {
            x if x == b"global" => Some(VariableType::Global),
            x if x == b"spritelocal" => Some(VariableType::SpriteLocal),
            _ => None,
        };
        if let Some(ty) = ty {
            let (name, rest) = split_first_token(rest)
                .ok_or_else(|| Error::Msg("Expected variable name"))?;
            let (_, rest) = split_first_token(rest)
                .filter(|x| x.0 == b"=")
                .ok_or_else(|| Error::Msg("Expected '='"))?;
            return Ok((ParsedPlace::Variable(ty, name), rest));
        }
        let (_, rest) = split_first_token(rest)
            .filter(|x| x.0 == b"=")
            .ok_or_else(|| Error::Msg("Expected '='"))?;
        match self.bw_places.get(place.as_bytes()) {
            Some(&s) => Ok((ParsedPlace::Bw(s), rest)),
            None => Err(Error::Dynamic(format!("Unknown variable type '{}'", place.as_bstr()))),
        }
    }
}

struct TextParseContext<'a> {
    // Block, index, block start line
    block_stack: Vec<(Block<'a>, BlockId, usize)>,
    current_block: (Block<'a>, BlockId, usize),
    text: &'a [u8],
    line_number: usize,
    is_continuing_commands: bool,
}

fn split_first_token(text: &[u8]) -> Option<(&[u8], &[u8])> {
    let start = text.bytes().position(|x| x != b' ' && x != b'\t')?;
    let end = start + text.bytes().skip(start).position(|x| x == b' ' || x == b'\t')?;
    let first = &text[start..end];
    Some(match text.bytes().skip(end).position(|x| x != b' ' && x != b'\t') {
        Some(s) => (first, &text[end + s..]),
        None => (first, b""),
    })
}

fn parse_bool_expr<'a>(
    text: &'a [u8],
    parser: &Parser<'a>,
    compiler: &Compiler<'a>,
) -> Result<(BoolExpr, &'a [u8]), Error> {
    let mut parser = ExprParser {
        compiler,
        parser,
    };
    CustomBoolExpr::parse_part_custom(text.as_ref(), &mut parser)
        .map(|(a, b)| (a, b))
        .map_err(|e| e.into())
}

fn parse_int_expr<'a>(
    text: &'a [u8],
    parser: &Parser<'a>,
    compiler: &Compiler<'a>,
) -> Result<(IntExpr, &'a [u8]), Error> {
    let mut parser = ExprParser {
        compiler,
        parser,
    };
    CustomIntExpr::parse_part_custom(text.as_ref(), &mut parser)
        .map(|(a, b)| (a, b))
        .map_err(|e| e.into())
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
    conditions: Vec<Rc<BoolExpr>>,
    existing_conditions: FxHashMap<Rc<BoolExpr>, u32>,
    int_expressions: Vec<Rc<IntExpr>>,
    existing_int_expressions: FxHashMap<Rc<IntExpr>, u32>,
    variables: FxHashMap<&'a [u8], PlaceId>,
    variable_counts: [u32; 2],
    flow_in_bw: bool,
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

/// 0xc000_0000 == tag
#[derive(Copy, Clone, Serialize, Deserialize, Debug, Eq, PartialEq, Hash)]
pub struct PlaceId(pub u32);

enum ParsedPlace<'a> {
    Variable(VariableType, &'a [u8]),
    Bw(PlaceId),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum VariableType {
    Global,
    SpriteLocal,
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

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Place {
    Global(u32),
    SpriteLocal(u32),
    Flingy(FlingyVar),
    Bullet(BulletVar),
}

impl PlaceId {
    fn new(place: Place) -> PlaceId {
        let (tag, id) = match place {
            Place::Global(id) => (0, id),
            Place::SpriteLocal(id) => (1, id),
            Place::Flingy(id) => (2, id as u32),
            Place::Bullet(id) => (3, id as u32),
        };
        assert!(id & 0xc000_0000 == 0);
        PlaceId(id | ((tag as u32) << 30))
    }

    const fn new_flingy(x: FlingyVar) -> PlaceId {
        PlaceId((x as u32) | (2 << 30))
    }
    const fn new_bullet(x: BulletVar) -> PlaceId {
        PlaceId((x as u32) | (3 << 30))
    }

    pub fn place(self) -> Place {
        let id = self.0 & !0xc000_0000;
        match self.0 >> 30 {
            0 => Place::Global(id),
            1 => Place::SpriteLocal(id),
            2 => Place::Flingy(unsafe { std::mem::transmute(id as u8) }),
            3 | _ => Place::Bullet(unsafe { std::mem::transmute(id as u8) }),
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
            conditions: Vec::with_capacity(16),
            existing_conditions: FxHashMap::with_capacity_and_hasher(16, Default::default()),
            int_expressions: Vec::with_capacity(16),
            existing_int_expressions: FxHashMap::with_capacity_and_hasher(16, Default::default()),
            variables: FxHashMap::default(),
            variable_counts: [0; 2],
            flow_in_bw: true,
        }
    }

    fn code_position(&self) -> CodePosition {
        if self.flow_in_bw {
            CodePosition(self.bw_bytecode.len() as u32)
        } else {
            CodePosition(self.aice_bytecode.len() as u32 | 0x8000_0000)
        }
    }

    fn add_variables(&mut self, variables: &FxHashMap<&'a [u8], VariableType>) {
        self.variables.reserve(variables.len());
        for (&name, &ty) in variables {
            let id = self.variable_counts[ty as usize] as u32;
            self.variable_counts[ty as usize] += 1;
            self.variables.insert(name, match ty {
                VariableType::Global => PlaceId::new(Place::Global(id)),
                VariableType::SpriteLocal => PlaceId::new(Place::SpriteLocal(id)),
            });
        }
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
        self.bw_bytecode.extend_from_slice(code);
    }

    fn emit_aice_jump_to_bw(&mut self, to: u16) {
        self.bw_offsets_in_aice.push(self.aice_bytecode.len() as u32 + 1);
        self.aice_bytecode
            .extend_from_slice(&[aice_op::JUMP_TO_BW, to as u8, (to >> 8) as u8]);
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

    fn add_set(&mut self, place: ParsedPlace, value: IntExpr) {
        self.add_flow_to_aice();
        let index = self.int_expr_id(value);
        let var_id = match place {
            ParsedPlace::Variable(_ty, var_name) => *self.variables.get(var_name).unwrap(),
            ParsedPlace::Bw(place) => place,
        };
        self.aice_bytecode.push(aice_op::SET);
        self.aice_bytecode.write_u32::<LE>(var_id.0).unwrap();
        self.aice_bytecode.write_u32::<LE>(index as u32).unwrap();
    }

    fn add_if(
        &mut self,
        condition: BoolExpr,
        dest: Label<'a>,
        block_scope: &BlockScope<'a, '_>
    ) -> Result<(), Error> {
        self.add_flow_to_aice();
        let index = match self.existing_conditions.get(&condition).cloned() {
            Some(s) => s,
            None => {
                let index = self.conditions.len() as u32;
                let rc = Rc::new(condition);
                self.conditions.push(rc.clone());
                self.existing_conditions.insert(rc, index);
                index
            }
        };
        let pos = self.label_location(dest, block_scope)
            .ok_or_else(|| Error::Dynamic(format!("Label '{}' not defined", dest.0)))?;

        self.aice_bytecode.push(aice_op::IF);
        self.aice_bytecode.write_u32::<LE>(index as u32).unwrap();
        self.aice_bytecode.write_u32::<LE>(pos.0).unwrap();
        let offset = self.aice_bytecode.len() as u32 - 4;
        if pos.is_unknown() {
            let block_id = block_scope.block_id();
            self.needed_label_positions.push((CodePosition::aice(offset), dest, block_id));
        }
        if pos.if_bw_pos().is_some() {
            self.bw_offsets_in_aice.push(offset);
        }
        Ok(())
    }

    fn add_aice_command(&mut self, byte: u8) {
        self.add_flow_to_aice();
        self.aice_bytecode.push(byte);
    }

    fn add_aice_command_u32(&mut self, byte: u8, param: u32) {
        self.add_flow_to_aice();
        self.aice_bytecode.push(byte);
        self.aice_bytecode.write_u32::<LE>(param).unwrap();
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

    fn compile(mut self, blocks: &Blocks<'a>, headers: &[Header<'a>]) -> Result<Iscript, Error> {
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
            conditions: self.conditions,
            int_expressions: self.int_expressions,
            bw_aice_cmd_offsets,
            headers,
            global_count: self.variable_counts[VariableType::Global as usize],
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

enum BlockLine<'a> {
    /// Normal line (Command, rest, linked_block if @{})
    Command(CommandPrototype, &'a [u8], Option<BlockId>),
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

pub fn compile_iscript_txt(text: &[u8]) -> Result<Iscript, Vec<ErrorWithLine>> {
    let mut compiler = Compiler::new();
    let mut parser = Parser::new();
    let mut errors = Vec::new();

    let blocks = parser.parse_text(text, &mut compiler)?;
    compiler.add_variables(&parser.variable_types);
    for &block_id in &blocks.root_blocks {
        blocks.iter_block_lines(block_id, |block, line| {
            if let Err(error) = parser.parse_line_add_label(line, &block, &mut compiler) {
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
    for &block_id in &blocks.root_blocks {
        blocks.iter_block_lines(block_id, |block, line| {
            if let Err(error) = parser.parse_line_pass3(line, &block, &mut compiler) {
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
    let script = match compiler.compile(&blocks, &parser.headers) {
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

#[cfg(test)]
mod test {
    use super::*;

    use bw_dat::expr::{IntFunc, IntFuncType, BoolExprTree, IntExprTree};

    fn read(filename: &str) -> Vec<u8> {
        std::fs::read(format!("test_scripts/{}", filename)).unwrap()
    }

    fn compile_success(filename: &str) -> Iscript {
        let text = read(filename);
        match compile_iscript_txt(&text) {
            Ok(o) => o,
            Err(errors) => {
                for e in &errors {
                    if let Some(line) = e.line {
                        println!("Line {}: {}", line, e.error);
                    } else {
                        println!("{}", e.error);
                    }
                }
                panic!("{} errors", errors.len());
            }
        }
    }

    fn compile_err(filename: &str) -> Vec<ErrorWithLine> {
        let text = read(filename);
        match compile_iscript_txt(&text) {
            Ok(_) => panic!("Compilation succeeded"),
            Err(errors) => errors,
        }
    }

    fn find_error(errors: &mut Vec<ErrorWithLine>, substring: &str, line: usize) {
        let index = errors.iter()
            .position(|x| {
                if line != !0 {
                    if x.line != Some(line) {
                        return false;
                    }
                } else {
                    if x.line != None {
                        return false;
                    }
                }
                let text = format!("{}", x.error);
                text.contains(substring)
            });
        match index {
            Some(s) => {
                errors.remove(s);
            }
            None => {
                println!("Errors: {:#?}", errors);
                panic!("Couldn't find error '{}' @ line {}", substring, line);
            }
        }
    }

    #[test]
    fn parse_vanilla() {
        let iscript = compile_success("vanilla.txt");
        assert_eq!(iscript.bw_data[0], AICE_COMMAND);
        assert_eq!(iscript.bw_data[0x4], 0x16);
        assert_eq!(iscript.bw_data[0x1b], AICE_COMMAND);
        assert_eq!(iscript.bw_data[0x4550], AICE_COMMAND);
        assert_eq!(iscript.bw_data[0x4353], AICE_COMMAND);
    }

    #[test]
    fn parse_if1() {
        let iscript = compile_success("if1.txt");
        assert_eq!(iscript.conditions.len(), 1);
        let expr = BoolExprTree::EqualInt(Box::new((
            IntExprTree::Func(IntFunc {
                ty: IntFuncType::UnitId,
                args: Box::new([]),
            }),
            IntExprTree::Integer(53),
        )));
        assert_eq!(*iscript.conditions[0].inner(), expr);
    }

    #[test]
    fn test1() {
        let iscript = compile_success("test1.txt");
        assert_eq!(iscript.bw_data[0], AICE_COMMAND);
        assert_eq!(iscript.bw_data[0x4], 0x16);
        assert_eq!(iscript.bw_data[0x1b], AICE_COMMAND);
        assert_eq!(iscript.bw_data[0x4550], AICE_COMMAND);
        assert_eq!(iscript.bw_data[0x4353], AICE_COMMAND);
    }

    #[test]
    fn vars() {
        let iscript = compile_success("vars.txt");
        assert_eq!(iscript.global_count, 1);
    }

    #[test]
    fn blocks_ok() {
        compile_success("blocks_ok.txt");
    }

    #[test]
    fn blocks_err() {
        let mut errors = compile_err("blocks_err.txt");
        find_error(&mut errors, "'ScourgeDeath' not found", !0);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_parse_i8() {
        assert_eq!(parse_i8(b"3"), Some(3));
        assert_eq!(parse_i8(b"127"), Some(127));
        assert_eq!(parse_i8(b"128"), Some(128));
        assert_eq!(parse_i8(b"-128"), Some(128));
        assert_eq!(parse_i8(b"-1"), Some(255));
        assert_eq!(parse_i8(b"255"), Some(255));
        assert_eq!(parse_i8(b"-129"), None);
        assert_eq!(parse_i8(b"256"), None);
        assert_eq!(parse_i8(b"255"), Some(255));
        assert_eq!(parse_i8(b"-0x10"), Some(240));
        assert_eq!(parse_i8(b"-16"), Some(240));
    }
}
