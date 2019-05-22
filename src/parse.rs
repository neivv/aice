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
use std::rc::Rc;

use bstr::{BStr, BString};
use byteorder::{ReadBytesExt, WriteBytesExt, LE};
use fxhash::{FxHashMap, FxHashSet};
use quick_error::quick_error;
use serde_derive::{Serialize, Deserialize};

use bw_dat::expr::{BoolExpr, IntExpr};

pub mod aice_op {
    pub const JUMP_TO_BW: u8 = 0x00;
    pub const ANIMATION_START: u8 = 0x01;
    pub const IF: u8 = 0x02;
    pub const PRE_END: u8 = 0x03;
    pub const SET: u8 = 0x04;
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
            display("{}", e)
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
        (b"sethorpos", bw_cmd(0x02, &[U8])),
        (b"setvertpos", bw_cmd(0x03, &[U8])),
        (b"setpos", bw_cmd(0x04, &[U8, U8])),
        (b"wait", bw_cmd(0x05, &[U8])),
        (b"waitrand", bw_cmd(0x06, &[U8, U8])),
        (b"goto", bw_cmd_final(0x07, &[Label])),
        (b"imgol", bw_cmd(0x08, &[U16, U8, U8])),
        (b"imgul", bw_cmd(0x09, &[U16, U8, U8])),
        (b"imgolorig", bw_cmd(0x0a, &[U16])),
        (b"switchul", bw_cmd(0x0b, &[U16])),
        (b"imgoluselo", bw_cmd(0x0d, &[U16, U8, U8])),
        (b"imguluselo", bw_cmd(0x0e, &[U16, U8, U8])),
        (b"sprol", bw_cmd(0x0f, &[U16, U8, U8])),
        (b"highsprol", bw_cmd(0x10, &[U16, U8, U8])),
        (b"lowsprul", bw_cmd(0x11, &[U16, U8, U8])),
        (b"spruluselo", bw_cmd(0x13, &[U16, U8, U8])),
        (b"sprul", bw_cmd(0x14, &[U16, U8, U8])),
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
        (b"gotorepeatattk", bw_cmd(0x2a, &[])),
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
        (b"imgulnextid", bw_cmd(0x3d, &[U8, U8])),
        (b"liftoffcondjmp", bw_cmd(0x3f, &[Label])),
        (b"warpoverlay", bw_cmd(0x40, &[U16])),
        (b"orderdone", bw_cmd(0x41, &[U8])),
        (b"grdsprol", bw_cmd(0x42, &[U16, U8, U8])),
        (b"__43", bw_cmd(0x43, &[])),
        (b"dogrddamage", bw_cmd(0x44, &[])),
        (b"if", If),
        (b"set", Set),
    ]
};

#[derive(Copy, Clone)]
enum CommandPrototype {
    BwCommand(u8, &'static [BwCommandParam], bool),
    If,
    Set,
    End,
}

pub struct Iscript {
    pub bw_data: Box<[u8]>,
    pub aice_data: Vec<u8>,
    pub bw_aice_cmd_offsets: FxHashMap<u16, u32>,
    pub headers: Vec<ResolvedHeader>,
    pub conditions: Vec<Rc<BoolExpr>>,
    pub int_expressions: Vec<Rc<IntExpr>>,
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
    parsing_header_pass2: bool,
    headers: Vec<Header<'a>>,
    // true if the previous command didn't force end execution
    is_continuing_commands: bool,
    animation_name_to_index: FxHashMap<&'static [u8], u8>,
    command_map: FxHashMap<&'static [u8], CommandPrototype>,
}

impl<'a> Parser<'a> {
    fn new() -> Parser<'a> {
        Parser {
            parsing_header: None,
            parsing_header_pass2: false,
            headers: Vec::with_capacity(1024),
            is_continuing_commands: false,
            animation_name_to_index: ANIMATION_NAMES
                .iter()
                .map(|x| (x.0.as_bytes(), x.1))
                .collect(),
            command_map: COMMANDS.iter().cloned().collect(),
        }
    }

    fn parse_line_pass1(
        &mut self,
        line: &'a BStr,
        compiler: &mut Compiler<'a>,
    ) -> Result<(), Error> {
        if line == ".headerend" {
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
        if line == ".headerstart" {
            if self.is_continuing_commands {
                return Err(Error::Msg("Previous script hasn't terminated"));
            }
            self.parsing_header = Some(Header {
                iscript_id: !0,
                animations: Vec::with_capacity(10),
            });
            Ok(())
        } else {
            self.parse_command_pass1(line, compiler)
        }
    }

    fn parse_command_pass1(
        &mut self,
        line: &'a BStr,
        compiler: &mut Compiler<'a>,
    ) -> Result<(), Error> {
        if let Some(_) = parse_label(line)? {
            return Ok(());
        }
        let command = line.fields().next().ok_or_else(|| Error::Msg("???"))?;
        let rest = (&line[command.len()..]).trim_start();
        match self.command_map.get(command.as_ref()) {
            Some(CommandPrototype::BwCommand(_, commands, ends_flow)) => {
                mark_required_bw_labels(rest, commands, compiler)?;
                compiler.flow_to_bw();
                self.is_continuing_commands = !ends_flow;
            }
            Some(CommandPrototype::If) => {
                compiler.flow_to_aice();
                self.is_continuing_commands = true;
            }
            Some(CommandPrototype::End) => {
                compiler.flow_to_bw();
                self.is_continuing_commands = false;
            }
            Some(CommandPrototype::Set) => {
                let (ty, var_name, _rest) = parse_set_place(rest)?;
                //compiler.add_set_decl(var_name, ty)?;
                compiler.flow_to_aice();
                self.is_continuing_commands = true;
            }
            None => return Err(Error::UnknownCommand(command.into())),
        }
        Ok(())
    }

    fn parse_line_add_label(
        &mut self,
        line: &'a BStr,
        compiler: &mut Compiler<'a>,
    ) -> Result<(), Error> {
        if self.skip_header_line(line) {
            return Ok(());
        }
        if let Some(label) = parse_label(line)? {
            compiler.add_label(label)?;
        }
        Ok(())
    }

    fn skip_header_line(&mut self, line: &'a BStr) -> bool {
        if self.parsing_header_pass2 {
            if line == ".headerend" {
                self.parsing_header_pass2 = false;
            }
            true
        } else if line == ".headerstart" {
            self.parsing_header_pass2 = true;
            true
        } else {
            false
        }
    }

    fn parse_line_pass3(
        &mut self,
        line: &'a BStr,
        compiler: &mut Compiler<'a>,
    ) -> Result<(), Error> {
        if self.skip_header_line(line) {
            return Ok(());
        }

        if let Some(label) = parse_label(line)? {
            compiler.set_label_position_to_current(label);
            return Ok(());
        }
        let command = line.fields().next().ok_or_else(|| Error::Msg("???"))?;
        let rest = (&line[command.len()..]).trim_start();
        match self.command_map.get(command.as_ref()) {
            Some(&CommandPrototype::BwCommand(op, params, ends_flow)) => {
                compiler.add_bw_command(op, rest, params, ends_flow)?;
                Ok(())
            }
            Some(CommandPrototype::If) => {
                let (condition, rest) = parse_bool_expr(rest)?;
                let mut tokens = rest.fields();
                let next = tokens.next();
                if next != Some(BStr::new("goto")) {
                    return Err(Error::Dynamic(format!("Expected 'goto', got {:?}", next)));
                }
                let label = tokens.next()
                    .and_then(|l| parse_label_ref(l))
                    .ok_or_else(|| Error::Msg("Expected label after goto"))?;
                compiler.add_if(condition, label)
            }
            Some(CommandPrototype::End) => {
                compiler.add_aice_command(aice_op::PRE_END);
                compiler.flow_to_bw();
                Ok(())
            }
            Some(CommandPrototype::Set) => {
                let (_ty, var_name, rest) = parse_set_place(rest)?;
                let (expr, rest) = parse_int_expr(rest)?;
                if !rest.is_empty() {
                    return Err(Error::Dynamic(format!("Trailing characters '{}'", rest)));
                }
                compiler.add_set(var_name, expr);
                Ok(())
            }
            None => unreachable!(),
        }
    }
}

fn split_first_token(text: &BStr) -> Option<(&BStr, &BStr)> {
    let start = text.bytes().position(|x| x != b' ' && x != b'\t')?;
    let end = start + text.bytes().skip(start).position(|x| x == b' ' && x == b'\t')?;
    let first = &text[start..end];
    Some(match text.bytes().skip(end).position(|x| x != b' ' && x != b'\t') {
        Some(s) => (first, &text[end + s..]),
        None => (first, BStr::new(&[])),
    })
}

fn parse_set_place(text: &BStr) -> Result<(VariableType, &BStr, &BStr), Error> {
    let (ty, rest) = split_first_token(text)
        .ok_or_else(|| Error::Msg("Expected place"))?;
    let (name, rest) = split_first_token(rest)
        .ok_or_else(|| Error::Msg("Expected variable name"))?;
    let (_, rest) = split_first_token(rest)
        .filter(|x| x.0 == "=")
        .ok_or_else(|| Error::Msg("Expected '='"))?;
    let ty = match ty {
        x if x == "global" => VariableType::Global,
        x if x == "spritelocal" => VariableType::SpriteLocal,
        x => return Err(Error::Dynamic(format!("Unknown variable type '{}'", x))),
    };
    Ok((ty, name, rest))
}

fn parse_bool_expr(text: &BStr) -> Result<(BoolExpr, &BStr), Error> {
    BoolExpr::parse_part(text.as_ref())
        .map(|(a, b)| (a, BStr::new(b)))
        .map_err(|e| e.into())
}

fn parse_int_expr(text: &BStr) -> Result<(IntExpr, &BStr), Error> {
    IntExpr::parse_part(text.as_ref())
        .map(|(a, b)| (a, BStr::new(b)))
        .map_err(|e| e.into())
}

fn mark_required_bw_labels<'a>(
    text: &'a BStr,
    params: &[BwCommandParam],
    compiler: &mut Compiler<'a>,
) -> Result<(),  Error> {
    let mut tokens = text.fields();
    for param in params {
        let arg = tokens.next()
            .ok_or_else(|| Error::Dynamic(format!("Expected {} arguments", params.len())))?;
        if let BwCommandParam::Label = param {
            let label = parse_label_ref(arg).ok_or_else(|| Error::Msg("Expected label"))?;
            compiler.require_bw_label(label);
        }
    }
    Ok(())
}

fn parse_header_opcode<'a>(
    line: &'a BStr,
    animation_name_to_index: &FxHashMap<&'static [u8], u8>,
) -> Result<HeaderOpcode<'a>, Error> {
    let mut tokens = line.fields();
    let first = tokens.next().ok_or_else(|| Error::Msg("???"))?;
    if first == "Type" {
        Ok(HeaderOpcode::Type)
    } else if first == "IsId" {
        let id = tokens.next()
            .and_then(|x| parse_u16(x))
            .ok_or_else(|| Error::Msg("Expected iscript ID"))?;
        Ok(HeaderOpcode::IscriptId(id))
    } else {
        let label = tokens.next().ok_or_else(|| Error::Msg("Expected label"))?;
        if let Some(next) = tokens.next() {
            return Err(Error::Trailing(next.into()));
        }
        let animation = match animation_name_to_index.get(first.as_ref()) {
            Some(&x) => x,
            None => return Err(Error::Dynamic(format!("Unknown animation '{}'", first))),
        };
        Ok(HeaderOpcode::Animation(animation, Label(label)))
    }
}

/// Parses line in form "Label:", or returns None for other command
fn parse_label<'a>(line: &'a BStr) -> Result<Option<Label<'a>>, Error> {
    let mut tokens = line.fields();
    let first = tokens.next().ok_or_else(|| Error::Msg("???"))?;
    if first.ends_with(b":") {
        let label = &first[..first.len() - 1];
        if let Some(label) = parse_label_ref(label) {
            if let Some(next) = tokens.next() {
                Err(Error::Trailing(next.into()))
            } else {
                Ok(Some(label))
            }
        } else {
            Err(Error::Msg("Invalid characters in label name"))
        }
    } else {
        Ok(None)
    }
}

/// Parses label name (as in argument)
fn parse_label_ref<'a>(label: &'a BStr) -> Option<Label<'a>> {
    if label.bytes().all(|x| x.is_ascii_alphanumeric() || x == b'_') {
        Some(Label(label))
    } else {
        None
    }
}

fn parse_u16(text: &BStr) -> Option<u16> {
    let (base, text) = match text.starts_with("0x") {
        true => (16, &text[2..]),
        false => (10, text),
    };
    u16::from_str_radix(text.to_str().ok()?, base).ok()
}

fn parse_u8(text: &BStr) -> Option<u8> {
    let (base, text) = match text.starts_with("0x") {
        true => (16, &text[2..]),
        false => (10, text),
    };
    u8::from_str_radix(text.to_str().ok()?, base).ok()
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
    labels: FxHashMap<Label<'a>, CodePosition>,
    needed_label_positions: Vec<(CodePosition, Label<'a>)>,
    bw_required_labels: FxHashSet<Label<'a>>,
    bw_aice_cmd_offsets: Vec<(u16, u32)>,
    conditions: Vec<Rc<BoolExpr>>,
    existing_conditions: FxHashMap<Rc<BoolExpr>, u32>,
    int_expressions: Vec<Rc<IntExpr>>,
    existing_int_expressions: FxHashMap<Rc<IntExpr>, u32>,
    variables: FxHashMap<&'a BStr, VariableId>,
    variable_counts: Vec<u32>,
    flow_in_bw: bool,
}

/// 0xc000_0000 == tag
#[derive(Copy, Clone, Serialize, Deserialize, Debug)]
pub struct VariableId(pub u32);

#[repr(u8)]
pub enum VariableType {
    Global = 0x0,
    SpriteLocal = 0x1,
}

impl VariableId {
    fn new(id: u32, ty: VariableType) -> VariableId {
        assert!(id & 0xc000_0000 == 0);
        VariableId(id | ((ty as u32) << 30))
    }

    pub fn index(self) -> u32 {
        (self.0 & !0xc000_0000)
    }

    pub fn ty(self) -> Option<VariableType> {
        Some(match self.0 >> 30 {
            0 => VariableType::Global,
            1 => VariableType::Global,
            _ => return None,
        })
    }
}

#[derive(Copy, Clone, Serialize, Deserialize)]
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
            bw_required_labels: FxHashSet::with_capacity_and_hasher(1024, Default::default()),
            conditions: Vec::with_capacity(16),
            existing_conditions: FxHashMap::with_capacity_and_hasher(16, Default::default()),
            int_expressions: Vec::with_capacity(16),
            existing_int_expressions: FxHashMap::with_capacity_and_hasher(16, Default::default()),
            variables: FxHashMap::with_capacity_and_hasher(16, Default::default()),
            variable_counts: vec![0; 4],
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

    /// Makes `label` located at current position.
    fn add_label(&mut self, label: Label<'a>) -> Result<(), Error> {
        if self.bw_required_labels.contains(&label) {
            self.flow_to_bw();
        }
        match self.labels.entry(label) {
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

    fn require_bw_label(&mut self, label: Label<'a>) {
        self.bw_required_labels.insert(label);
    }

    fn set_label_position_to_current(&mut self, label: Label<'a>) {
        if self.bw_required_labels.contains(&label) {
            if !self.flow_in_bw {
                self.emit_aice_jump_to_bw(self.bw_bytecode.len() as u16);
                self.flow_in_bw = true;
            }
        }
        debug!("Label {} -> {:04x}", label.0, self.code_position().0);
        *self.labels.get_mut(&label).expect("Label not added?") = self.code_position();
    }

    fn label_location(&self, label: Label<'a>) -> Option<CodePosition> {
        self.labels.get(&label).cloned()
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

    fn add_set(&mut self, var_name: &'a BStr, value: IntExpr) {
        self.add_flow_to_aice();
        let index = match self.existing_int_expressions.get(&value).cloned() {
            Some(s) => s,
            None => {
                let index = self.int_expressions.len() as u32;
                let rc = Rc::new(value);
                self.int_expressions.push(rc.clone());
                self.existing_int_expressions.insert(rc, index);
                index
            }
        };
        let var_id = match self.variables.get(var_name) {
            Some(&s) => s,
            None => unreachable!(),
        };
        self.aice_bytecode.push(aice_op::SET);
        self.aice_bytecode.write_u32::<LE>(var_id.0).unwrap();
        self.aice_bytecode.write_u32::<LE>(index as u32).unwrap();
    }

    fn add_if(&mut self, condition: BoolExpr, dest: Label<'a>) -> Result<(), Error> {
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
        let pos = self.label_location(dest)
            .ok_or_else(|| Error::Dynamic(format!("Label '{}' not defined", dest.0)))?;

        self.aice_bytecode.push(aice_op::IF);
        self.aice_bytecode.write_u32::<LE>(index as u32).unwrap();
        self.aice_bytecode.write_u32::<LE>(pos.0).unwrap();
        let offset = self.aice_bytecode.len() as u32 - 4;
        if pos.is_unknown() {
            self.needed_label_positions.push((CodePosition::aice(offset), dest));
        }
        if pos.if_bw_pos().is_some() {
            self.bw_offsets_in_aice.push(offset);
        }
        Ok(())
    }

    fn add_aice_command(
        &mut self,
        byte: u8,
    ) {
        self.add_flow_to_aice();
        self.aice_bytecode.push(byte);
    }

    fn add_bw_command(
        &mut self,
        byte: u8,
        param_text: &'a BStr,
        params: &[BwCommandParam],
        ends_flow: bool,
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
                    let pos = self.label_location(label)
                        .ok_or_else(|| Error::Dynamic(format!("Label '{}' not defined", label.0)))?;
                    if let Some(pos) = pos.if_bw_pos() {
                        let offset_in_command = code_len - out.len();
                        let offset = (self.bw_bytecode.len() + offset_in_command) as u16;
                        self.bw_offsets_in_bw.push(offset);
                        (&mut out).write_u16::<LE>(pos).unwrap();
                        if pos == 0xffff {
                            let pos = CodePosition::bw(offset);
                            self.needed_label_positions.push((pos, label));
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

    fn fixup_missing_label_positions(&mut self) {
        for &(pos, label) in &self.needed_label_positions {
            let label_pos = self.label_location(label)
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

    fn compile(mut self, headers: &[Header<'a>]) -> Result<Iscript, Error> {
        self.fixup_missing_label_positions();

        if self.bw_bytecode.len() >= 0x10000 {
            return Err(Error::Msg("Iscript too large"));
        }
        debug_assert!(self.bw_code_flow_ends.windows(2).all(|x| x[0] < x[1]));

        let mut bw_data = Vec::with_capacity(32768);
        bw_data.resize(0x1c, AICE_COMMAND);
        bw_data[0x4] = 0x16; // Global end command
        bw_data.extend((0..headers.len()).flat_map(|_| [
            0x53, 0x43, 0x50, 0x45, 0x1b, 0x00, 0x00, 0x00,
        ].iter().cloned()));
        // Add animation offsets for the last script
        bw_data.resize(bw_data.len() + 0x1c * 2, 0x00);
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

        let headers = headers.iter().map(|header| {
            let animations = header.animations.iter().map(|&label| match label {
                None => Ok(None),
                Some(label) => {
                    if *label.0 == &b"[NONE]"[..] {
                        Ok(None)
                    } else {
                        self.label_location(label).ok_or_else(|| {
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


fn remove_comment(line: &BStr) -> &BStr {
    match line.find_byte(b'#') {
        Some(s) => &line[..s],
        None => line,
    }
}

#[derive(Debug)]
pub struct ErrorWithLine {
    pub error: Error,
    pub line: Option<usize>,
}

pub fn compile_iscript_txt(text: &[u8]) -> Result<Iscript, Vec<ErrorWithLine>> {
    let text = BStr::new(text);
    fn lines<'a>(text: &'a BStr) -> impl Iterator<Item = (usize, &'a BStr)> {
        text.lines()
            .enumerate()
            .map(|(i, line)| (1 + i, remove_comment(line).trim()))
            .filter(|(_, line)| !line.is_empty())
    }

    let mut compiler = Compiler::new();
    let mut parser = Parser::new();
    let mut errors = Vec::new();
    for (line_number, line) in lines(text) {
        if let Err(error) = parser.parse_line_pass1(line, &mut compiler) {
            errors.push(ErrorWithLine {
                error,
                line: Some(line_number),
            });
        }
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    for (line_number, line) in lines(text) {
        if let Err(error) = parser.parse_line_add_label(line, &mut compiler) {
            errors.push(ErrorWithLine {
                error,
                line: Some(line_number),
            });
        }
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    for (line_number, line) in lines(text) {
        if let Err(error) = parser.parse_line_pass3(line, &mut compiler) {
            errors.push(ErrorWithLine {
                error,
                line: Some(line_number),
            });
        }
    }
    if !errors.is_empty() {
        return Err(errors);
    }
    let script = match compiler.compile(&parser.headers) {
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
}
