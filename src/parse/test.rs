use super::*;

use std::cell::Cell;

use bw_dat::expr::{IntFunc, IntFuncType};

trait ExprIdExt {
    fn expect_int(self) -> usize;
}

impl ExprIdExt for super::ExprId {
    fn expect_int(self) -> usize {
        match self.unpack() {
            UnpackedExprId::Int(i) => i,
            x => panic!("Expected integer ExprId, got {x:?}"),
        }
    }
}

fn read(filename: &str) -> Vec<u8> {
    std::fs::read(format!("test_scripts/{}", filename)).unwrap()
}

fn compile_catch(text: &[u8]) -> Result<Iscript, Vec<ErrorWithLine>> {
    let result = std::panic::catch_unwind(|| compile_iscript_txt(&text));
    match result {
        Ok(o) => o,
        Err(err) => {
            let line = LINE_CTX.with(|x| x.get());
            if let Some(msg) = err.downcast_ref::<String>() {
                panic!("Panic at line {}: {}", line, msg);
            } else if let Some(msg) = err.downcast_ref::<&'static str>() {
                panic!("Panic at line {}: {}", line, msg);
            } else {
                panic!("Panic at line {} (No text payload)", line);
            }
        }
    }
}

fn compile_success(filename: &str) -> Iscript {
    set_test_line_ctx(!0);
    let text = read(filename);
    let result = compile_catch(&text);
    match result {
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
    let result = compile_catch(&text);
    match result {
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
            println!("Errors:");
            for e in errors {
                println!("    {}: {}", e.line.unwrap_or(0), e.error);
            }
            panic!("Couldn't find error '{}' @ line {}", substring, line);
        }
    }
}

fn aice_pos_for_line(iscript: &Iscript, line: u32) -> usize {
    iscript.line_info.aice.line_to_pos(line)
        .unwrap_or_else(|| panic!("No aice opcode on line {line}")) as usize
}

fn aice_code_for_line(iscript: &Iscript, line: u32) -> &[u8] {
    let pos = aice_pos_for_line(iscript, line);
    let next_aice_pos = (1..0x10)
        .map(|i| iscript.line_info.pos_to_line(CodePosition::aice(pos as u32 + i)))
        .find(|&line2| line != line2)
        .and_then(|line2| iscript.line_info.aice.line_to_pos(line2))
        .unwrap_or(iscript.aice_data.len());
    &iscript.aice_data[pos..next_aice_pos]
}

fn aice_params_for_line(iscript: &Iscript, line: u32, opcode: u8) -> &[u8] {
    let data = aice_code_for_line(iscript, line);
    assert_eq!(
        data[0], opcode,
        "Expected opcode {opcode:02x} for line {line}, got {:02x}", data[0],
    );
    &data[1..]
}

fn read_aice_params_for_line(
    iscript: &Iscript,
    line: u32,
    opcode: u8,
    params: &CommandParams,
) -> ([i32; 8], [u8; 8], usize) {
    let data = aice_code_for_line(iscript, line);
    assert_eq!(
        data[0], opcode,
        "Expected opcode {opcode:02x} for line {line}, got {:02x}", data[0],
    );

    let opcode_start = (data.as_ptr() as usize) - (iscript.aice_data.as_ptr() as usize);
    read_aice_params_next(&iscript, opcode_start, params)
}

fn set_place_for_line(iscript: &Iscript, line: u32) -> PlaceId {
    let data = aice_code_for_line(iscript, line);
    assert!(
        matches!(data[0], aice_op::SET | aice_op::SET_COPY),
        "Expected set opcode for line {line}, got {:02x}", data[0],
    );
    PlaceId(LittleEndian::read_u32(&data[1..5]))
}

fn assert_place_obj(
    iscript: &Iscript,
    place: PlaceId,
    var: UnitVar,
    object: &[UnitObject],
) {
    let object = object.iter()
        .map(|&o| UnitObjectOrVariable::new_object(o))
        .collect::<Vec<_>>();
    let object = &object[..];
    match place.place() {
        Place::Unit(unit, x) if x == var  => {
            match iscript.unit_ref_object(unit) {
                UnitRefParts::Single(x) => {
                    assert_eq!(&[x], object);
                }
                UnitRefParts::Many(x) => {
                    assert_eq!(x, object);
                }
            }
        }
        x => panic!("Wrong place {:?}", x),
    }
}

fn assert_spritelocal_place_obj(
    iscript: &Iscript,
    place: PlaceId,
    object: &[UnitObject],
) -> u32 {
    let object = object.iter()
        .map(|&o| UnitObjectOrVariable::new_object(o))
        .collect::<Vec<_>>();
    let object = &object[..];
    match place.place() {
        Place::SpriteLocal(unit, x) => {
            match iscript.unit_ref_object(unit) {
                UnitRefParts::Single(x) => {
                    assert_eq!(&[x], object);
                }
                UnitRefParts::Many(x) => {
                    assert_eq!(x, object);
                }
            }
            x
        }
        x => panic!("Wrong place {:?}", x),
    }
}

fn unwrap_expr_default(expr: &IntExprTree) -> (&IntExprTree, &IntExprTree) {
    match expr {
        IntExprTree::Custom(Int::Default(ref pair)) => {
            (pair.0.inner(), pair.1.inner())
        }
        x => panic!("Wrong expr {:?}", x),
    }
}

fn read_aice_params_next(
    iscript: &Iscript,
    opcode_start: usize,
    params: &CommandParams,
) -> ([i32; 8], [u8; 8], usize) {
    let len = iscript.aice_data.len();
    let end = iscript.aice_data.as_ptr().wrapping_add(len);

    let ptr = iscript.aice_data.as_ptr().wrapping_add(opcode_start + 1);
    assert!(ptr < end);
    let mut result = [0i32; 8];
    let mut exprs = [0u8; 8];
    unsafe {
        let (new_ptr, _) = super::read_aice_params(params, ptr, &mut result, &mut exprs);
        assert!(new_ptr <= end);
        let new_pos = new_ptr as usize - iscript.aice_data.as_ptr() as usize;
        (result, exprs, new_pos)
    }
}

fn read_aice_params(
    iscript: &Iscript,
    opcode_start: usize,
    params: &CommandParams,
) -> ([i32; 8], [u8; 8]) {
    let (a, b, _) = read_aice_params_next(iscript, opcode_start, params);
    (a, b)
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
fn vars2() {
    compile_success("vars2.txt");
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

#[test]
fn line_lookup() {
    let iscript = compile_success("if1.txt");
    // This test needs to be updated if `if` is no longer 9 byte command
    // or jump_to_bw no longer 3 bytes
    assert_eq!(iscript.line_info.pos_to_line(CodePosition::aice(0)), 42);
    assert_eq!(iscript.line_info.pos_to_line(CodePosition::aice(12)), 51);
    assert_eq!(iscript.line_info.pos_to_line(CodePosition::aice(6 * 12)), 229);
}

#[test]
fn if_call() {
    compile_success("if2.txt");
}

#[test]
fn if_regression() {
    compile_success("if3.txt");
}

#[test]
fn create_unit_expr_regression() {
    let iscript = compile_success("create_unit_expr.txt");
    // The pos_to_line currently returns next aice line for any position
    // that isn't inside the command start; search in reverse to find the actual
    // line 47
    let aice_pos = (0..0x40).rfind(|&i| {
        iscript.line_info.pos_to_line(CodePosition::aice(i)) == 47
    }).unwrap() as usize;
    assert_eq!(iscript.aice_data[aice_pos], aice_op::CREATE_UNIT);
    let (values, exprs) = read_aice_params(&iscript, aice_pos, &aice_op::CREATE_UNIT_PARAMS);
    assert!(exprs[1] != 0);
    assert!(exprs[2] != 0);
    let x = values[1] as usize;
    let y = values[2] as usize;
    let with_set = LittleEndian::read_u32(&iscript.aice_data[(aice_pos + 17)..]);
    assert_eq!(with_set, 0);
    let x_place = PlaceId::new_flingy(FlingyVar::PositionX, UnitRefId::this());
    let y_place = PlaceId::new_flingy(FlingyVar::PositionY, UnitRefId::this());
    let x_expr = IntExprTree::Custom(Int::Variable(x_place, [None, None, None, None]));
    let y_expr = IntExprTree::Sub(Box::new((
        IntExprTree::Custom(Int::Variable(y_place, [None, None, None, None])),
        IntExprTree::Integer(9),
    )));
    assert_eq!(*iscript.int_expressions[x].inner(), x_expr);
    assert_eq!(*iscript.int_expressions[y].inner(), y_expr);
}

#[test]
fn issue_order() {
    let iscript = compile_success("issue_order.txt");
    // The pos_to_line currently returns next aice line for any position
    // that isn't inside the command start; search in reverse to find the actual
    // line 47
    let aice_pos = (0..0x40).rfind(|&i| {
        iscript.line_info.pos_to_line(CodePosition::aice(i)) == 47
    }).unwrap() as usize;
    assert_eq!(iscript.aice_data[aice_pos], aice_op::ISSUE_ORDER);
    let (values, exprs) = read_aice_params(&iscript, aice_pos, &aice_op::ISSUE_ORDER_PARAMS);
    let order = values[0] as usize;
    assert!(exprs[0] == 0);
    assert!(exprs[1] != 0);
    assert!(exprs[2] != 0);
    let x = values[1] as usize;
    let y = values[2] as usize;
    assert_eq!(order, 72);
    let x_place = PlaceId::new_flingy(FlingyVar::PositionX, UnitRefId::this());
    let y_place = PlaceId::new_flingy(FlingyVar::PositionY, UnitRefId::this());
    let x_expr = IntExprTree::Custom(Int::Variable(x_place, [None, None, None, None]));
    let y_expr = IntExprTree::Sub(Box::new((
        IntExprTree::Custom(Int::Variable(y_place, [None, None, None, None])),
        IntExprTree::Integer(9),
    )));
    assert_eq!(*iscript.int_expressions[x].inner(), x_expr);
    assert_eq!(*iscript.int_expressions[y].inner(), y_expr);
}

#[test]
fn issue_order_nonconst() {
    let _ = compile_success("issue_order_nonconst.txt");
}

#[test]
fn unit_refs() {
    let iscript = compile_success("unit_refs.txt");
    // The pos_to_line currently returns next aice line for any position
    // that isn't inside the command start; search in reverse to find the actual line

    // Line 60 check `set unit.target.turn_speed = ..`
    let aice_pos = (0..0x40).rfind(|&i| {
        iscript.line_info.pos_to_line(CodePosition::aice(i)) == 60
    }).unwrap() as usize;
    assert_eq!(iscript.aice_data[aice_pos], aice_op::SET);
    let place = PlaceId(LittleEndian::read_u32(&iscript.aice_data[(aice_pos + 1)..]));
    match place.place() {
        Place::Flingy(unit, FlingyVar::TurnSpeed) => {
            match iscript.unit_ref_object(unit) {
                UnitRefParts::Single(x)
                    if x.if_unit_object() == Some(UnitObject::UnitTarget) => (),
                x => panic!("Wrong unit ref parts {:?}", x),
            }
        }
        x => panic!("Wrong place {:?}", x),
    }

    // Line 61 check `set unit.target.hitpoints = unit.target.hitpoints - 10 default 0`
    let aice_pos = (0..0x40).rfind(|&i| {
        iscript.line_info.pos_to_line(CodePosition::aice(i)) == 61
    }).unwrap() as usize;
    assert_eq!(iscript.aice_data[aice_pos], aice_op::SET);
    let place = PlaceId(LittleEndian::read_u32(&iscript.aice_data[(aice_pos + 1)..]));
    let expr = LittleEndian::read_u32(&iscript.aice_data[(aice_pos + 5)..]) as usize;
    assert_place_obj(
        &iscript,
        place,
        UnitVar::Hitpoints,
        &[UnitObject::UnitTarget],
    );
    let (l, r) = unwrap_expr_default(iscript.int_expressions[expr].inner());
    match l {
        IntExprTree::Sub(ref pair) => {
            match pair.0 {
                IntExprTree::Custom(Int::Variable(place, _)) => {
                    assert_place_obj(
                        &iscript,
                        place,
                        UnitVar::Hitpoints,
                        &[UnitObject::UnitTarget],
                    );
                }
                ref x => panic!("Wrong expr {:?}", x),
            }
            assert_eq!(pair.1, IntExprTree::Integer(10));
        }
        x => panic!("Wrong expr {:?}", x),
    }
    assert_eq!(r, &IntExprTree::Integer(0));

    // Line 64 check `set .. = 5 + (unit.target.parent.hitpoints default unit.target.hitpoints default unit.hitpoints)`
    let aice_pos = (0..0x80).rfind(|&i| {
        iscript.line_info.pos_to_line(CodePosition::aice(i)) == 64
    }).unwrap() as usize;
    assert_eq!(iscript.aice_data[aice_pos], aice_op::SET);
    let expr = LittleEndian::read_u32(&iscript.aice_data[(aice_pos + 5)..]) as usize;
    match *iscript.int_expressions[expr].inner() {
        IntExprTree::Add(ref pair) => {
            assert_eq!(&pair.0, &IntExprTree::Integer(5));
            let (l, r) = unwrap_expr_default(&pair.1);
            let (l, m) = unwrap_expr_default(l);
            match *l {
                IntExprTree::Custom(Int::Variable(place, _)) => {
                    assert_place_obj(
                        &iscript,
                        place,
                        UnitVar::Hitpoints,
                        &[UnitObject::UnitTarget, UnitObject::UnitParent],
                    );
                }
                ref x => panic!("Wrong expr {:?}", x),
            }
            match *m {
                IntExprTree::Custom(Int::Variable(place, _)) => {
                    assert_place_obj(
                        &iscript,
                        place,
                        UnitVar::Hitpoints,
                        &[UnitObject::UnitTarget]
                    );
                }
                ref x => panic!("Wrong expr {:?}", x),
            }
            match *r {
                IntExprTree::Custom(Int::Variable(place, _)) => {
                    assert_place_obj(
                        &iscript,
                        place,
                        UnitVar::Hitpoints,
                        &[UnitObject::This]
                    );
                }
                ref x => panic!("Wrong expr {:?}", x),
            }
        }
        ref x => panic!("Wrong expr {:?}", x),
    }

    // Line 115 `set if_uninit spritelocal unit.transport.marine_count = 0`
    let aice_pos = (0..0x100).rfind(|&i| {
        iscript.line_info.pos_to_line(CodePosition::aice(i)) == 115
    }).unwrap() as usize;
    assert_eq!(iscript.aice_data[aice_pos], aice_op::SET);
    let place = PlaceId(LittleEndian::read_u32(&iscript.aice_data[(aice_pos + 1)..]));
    let marine_count = assert_spritelocal_place_obj(&iscript, place, &[UnitObject::Transport]);
    // Line 116 `set spritelocal unit.transport.marine_count = unit.transport.marine_count + 1 default 0`
    let aice_pos = (0..0x100).rfind(|&i| {
        iscript.line_info.pos_to_line(CodePosition::aice(i)) == 116
    }).unwrap() as usize;
    assert_eq!(iscript.aice_data[aice_pos], aice_op::SET);
    let place = PlaceId(LittleEndian::read_u32(&iscript.aice_data[(aice_pos + 1)..]));
    let expr = LittleEndian::read_u32(&iscript.aice_data[(aice_pos + 5)..]);
    let marine_count2 = assert_spritelocal_place_obj(&iscript, place, &[UnitObject::Transport]);
    assert_eq!(marine_count, marine_count2);
    let (l, r) = unwrap_expr_default(iscript.int_expressions[expr as usize].inner());
    assert_eq!(r, &IntExprTree::Integer(0));
    match l {
        IntExprTree::Add(ref pair) => {
            match pair.0 {
                IntExprTree::Custom(Int::Variable(place, _)) => {
                    let marine_count2 = assert_spritelocal_place_obj(
                        &iscript,
                        place,
                        &[UnitObject::Transport],
                    );
                    assert_eq!(marine_count, marine_count2);
                }
                ref x => panic!("Wrong expr {:?}", x),
            }
            assert_eq!(&pair.1, &IntExprTree::Integer(1));
        }
        ref x => panic!("Wrong expr {:?}", x),
    }
}

#[test]
fn unit_refs_err() {
    let mut errors = compile_err("unit_refs_err.txt");
    find_error(&mut errors, "needs `default`", 60);
    find_error(&mut errors, "needs `default`", 61);
    find_error(&mut errors, "needs `default`", 62);
    find_error(&mut errors, "needs `default`", 63);
    find_error(&mut errors, "Parameter 3 must be between", 78);
    assert!(errors.is_empty());
}

#[test]
fn if_uninit() {
    let iscript = compile_success("if_uninit.txt");
    // The pos_to_line currently returns next aice line for any position
    // that isn't inside the command start; search in reverse to find the actual line

    // Line 25 check `set if_uninit spritelocal`
    let aice_pos = (0..0x40).rfind(|&i| {
        iscript.line_info.pos_to_line(CodePosition::aice(i)) == 25
    }).unwrap() as usize;
    assert_eq!(iscript.aice_data[aice_pos], aice_op::SET);
    let place = PlaceId(LittleEndian::read_u32(&iscript.aice_data[(aice_pos + 1)..]));
    assert!(place.if_uninit());
    match place.place() {
        Place::SpriteLocal(..) => {
        }
        x => panic!("Wrong place {:?}", x),
    }
}

#[test]
fn if_uninit_err() {
    let mut errors = compile_err("if_uninit_err.txt");
    find_error(&mut errors, "if_uninit", 25);
    find_error(&mut errors, "if_uninit", 26);
    assert!(errors.is_empty());
}

#[test]
fn with() {
    let iscript = compile_success("with.txt");
    // The pos_to_line currently returns next aice line for any position
    // that isn't inside the command start; search in reverse to find the actual line

    // Line 48 check `create_unit .. with {}`
    let aice_pos = (0..0x80).rfind(|&i| {
        iscript.line_info.pos_to_line(CodePosition::aice(i)) == 48
    }).unwrap() as usize;
    assert_eq!(iscript.aice_data[aice_pos], aice_op::CREATE_UNIT);
    let (values, _) = read_aice_params(&iscript, aice_pos, &aice_op::CREATE_UNIT_PARAMS);
    let with_set = values[4] as u32;
    assert_eq!(with_set, 1);
    let locals = iscript.sprite_local_set(SpriteLocalSetId(with_set));
    assert_eq!(locals.len(), 3);
    assert_ne!(locals[0].0, locals[1].0);
    assert_ne!(locals[0].0, locals[2].0);
    // deaths = deaths + 1
    let expr1 = locals[0].1.expect_int();
    match *iscript.int_expressions[expr1].inner() {
        IntExprTree::Add(ref pair) => {
            assert_eq!(&pair.1, &IntExprTree::Integer(1));
            match pair.0 {
                IntExprTree::Custom(Int::Variable(place, _)) => {
                    match place.place() {
                        Place::SpriteLocal(unit_ref, x) => {
                            assert_eq!(unit_ref, UnitRefId::this());
                            assert_eq!(x, locals[0].0);
                        }
                        ref x => panic!("Wrong place {:?}", x),
                    }
                }
                ref x => panic!("Wrong expr {:?}", x),
            }
        }
        ref x => panic!("Wrong expr {:?}", x),
    }
    // miscstuff = 50
    let expr2 = locals[1].1.expect_int();
    match *iscript.int_expressions[expr2].inner() {
        IntExprTree::Integer(50) => (),
        ref x => panic!("Wrong expr {:?}", x),
    }
    // rest = unit.target.hitpoints default 0
    let expr3 = locals[2].1.expect_int();
    let (l, r) = unwrap_expr_default(iscript.int_expressions[expr3].inner());
    match *l {
        IntExprTree::Custom(Int::Variable(place, _)) => {
            assert_place_obj(
                &iscript,
                place,
                UnitVar::Hitpoints,
                &[UnitObject::UnitTarget],
            );
        }
        ref x => panic!("Wrong expr {:?}", x),
    }
    assert_eq!(r, &IntExprTree::Integer(0));
}

#[test]
fn with_err() {
    let mut errors = compile_err("with_err.txt");
    find_error(&mut errors, "Unterminated", 48);
    find_error(&mut errors, "Last command does not terminate", 82);
    assert!(errors.is_empty());
}

#[test]
fn with_err2() {
    let mut errors = compile_err("with_err2.txt");
    find_error(&mut errors, "Global", 51);
    assert!(errors.is_empty());
}

#[test]
fn print() {
    let _ = compile_success("print.txt");
}

#[test]
fn print_err() {
    let mut errors = compile_err("print_err.txt");
    find_error(&mut errors, "got '.taget}", 52);
    assert!(errors.is_empty());
}

#[test]
fn set_copy() {
    use super::anytype_expr::{AnyTypeExpr, read_any_type, test_any_type_eq};

    let iscript = compile_success("set_copy.txt");
    let b = bumpalo::Bump::new();

    let var2 = set_place_for_line(&iscript, 25);
    let var3 = set_place_for_line(&iscript, 26);
    let var4 = set_place_for_line(&iscript, 27);

    // var1 = var2
    let params = aice_params_for_line(&iscript, 29, aice_op::SET_COPY);
    let expected = AnyTypeExpr::Variable(var2);
    let expr = read_any_type(&params[4..]).unwrap().0;
    test_any_type_eq(&expected, &expr);

    // var1 = var2 default var3
    let params = aice_params_for_line(&iscript, 30, aice_op::SET_COPY);
    let expected = AnyTypeExpr::Default(
        b.alloc(AnyTypeExpr::Variable(var2)),
        b.alloc(AnyTypeExpr::Variable(var3)),
    );
    let expr = read_any_type(&params[4..]).unwrap().0;
    test_any_type_eq(&expected, &expr);

    // var1 = (var2 default var3) default var4
    let params = aice_params_for_line(&iscript, 31, aice_op::SET_COPY);
    let expected = AnyTypeExpr::Default(
        b.alloc(AnyTypeExpr::Default(
            b.alloc(AnyTypeExpr::Variable(var2)),
            b.alloc(AnyTypeExpr::Variable(var3)),
        )),
        b.alloc(AnyTypeExpr::Variable(var4)),
    );
    let expr = read_any_type(&params[4..]).unwrap().0;
    test_any_type_eq(&expected, &expr);

    // Verify that these lines are SET since dest is game.deaths
    let _ = aice_params_for_line(&iscript, 33, aice_op::SET);
    let _ = aice_params_for_line(&iscript, 34, aice_op::SET);
    // This should be SET since it assigns `player`
    let params = aice_params_for_line(&iscript, 38, aice_op::SET);
    let expr = LittleEndian::read_u32(&params[4..8]) as usize;
    // This has to be aice `flingy.player` as it allows also accessing bullet's player,
    // even if it was just spelled as `player`
    match *iscript.int_expressions[expr].inner() {
        IntExprTree::Custom(Int::Variable(place, _)) => {
            match place.place() {
                Place::Flingy(unit, var) => {
                    assert_eq!(var, FlingyVar::Player);
                    assert_eq!(unit.0, UnitObject::This as u16);
                }
                x => panic!("Wrong place {:?}", x),
            }
        }
        ref x => panic!("Wrong expr {:?}", x),
    }
}

#[test]
fn bullet_vars() {
    // Just test that misc usage of the variables works
    let _ = compile_success("bullet_vars.txt");
}

thread_local!(static LINE_CTX: Cell<u32> = Cell::new(!0));

pub fn set_test_line_ctx(line: u32) {
    LINE_CTX.with(|x| x.set(line));
}

#[test]
fn imgul_on() {
    let iscript = compile_success("imgul_on.txt");

    let (params, expr, next_pos) =
        read_aice_params_for_line(&iscript, 24, aice_op::IMG_ON, &aice_op::IMGUL_ON_PARAMS);
    assert!(expr[0] == 0);
    assert!(expr[1] == 0);
    assert!(expr[2] != 0);
    assert!(expr[3] == 0);
    assert!(expr[4] == 0);
    assert_eq!(params[0], 1);
    assert_eq!(params[3], 0);
    assert_eq!(params[4], 0);
    assert_eq!(iscript.aice_data[next_pos], aice_op::SET);
}

#[test]
fn unit_vars() {
    let _ = compile_success("unit_vars.txt");
}

#[test]
fn unit_vars_err() {
    let mut errors = compile_err("unit_vars_err.txt");
    find_error(&mut errors, "Conflicting variable type", 63);
    find_error(&mut errors, "Conflicting variable type", 64);
    assert!(errors.is_empty());

    let mut errors = compile_err("unit_vars_err2.txt");
    find_error(&mut errors, "Cannot parse", 60);
    find_error(&mut errors, "Cannot parse", 61);
    find_error(&mut errors, "default is not currently", 62);
    find_error(&mut errors, "Cannot use non-spritelocal object refs as a place", 63);
    assert!(errors.is_empty());
}
