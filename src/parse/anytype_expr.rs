use bumpalo::Bump;
use bumpalo::collections::Vec as BumpVec;
use byteorder::{ByteOrder, LittleEndian, WriteBytesExt};

use super::{
    CompilerVariables, Error, ParserExprs, split_first_token,
    expect_token, PlaceId,
};

pub(super) struct AnyTypeParser<'a> {
    stack: BumpVec<'a, AnyTypeExpr<'a>>,
    op_stack: BumpVec<'a, Operator>,
    bump: &'a Bump,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Operator {
    OpenBrace,
    Default,
}

/// For parsing, arena allocated
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum AnyTypeExpr<'a> {
    Variable(PlaceId),
    Default(&'a AnyTypeExpr<'a>, &'a AnyTypeExpr<'a>),
    // Could have If(BoolExpr, &'a AnyTypeExpr, &'a AnyTypeExpr) too
}

/// For reading AnyType during execution from byte buffer
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum AnyTypeRef<'a> {
    Variable(PlaceId),
    Default(&'a [u8], &'a [u8]),
}

fn apply_op<'a>(
    bump: &'a Bump,
    stack: &mut BumpVec<'a, AnyTypeExpr<'a>>,
    op: Operator,
) -> Result<AnyTypeExpr<'a>, Error> {
    match op {
        Operator::Default | Operator::OpenBrace => {
            let right = match stack.pop() {
                Some(s) => s,
                None => return Err(Error::Msg("Expression not finished")),
            };
            let left = match stack.pop() {
                Some(s) => s,
                None => return Err(Error::Msg("Expression not finished")),
            };
            let new = AnyTypeExpr::Default(bump.alloc(left), bump.alloc(right));
            Ok(new)
        }
    }
}

impl<'a> AnyTypeParser<'a> {
    pub fn new(bump: &'a Bump) -> AnyTypeParser<'a> {
        AnyTypeParser {
            stack: bumpvec_with_capacity(0x8, bump),
            op_stack: bumpvec_with_capacity(0x8, bump),
            bump,
        }
    }

    /// This can only parse entire text, or something inside (), or followed by another
    /// non-symbol token.
    /// Otherwise it assumes this to be int expr and returns Ok(None)
    pub fn try_parse_anytype_expr<'i>(
        &mut self,
        text: &'i [u8],
        parser: &mut ParserExprs,
        variable_decls: &CompilerVariables<'_>,
    ) -> Result<Option<(AnyTypeExpr<'a>, &'i [u8])>, Error> {
        let stack = &mut self.stack;
        stack.clear();
        let op_stack = &mut self.op_stack;
        op_stack.clear();
        let bump = self.bump;
        let mut rest = text;

        'outer: loop {
            let first = match rest.get(0) {
                Some(&s) => s,
                None => break,
            };
            if first == b'(' {
                rest = expect_token(rest, b"(")?;
                op_stack.push(Operator::OpenBrace);
                continue;
            }
            let (var, rest2) = match parser.parse_place_expr_pre_params(rest, variable_decls)
                .ok()
                .filter(|x| x.0.place_id.is_variable())
            {
                Some(s) => s,
                None => return Ok(None),
            };
            stack.push(AnyTypeExpr::Variable(var.place_id));
            rest = rest2;
            'op_loop: loop {
                let (next, rest2) = match split_first_token(rest) {
                    Some(s) => s,
                    None => break 'outer,
                };

                let op = if next == b"default" {
                    Operator::Default
                } else if next == b")" {
                    while let Some(op) = op_stack.pop() {
                        match op {
                            Operator::OpenBrace => {
                                rest = rest2;
                                continue 'op_loop;
                            }
                            _ => {
                                let new = apply_op(bump, stack, op)?;
                                stack.push(new);
                            }
                        }
                    }
                    break 'outer;
                } else {
                    return Ok(None);
                };
                rest = rest2;

                loop {
                    let last = match op_stack.last() {
                        Some(&s) => s,
                        None => break,
                    };
                    let last_priority = op_priority(last);
                    let op_priority = op_priority(op);

                    fn op_priority(op: Operator) -> u8 {
                        match op {
                            Operator::OpenBrace => 0,
                            Operator::Default => 1,
                        }
                    }
                    if last_priority < op_priority {
                        break;
                    }
                    if last != op && last_priority == op_priority {
                        return Err(Error::Msg("Conflicting operators"));
                    }
                    op_stack.pop();
                    let val = apply_op(bump, stack, last)?;
                    stack.push(val);
                }
                op_stack.push(op);
                break;
            }
        }

        while let Some(op) = op_stack.pop() {
            if let Operator::OpenBrace = op {
                return Err(Error::Msg("Unclosed '(')"));
            }
            let val = apply_op(bump, stack, op)?;
            stack.push(val);
        }
        if stack.is_empty() {
            Ok(None)
        } else {
            Ok(Some((stack.remove(0), rest)))
        }
    }
}

pub fn write_any_type(buf: &mut Vec<u8>, ty: &AnyTypeExpr<'_>) {
    match *ty {
        AnyTypeExpr::Variable(place) => {
            buf.push(0);
            let _ = buf.write_u32::<LittleEndian>(place.0);
        }
        AnyTypeExpr::Default(a, b) => {
            buf.push(1);
            write_any_type(buf, a);
            write_any_type(buf, b);
        }
    }
}

trait SliceExt {
    fn split_at_opt(&self, pos: usize) -> Option<(&Self, &Self)>;
}

impl<T> SliceExt for [T] {
    fn split_at_opt(&self, pos: usize) -> Option<(&Self, &Self)> {
        Some((self.get(..pos)?, self.get(pos..)?))
    }
}

pub fn read_any_type<'a>(buf: &'a [u8]) -> Option<(AnyTypeRef<'a>, &'a [u8])> {
    match *buf.get(0)? {
        0 => {
            let (data, rest) = buf.split_at_opt(5)?;
            let place = LittleEndian::read_u32(&data[1..]);
            Some((AnyTypeRef::Variable(PlaceId(place)), rest))
        }
        1 => {
            let rest = &buf[1..];
            let (_left, rest2) = read_any_type(rest)?;
            let left_len = rest.len() - rest2.len();
            let left = &rest[..left_len];
            let (_right, rest3) = read_any_type(rest2)?;
            let right_len = rest2.len() - rest3.len();
            let right = &rest2[..right_len];
            Some((AnyTypeRef::Default(left, right), rest3))
        }
        _ => None,
    }
}

#[cfg(test)]
pub(super) fn test_any_type_eq(a: &AnyTypeExpr<'_>, b: &AnyTypeRef<'_>) {
    match (*a, *b) {
        (AnyTypeExpr::Variable(x), AnyTypeRef::Variable(y)) => {
            assert_eq!(x, y);
        }
        (AnyTypeExpr::Default(x1, x2), AnyTypeRef::Default(y1, y2)) => {
            let (y1, rest) = read_any_type(y1).unwrap();
            assert_eq!(rest.len(), 0);
            let (y2, rest) = read_any_type(y2).unwrap();
            assert_eq!(rest.len(), 0);
            test_any_type_eq(x1, &y1);
            test_any_type_eq(x2, &y2);
        }
        _ => panic!("Expr mismatch {:?} vs {:?}", a, b),
    }
}

#[test]
fn test_write_read_any_type() {

    let mut buf = Vec::new();
    let ty1 = AnyTypeExpr::Variable(PlaceId(6));
    let ty2 = AnyTypeExpr::Variable(PlaceId(9));
    let ty3 = AnyTypeExpr::Default(&ty1, &ty2);
    let ty4 = AnyTypeExpr::Default(&ty1, &ty3);
    let ty5 = AnyTypeExpr::Default(&ty4, &ty4);
    write_any_type(&mut buf, &ty1);
    write_any_type(&mut buf, &ty2);
    write_any_type(&mut buf, &ty3);
    write_any_type(&mut buf, &ty1);
    write_any_type(&mut buf, &ty4);
    write_any_type(&mut buf, &ty5);
    write_any_type(&mut buf, &ty4);
    let rest = &buf[..];

    let (ty1b, rest) = read_any_type(rest).unwrap();
    test_any_type_eq(&ty1, &ty1b);

    let (ty2b, rest) = read_any_type(rest).unwrap();
    test_any_type_eq(&ty2, &ty2b);

    let (ty3b, rest) = read_any_type(rest).unwrap();
    test_any_type_eq(&ty3, &ty3b);

    let (ty1b, rest) = read_any_type(rest).unwrap();
    test_any_type_eq(&ty1, &ty1b);

    let (ty4b, rest) = read_any_type(rest).unwrap();
    test_any_type_eq(&ty4, &ty4b);

    let (ty5b, rest) = read_any_type(rest).unwrap();
    test_any_type_eq(&ty5, &ty5b);

    let (ty4b, rest) = read_any_type(rest).unwrap();
    test_any_type_eq(&ty4, &ty4b);

    assert_eq!(rest.len(), 0);
}

#[test]
fn parse_any_type() {
    use super::VariableStorage;
    let mut parser = super::Parser::new();
    let mut variable_decls = CompilerVariables::for_test(&[
        (b"var1", VariableStorage::SpriteLocal),
        (b"var2", VariableStorage::SpriteLocal),
        (b"global1", VariableStorage::Global),
        (b"global2", VariableStorage::Global),
    ]);

    let var1_id = variable_decls.variables.get(&b"var1"[..]).unwrap().place_id;
    let var2_id = variable_decls.variables.get(&b"var2"[..]).unwrap().place_id;
    let global1_id = variable_decls.variables.get(&b"global1"[..]).unwrap().place_id;
    let global2_id = variable_decls.variables.get(&b"global2"[..]).unwrap().place_id;

    let p = &mut parser.exprs;
    let v = &mut variable_decls;
    let b = &bumpalo::Bump::new();
    let mut a = AnyTypeParser::new(b);

    let (x, rest) = a.try_parse_anytype_expr(b"global1", p, v).unwrap().unwrap();
    assert_eq!(x, AnyTypeExpr::Variable(global1_id));
    assert_eq!(rest, &b""[..]);

    let (x, rest) =
        a.try_parse_anytype_expr(b"var1 default (var2 default global2))", p, v).unwrap().unwrap();
    let eq = AnyTypeExpr::Default(
        b.alloc(AnyTypeExpr::Variable(var1_id)),
        b.alloc(AnyTypeExpr::Default(
            b.alloc(AnyTypeExpr::Variable(var2_id)),
            b.alloc(AnyTypeExpr::Variable(global2_id)),
        )),
    );
    assert_eq!(x, eq);
    assert_eq!(rest, &b")"[..]);


    let x =
        a.try_parse_anytype_expr(b"var1 default (var2 default global2 + 5))", p, v).unwrap();
    assert!(x.is_none());
}

// This is slightly better for binary size than BumpVec::with_capcity_in,
// as bumpalo is otherwise pretty bad with monomorphizing
pub fn bumpvec_with_capacity<T>(cap: usize, bump: &Bump) -> BumpVec<'_, T> {
    let mut vec = BumpVec::new_in(bump);
    vec.reserve(cap);
    vec
}
