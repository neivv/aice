use bstr::{ByteSlice};

use super::{
    AnyExpr, CompilerExprs, CompilerVariables, Error, ParserExprs, VecIndex, VecOfVecs,
    parse_any_expr_allow_opt,
};

pub struct FormatStringId(pub u32);

impl VecIndex for FormatStringId {
    #[inline(always)]
    fn index(&self) -> Option<usize> {
        Some(self.0 as usize)
    }

    #[inline(always)]
    fn from_index(index: u32) -> Option<Self> {
        Some(FormatStringId(index))
    }
}

pub struct FormatStrings {
    strings: VecOfVecs<FormatStringId, FormatStringPart>,
    text_buffer: Vec<u8>,
}

pub enum FormatStringPart {
    /// Index, length in text_buffer
    String(u32, u32),
    Expr(AnyExpr),
}

impl FormatStrings {
    pub fn new() -> FormatStrings {
        FormatStrings {
            strings: VecOfVecs::new(),
            text_buffer: Vec::new(),
        }
    }

    pub(super) fn parse<'a>(
        &mut self,
        text: &'a [u8],
        parser: &mut ParserExprs,
        vars: &CompilerVariables<'a>,
        exprs: &mut CompilerExprs,
    ) -> Result<FormatStringId, Error> {
        if self.text_buffer.capacity() == 0 {
            self.text_buffer.reserve(0x800);
        }
        let mut text = text;
        self.strings.build(|| {
            if text.is_empty() {
                return Ok(None);
            }
            let offset = u32::try_from(self.text_buffer.len()).map_err(|_| Error::Overflow)?;
            let mut len = 0u32;
            loop {
                let text_end = text.bytes().position(|x| x == b'<' || x == b'{' || x == b'}')
                    .unwrap_or(text.len());
                if text_end != 0 {
                    let string = &text[..text_end];
                    text = &text[text_end..];
                    len = u32::try_from(text_end).ok()
                        .and_then(|x| len.checked_add(x))
                        .ok_or_else(|| Error::Overflow)?;
                    self.text_buffer.extend_from_slice(string);
                }
                match text.get(0).copied() {
                    Some(b'}') => {
                        return Err(Error::Msg("Unexpected '}' in format string"));
                    }
                    Some(b'{') => {
                        if len != 0 {
                            break;
                        }
                        let (expr, rest) =
                            parse_any_expr_allow_opt(&text[1..], parser, vars, exprs)?;
                        if rest.get(0).copied() != Some(b'}') {
                            return Err(Error::Expected(rest.into(), b"}"));
                        }
                        text = &rest[1..];
                        return Ok(Some(FormatStringPart::Expr(expr)));
                    }
                    Some(b'<') => {
                        len = len.checked_add(1).ok_or_else(|| Error::Overflow)?;
                        let rest = &text[1..];
                        let (hex, rest) = parse_hex_code(rest).unwrap_or((b'<', rest));
                        self.text_buffer.push(hex);
                        text = rest;
                    }
                    _ => break,
                }
            }
            Ok(Some(FormatStringPart::String(offset, len)))
        })
    }

    pub fn format<F: FnMut(&mut Vec<u8>, &AnyExpr)>(
        &self,
        id: FormatStringId,
        out: &mut Vec<u8>,
        mut callback: F,
    ) {
        for part in self.strings.get_or_empty(id) {
            match *part {
                FormatStringPart::String(start, len) => {
                    let start = start as usize;
                    let len = len as usize;
                    let text = &self.text_buffer[start..][..len];
                    out.extend_from_slice(text);
                }
                FormatStringPart::Expr(ref expr) => {
                    callback(out, expr);
                }
            }
        }
    }
}

/// Parses `55>` to 0x55 or 5>`
fn parse_hex_code(text: &[u8]) -> Option<(u8, &[u8])> {
    let chr2 = text.get(1).copied()?;
    let input;
    let rest;
    if chr2 != b'>' {
        if text.get(2).copied()? != b'>' {
            return None;
        }
        input = &text[..2];
        rest = &text[3..];
    } else {
        input = &text[..1];
        rest = &text[2..];
    }
    let result = u8::from_str_radix(input.to_str().ok()?, 16).ok()?;
    Some((result, rest))
}

#[test]
fn test_hex_code() {
    assert_eq!(parse_hex_code(b"5> 435").unwrap(), (5u8, &b" 435"[..]));
    assert_eq!(parse_hex_code(b"55> 435").unwrap(), (0x55u8, &b" 435"[..]));
    assert_eq!(parse_hex_code(b"ff> 435").unwrap(), (0xffu8, &b" 435"[..]));
    assert_eq!(parse_hex_code(b"ff 435"), None);
    assert_eq!(parse_hex_code(b"> 435"), None);
    assert_eq!(parse_hex_code(b"454> 435"), None);
}
