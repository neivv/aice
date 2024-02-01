
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum AiceCommandParam {
    IntExpr,
    False,
    True,
    U16Ffff,
    With,
    UnitRef,
    IntExprOrConstI8,
    IntExprOrConstU8,
    IntExprOrConstU16,
}

pub struct CommandParams {
    pub params: &'static [AiceCommandParam],
    pub flag_count: u8,
}

impl CommandParams {
    pub const fn new(params: &'static [AiceCommandParam]) -> CommandParams {
        use AiceCommandParam::*;
        let mut flag_count = 0u8;
        let mut i = 0;
        while i < params.len() {
            let param = &params[i];
            i += 1;
            match param {
                False | True | IntExprOrConstU8 | IntExprOrConstI8 | IntExprOrConstU16 => {
                    flag_count += 1;
                }
                _ => (),
            }
        }
        assert!(flag_count < 8);
        CommandParams {
            params,
            flag_count,
        }
    }
}

#[derive(Copy, Clone)]
struct AiceParamData {
    /// Second index is shorter length if flag 0x1 is set; otherwise equal to length
    lengths: [u8; 2],
    /// 0x1 = Uses flags for marking short constant or true / false
    /// 0x2 = Uses flags for marking bool true / false
    /// 0x4 = Contains expression
    flags: u8,
}

impl AiceParamData {
    const fn zeroed() -> AiceParamData {
        AiceParamData {
            lengths: [0, 0],
            flags: 0,
        }
    }
}

const fn gen_aice_param_data() -> [AiceParamData; 9] {
    let mut result = [AiceParamData::zeroed(); 9];
    result[AiceCommandParam::IntExpr as usize].lengths = [4, 4];
    result[AiceCommandParam::IntExpr as usize].flags = 0x4;
    result[AiceCommandParam::False as usize].flags = 0x3;
    result[AiceCommandParam::True as usize].flags = 0x3;
    result[AiceCommandParam::U16Ffff as usize].lengths = [2, 2];
    result[AiceCommandParam::With as usize].lengths = [4, 4];
    result[AiceCommandParam::UnitRef as usize].lengths = [2, 2];
    result[AiceCommandParam::IntExprOrConstI8 as usize].lengths = [4, 1];
    result[AiceCommandParam::IntExprOrConstI8 as usize].flags = 0x5;
    result[AiceCommandParam::IntExprOrConstU8 as usize].lengths = [4, 1];
    result[AiceCommandParam::IntExprOrConstU8 as usize].flags = 0x5;
    result[AiceCommandParam::IntExprOrConstU16 as usize].lengths = [4, 2];
    result[AiceCommandParam::IntExprOrConstU16 as usize].flags = 0x5;
    result
}

static AICE_PARAM_DATA: [AiceParamData; 9] = gen_aice_param_data();

/// Unsafe, but does "nothing worse" than out-of-bounds read from `input` if passed invalid input.
///
/// Input must also have 7 extra bytes past the actual code end so that potential u8 value
/// at last byte of script can be read safely as u64 and masked afterwards. (parse.rs does this)
///
/// Ideally the caller would still verify that the returned ptr is still in bounds.
pub unsafe fn read_aice_params(
    params: &CommandParams,
    input: *const u8,
    out: &mut [i32; 8],
    out_exprs: &mut [u8; 8],
) -> (*const u8, u8) {
    static MASKS: [u32; 8] = [0, 0xff, 0xffff, 0xff_ffff, 0xffff_ffff, 0, 0, 0];
    let mut read_ptr = input;

    let mut flags = 0u8;
    let mut expr_total = 0u8;
    if params.flag_count != 0 {
        flags = *read_ptr;
        read_ptr = read_ptr.add(1);
    }
    for i in 0..out.len() {
        let param = match params.params.get(i) {
            Some(&s) => s,
            None => break,
        };
        let param_data = &AICE_PARAM_DATA[param as usize];
        let param_flags = param_data.flags;
        let flags1 = flags & 0x1;
        let param_flags1 = param_flags & 0x1;
        let length = param_data.lengths[flags1 as usize] as usize;
        let mask = MASKS[length & 7];
        let value = ((read_ptr as *const u32).read_unaligned() & mask) |
            (flags1 & (param_flags >> 1)) as u32;
        read_ptr = read_ptr.add(length);
        flags = flags >> param_flags1;
        out[i] = value as i32;

        // Nonzero only if all:
        //      - param_flags 0x4 set
        //      - param_flags 0x1 clear or flags1 clear
        let expr_flags = (param_flags >> 2) & (!param_flags1 | !flags1);
        out_exprs[i] = expr_flags;
        expr_total |= expr_flags;
    }
    (read_ptr, expr_total)
}
