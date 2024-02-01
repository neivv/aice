use super::{
    BwCommandParam, CompilerLabels, CompilerOutput, CompilerExprs, SpriteLocalSetId, Error,
    VecOfVecs, PlaceId, SetExpr, BoolExpr, IntExpr, BlockScope, Label, ExprId,
};

/// Output (or in-out mutable state) for parse_add_command.
pub(super) struct ParseStage3OutputMain<'a, 'text> {
    pub out: &'a mut CompilerOutput<'text>,
    pub expr_ids: &'a mut CompilerExprs,
    pub sprite_local_set_builder: &'a mut VecOfVecs<SpriteLocalSetId, (u32, ExprId)>,
}

impl<'a, 'text> ParseStage3OutputMain<'a, 'text> {
    pub fn revert_on_err<F, O>(&mut self, func: F) -> Result<O, Error>
    where F: FnOnce(ParseStage3Output<'_, 'a, 'text>) -> Result<O, Error>
    {
        let mut out = ParseStage3Output {
            inner: self,
        };
        out.revert_on_err(func)
    }
}

pub(super) struct ParseStage3Output<'a, 'b, 'text> {
    inner: &'a mut ParseStage3OutputMain<'b, 'text>,
}

impl<'a, 'b, 'text> ParseStage3Output<'a, 'b, 'text> {
    pub fn revert_on_err<F, O>(&mut self, func: F) -> Result<O, Error>
    where F: FnOnce(ParseStage3Output<'_, 'b, 'text>) -> Result<O, Error>
    {
        let inner = &mut *self.inner;
        let out_revert = inner.out.revert_pos();
        let sprite_local_set_revert = inner.sprite_local_set_builder.revert_pos();
        let copy = ParseStage3Output {
            inner,
        };
        let result = func(copy);
        let inner = &mut *self.inner;
        if result.is_err() {
            let ParseStage3OutputMain {
                out,
                // Not reverted
                expr_ids: _,
                sprite_local_set_builder,
            } = inner;
            out.revert(&out_revert);
            sprite_local_set_builder.revert(&sprite_local_set_revert);
        }
        result
    }

    /// CompilerExprs doesn't need to be reverted on error,
    /// can just give access to it.
    pub fn expr_ids(&mut self) -> &mut CompilerExprs {
        &mut self.inner.expr_ids
    }

    pub fn flow_to_bw(&mut self) {
        self.inner.out.flow_to_bw()
    }

    pub fn add_aice_command(&mut self, byte: u8) {
        self.inner.out.add_aice_command(byte)
    }

    pub fn add_aice_command_u8(&mut self, byte: u8, param: u8) {
        self.inner.out.add_aice_command_u8(byte, param)
    }

    pub fn add_aice_command_u32(&mut self, byte: u8, param: u32) {
        self.inner.out.add_aice_command_u32(byte, param)
    }

    pub fn add_aice_code(&mut self, code: &[u8]) {
        self.inner.out.add_flow_to_aice();
        self.inner.out.add_aice_code(code)
    }

    pub fn add_bw_code(&mut self, code: &[u8]) {
        self.inner.out.add_bw_code(code)
    }

    pub fn aice_code_len(&self) -> usize {
        self.inner.out.aice_code.len()
    }

    pub fn build_sprite_local_set<F>(self, mut fun: F) -> Result<(SpriteLocalSetId, Self), Error>
    where F: FnMut(&mut CompilerExprs) -> Result<Option<(u32, ExprId)>, Error>
    {
        let expr_ids = &mut *self.inner.expr_ids;
        self.inner.sprite_local_set_builder.build(|| fun(expr_ids)).map(|x| (x, self))
    }

    pub fn add_set(
        &mut self,
        write_buffer: &mut Vec<u8>,
        place: PlaceId,
        if_uninit: bool,
        value: SetExpr<'_>,
        place_vars: &mut [Option<Box<IntExpr>>; 4],
    ) {
        let expr_ids = &mut self.inner.expr_ids;
        self.inner.out.add_set(expr_ids, write_buffer, place, if_uninit, value, place_vars)
    }

    pub fn add_bw_command(
        self,
        labels: &CompilerLabels<'text>,
        byte: u8,
        param_text: &'text [u8],
        params: &[BwCommandParam],
        ends_flow: bool,
        block_scope: &BlockScope<'text, '_>,
    ) -> Result<Self, Error> {
        self.inner.out.add_bw_command(labels, byte, param_text, params, ends_flow, block_scope)?;
        Ok(self)
    }

    pub fn add_if(
        self,
        labels: &CompilerLabels<'text>,
        condition: BoolExpr,
        dest: Label<'text>,
        block_scope: &BlockScope<'text, '_>,
        is_call: bool,
    ) -> Result<Self, Error> {
        let expr_ids = &mut self.inner.expr_ids;
        self.inner.out.add_if(expr_ids, labels, condition, dest, block_scope, is_call)?;
        Ok(self)
    }

    pub fn add_call(
        self,
        labels: &CompilerLabels<'text>,
        dest: Label<'text>,
        block_scope: &BlockScope<'text, '_>
    ) -> Result<Self, Error> {
        self.inner.out.add_call(labels, dest, block_scope)?;
        Ok(self)
    }

    pub fn set_current_line(&mut self, line: u32) {
        self.inner.out.set_current_line(line)
    }
}
