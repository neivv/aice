use bstr::ByteSlice;
use fxhash::FxHashMap;

use super::{
    UnitObject, UnitObjectOrVariable, UnitRefId, Error, expect_token, split_first_token,
    VariableType, CompilerVariables
};

static BULLET_OBJECTS: &[(&[u8], UnitObject)] = {
    use UnitObject::*;
    &[
        (b"parent", BulletParent),
        (b"target", BulletTarget),
        (b"previous_bounce_target", BulletPreviousBounceTarget),
    ]
};

static UNIT_OBJECTS: &[(&[u8], UnitObject)] = {
    use UnitObject::*;
    &[
        (b"parent", UnitParent),
        (b"nuke", Nuke),
        (b"currently_building", CurrentlyBuilding),
        (b"target", UnitTarget),
        (b"transport", Transport),
        (b"addon", Addon),
        (b"subunit", Subunit),
        (b"linked_nydus", LinkedNydus),
        (b"powerup", Powerup),
        (b"rally_target", RallyTarget),
        (b"irradiated_by", IrradiatedBy),
    ]
};

pub struct UnitRefBuilder {
    result: Vec<Vec<UnitObjectOrVariable>>,
    existing_lookup: FxHashMap<Vec<UnitObjectOrVariable>, UnitRefId>,
    buffer: Vec<UnitObjectOrVariable>,
    unit_objects: FxHashMap<&'static [u8], UnitObject>,
    bullet_objects: FxHashMap<&'static [u8], UnitObject>,
}

impl UnitRefBuilder {
    pub fn new() -> UnitRefBuilder {
        UnitRefBuilder {
            result: Vec::new(),
            existing_lookup: FxHashMap::default(),
            buffer: Vec::new(),
            bullet_objects: BULLET_OBJECTS.iter().cloned().collect(),
            unit_objects: UNIT_OBJECTS.iter().cloned().collect(),
        }
    }

    pub fn finish(self) -> Vec<Vec<UnitObjectOrVariable>> {
        self.result
    }

    pub fn get_long_ref(&self, id: UnitRefId) -> Option<&[UnitObjectOrVariable]> {
        let index = id.0.checked_sub(UnitObject::_Last as u16)? as usize;
        self.result.get(index).map(|x| &x[..])
    }

    pub fn parse<'a, 'b>(
        &mut self,
        text: &'a [u8],
        variables: &CompilerVariables<'b>,
    ) -> Result<(UnitRefId, &'a [u8]), Error> {
        let error = || {
            Error::CannotParse(text.into(), "object place")
        };

        let (first, rest) = split_first_token(text)
            .ok_or_else(error)?;
        if rest.get(0).copied() != Some(b'.') {
            if first == b"unit" {
                return Ok((UnitRefId::this(), rest));
            } else {
                if let Some(var) = self.get_var(variables, first, true) {
                    self.buffer.clear();
                    self.buffer.push(var);
                    let result = self.add_multi_component_ref()?;
                    return Ok((result, rest));
                }
                return Err(error());
            }
        }
        let rest_no_dot = expect_token(rest, b".")?;
        self.parse_pre_split(first, rest_no_dot, rest, variables)
    }

    fn get_var<'b>(
        &self,
        variables: &CompilerVariables<'b>,
        name: &[u8],
        accept_global: bool,
    ) -> Option<UnitObjectOrVariable> {
        let var = variables.variables.get(name)?;
        if var.ty == VariableType::Unit {
            if !accept_global && var.place_id.is_global() {
                None
            } else {
                UnitObjectOrVariable::from_place(var.place_id)
            }
        } else {
            None
        }
    }

    /// If the text `unit.x.y.z ~~` is already split to `unit` and `x.y.z ~~`, this can
    /// be used to avoid redoing that work
    pub fn parse_pre_split<'a, 'b>(
        &mut self,
        first: &[u8],
        rest_no_dot: &'a [u8],
        rest: &'a [u8],
        variables: &CompilerVariables<'b>,
    ) -> Result<(UnitRefId, &'a [u8]), Error> {
        let error = || {
            Error::Dynamic(
                format!("Cannot parse '{}{}' as object place", first.as_bstr(), rest.as_bstr())
            )
        };

        self.buffer.clear();
        let mut rest = rest;
        if first == b"unit" {
            // Nothing
        } else if first == b"bullet" {
            let (second, rest2) = split_first_token(rest_no_dot)
                .ok_or_else(error)?;
            let obj = match self.bullet_objects.get(&second) {
                Some(&obj) => obj,
                None => return Err(error()),
            };
            if rest2.get(0).copied() != Some(b'.') {
                return Ok((UnitRefId(obj as u16), rest2));
            }
            self.buffer.push(UnitObjectOrVariable::new_object(obj));
            rest = rest2;
        } else if let Some(var) = self.get_var(variables, first, true) {
            self.buffer.push(var);
        } else {
            return Err(error());
        }

        while rest.get(0).copied() == Some(b'.') {
            let r2 = expect_token(rest, b".")?;
            let (token, r2) = split_first_token(r2)
                .ok_or_else(error)?;
            let obj = match self.unit_objects.get(token) {
                Some(&s) => UnitObjectOrVariable::new_object(s),
                None => match self.get_var(variables, token, false) {
                    Some(s) => s,
                    None => break,
                },
            };
            self.buffer.push(obj);
            rest = r2;
        }
        if self.buffer.len() == 0 {
            return Ok((UnitRefId::this(), rest));
        }
        if self.buffer.len() == 1 {
            let first = self.buffer[0];
            if let Some(obj) = first.if_unit_object() {
                return Ok((UnitRefId(obj as u16), rest));
            }
        }
        let result = self.add_multi_component_ref()?;
        Ok((result, rest))
    }

    /// Uses self.buffer
    fn add_multi_component_ref(&mut self) -> Result<UnitRefId, Error> {
        if let Some(&val) = self.existing_lookup.get(&self.buffer) {
            return Ok(val);
        }
        let new_id = UnitRefId(
            u16::try_from(self.existing_lookup.len() + UnitObject::_Last as usize)
                .map_err(|_| Error::Msg("Too many object refs"))?
        );
        self.result.push(self.buffer.clone());
        self.existing_lookup.insert(self.buffer.clone(), new_id);
        Ok(new_id)
    }
}

#[test]
fn unit_ref_tests_no_vars() {
    let mut builder = UnitRefBuilder::new();
    let vars = &CompilerVariables::for_test(&[]);
    assert_eq!(
        builder.parse(b"unit", vars).unwrap(),
        (UnitRefId(UnitObject::This as _), &b""[..])
    );
    assert_eq!(
        builder.parse(b"unit.target~~~", vars).unwrap(),
        (UnitRefId(UnitObject::UnitTarget as _), &b"~~~"[..])
    );
    assert_eq!(
        builder.parse(b"bullet.target.x", vars).unwrap(),
        (UnitRefId(UnitObject::BulletTarget as _), &b".x"[..])
    );
    assert_eq!(
        builder.parse(b"unit.var1 + 5", vars).unwrap(),
        (UnitRefId(UnitObject::This as _), &b".var1 + 5"[..])
    );
    assert_eq!(
        builder.parse(b"bullet.target.target.irradiated_by.target", vars).unwrap(),
        (UnitRefId(UnitObject::_Last as _), &b""[..])
    );
    let refs = builder.finish();
    assert_eq!(refs.len(), 1);
    assert_eq!(
        &refs[0][..],
        &[
            UnitObjectOrVariable::new_object(UnitObject::BulletTarget),
            UnitObjectOrVariable::new_object(UnitObject::UnitTarget),
            UnitObjectOrVariable::new_object(UnitObject::IrradiatedBy),
            UnitObjectOrVariable::new_object(UnitObject::UnitTarget),
        ],
    );
}

#[test]
fn unit_ref_tests_with_vars() {
    use super::VariableStorage;
    let mut builder = UnitRefBuilder::new();
    let vars = &CompilerVariables::for_test_unit(&[
        (b"unitvar1", VariableStorage::Global),
        (b"unitvar2", VariableStorage::SpriteLocal),
        (b"unitvar3", VariableStorage::SpriteLocal),
    ]);
    let unitvar1_place = vars.variables.get(&b"unitvar1"[..]).unwrap().place_id;
    let unitvar2_place = vars.variables.get(&b"unitvar2"[..]).unwrap().place_id;
    let unitvar3_place = vars.variables.get(&b"unitvar3"[..]).unwrap().place_id;

    // Tried to use a global as spritelocal
    // (Could also be an error)
    assert_eq!(
        builder.parse(b"unit.unitvar1", vars).unwrap(),
        (UnitRefId(UnitObject::This as _), &b".unitvar1"[..])
    );
    // Nonexistent variable
    assert_eq!(
        builder.parse(b"unit.whatever", vars).unwrap(),
        (UnitRefId(UnitObject::This as _), &b".whatever"[..])
    );

    let first_long_ref = UnitObject::_Last as u16;
    assert_eq!(
        builder.parse(b"unit.unitvar2", vars).unwrap(),
        (UnitRefId(first_long_ref), &b""[..]),
    );
    // Same as above (Technically not since above implies unit existing?)
    assert_eq!(
        builder.parse(b"unitvar2", vars).unwrap(),
        (UnitRefId(first_long_ref), &b""[..]),
    );
    assert_eq!(
        builder.parse(b"unit.unitvar2.unitvar3", vars).unwrap(),
        (UnitRefId(first_long_ref + 1), &b""[..]),
    );
    assert_eq!(
        builder.parse(b"bullet.target.unitvar2.unitvar3", vars).unwrap(),
        (UnitRefId(first_long_ref + 2), &b""[..]),
    );
    assert_eq!(
        builder.parse(b"unitvar1.unitvar2.unitvar3", vars).unwrap(),
        (UnitRefId(first_long_ref + 3), &b""[..]),
    );
    let refs = builder.finish();
    assert_eq!(refs.len(), 4);
    assert_eq!(
        &refs[0][..],
        &[
            UnitObjectOrVariable::from_place(unitvar2_place).unwrap(),
        ],
    );
    assert_eq!(
        &refs[1][..],
        &[
            UnitObjectOrVariable::from_place(unitvar2_place).unwrap(),
            UnitObjectOrVariable::from_place(unitvar3_place).unwrap(),
        ],
    );
    assert_eq!(
        &refs[2][..],
        &[
            UnitObjectOrVariable::new_object(UnitObject::BulletTarget),
            UnitObjectOrVariable::from_place(unitvar2_place).unwrap(),
            UnitObjectOrVariable::from_place(unitvar3_place).unwrap(),
        ],
    );
    assert_eq!(
        &refs[3][..],
        &[
            UnitObjectOrVariable::from_place(unitvar1_place).unwrap(),
            UnitObjectOrVariable::from_place(unitvar2_place).unwrap(),
            UnitObjectOrVariable::from_place(unitvar3_place).unwrap(),
        ],
    );
}
