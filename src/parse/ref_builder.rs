use std::convert::TryFrom;

use bstr::ByteSlice;
use fxhash::FxHashMap;

use super::{UnitObject, UnitRefId, Error, expect_token, split_first_token};

static FULL_UNIT_REFS: &[((bool, &[u8]), UnitObject)] = {
    use UnitObject::*;
    &[
        ((false, b"parent"), BulletParent),
        ((false, b"target"), BulletTarget),
        ((false, b"previous_bounce_target"), BulletPreviousBounceTarget),
        ((true, b"parent"), UnitParent),
        ((true, b"nuke"), Nuke),
        ((true, b"currently_building"), CurrentlyBuilding),
        ((true, b"target"), UnitTarget),
        ((true, b"transport"), Transport),
        ((true, b"addon"), Addon),
        ((true, b"subunit"), Subunit),
        ((true, b"linked_nydus"), LinkedNydus),
        ((true, b"powerup"), Powerup),
        ((true, b"rally_target"), RallyTarget),
        ((true, b"irradiated_by"), IrradiatedBy),
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
    result: Vec<Vec<UnitObject>>,
    existing_lookup: FxHashMap<Vec<UnitObject>, UnitRefId>,
    buffer: Vec<UnitObject>,
    unit_objects: FxHashMap<&'static [u8], UnitObject>,
    // bool true = unit, false = bullet
    full_refs: FxHashMap<(bool, &'static [u8]), UnitObject>,
}

impl UnitRefBuilder {
    pub fn new() -> UnitRefBuilder {
        UnitRefBuilder {
            result: Vec::new(),
            existing_lookup: FxHashMap::default(),
            buffer: Vec::new(),
            full_refs: FULL_UNIT_REFS.iter().cloned().collect(),
            unit_objects: UNIT_OBJECTS.iter().cloned().collect(),
        }
    }

    pub fn finish(self) -> Vec<Vec<UnitObject>> {
        self.result
    }

    pub fn parse<'a>(&mut self, text: &'a [u8]) -> Result<(UnitRefId, &'a [u8]), Error> {
        let error = || {
            Error::Dynamic(format!("Cannot parse '{}' as object place", text.as_bstr()))
        };

        let (first, rest) = split_first_token(text)
            .ok_or_else(error)?;
        let is_unit = if first == b"unit" {
            true
        } else if first == b"bullet" {
            false
        } else {
            return Err(error());
        };
        if is_unit && rest.get(0).copied() != Some(b'.') {
            return Ok((UnitRefId::this(), rest));
        }
        let rest_no_dot = expect_token(rest, b".")?;
        self.parse_pre_split(first, rest_no_dot, rest)
    }

    /// If the text `unit.x.y.z ~~` is already split to `unit` and `x.y.z ~~`, this can
    /// be used to avoid redoing that work
    pub fn parse_pre_split<'a>(
        &mut self,
        first: &[u8],
        rest_no_dot: &'a [u8],
        rest: &'a [u8],
    ) -> Result<(UnitRefId, &'a [u8]), Error> {
        let error = || {
            Error::Dynamic(
                format!("Cannot parse '{}{}' as object place", first.as_bstr(), rest.as_bstr())
            )
        };

        let is_unit = if first == b"unit" {
            true
        } else if first == b"bullet" {
            false
        } else {
            return Err(error());
        };

        let (second, rest2) = split_first_token(rest_no_dot)
            .ok_or_else(error)?;

        let obj = match self.full_refs.get(&(is_unit, second)) {
            Some(&obj) => obj,
            None => {
                if is_unit {
                    return Ok((UnitRefId::this(), rest));
                } else {
                    return Err(error());
                }
            }
        };

        self.buffer.clear();
        self.buffer.push(obj);
        let mut rest = rest2;
        while rest.get(0).copied() == Some(b'.') {
            let r2 = expect_token(rest, b".")?;
            let (token, r2) = split_first_token(r2)
                .ok_or_else(error)?;
            let &obj = match self.unit_objects.get(token) {
                Some(s) => s,
                None => break,
            };
            rest = r2;
            self.buffer.push(obj);
        }
        if self.buffer.len() == 1 {
            return Ok((UnitRefId(self.buffer[0] as u16), rest));
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
fn unit_ref_tests() {
    let mut builder = UnitRefBuilder::new();
    assert_eq!(
        builder.parse(b"unit").unwrap(),
        (UnitRefId(UnitObject::This as _), &b""[..])
    );
    assert_eq!(
        builder.parse(b"unit.target~~~").unwrap(),
        (UnitRefId(UnitObject::UnitTarget as _), &b"~~~"[..])
    );
    assert_eq!(
        builder.parse(b"bullet.target.x").unwrap(),
        (UnitRefId(UnitObject::BulletTarget as _), &b".x"[..])
    );
    assert_eq!(
        builder.parse(b"unit.var1 + 5").unwrap(),
        (UnitRefId(UnitObject::This as _), &b".var1 + 5"[..])
    );
    assert_eq!(
        builder.parse(b"bullet.target.target.irradiated_by.target").unwrap(),
        (UnitRefId(UnitObject::_Last as _), &b""[..])
    );
    let refs = builder.finish();
    assert_eq!(refs.len(), 1);
    assert_eq!(
        &refs[0][..],
        &[
            UnitObject::BulletTarget,
            UnitObject::UnitTarget,
            UnitObject::IrradiatedBy,
            UnitObject::UnitTarget,
        ],
    );
}
