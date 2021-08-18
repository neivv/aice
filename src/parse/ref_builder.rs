use std::convert::TryFrom;

use bstr::ByteSlice;
use fxhash::FxHashMap;

use super::{UnitObject, UnitRefId, Error};

static FULL_UNIT_REFS: &[(&[u8], UnitObject)] = {
    use UnitObject::*;
    &[
        (b"bullet.parent", BulletParent),
        (b"bullet.target", BulletTarget),
        (b"bullet.previous_bounce_target", BulletPreviousBounceTarget),
        (b"unit", This),
        (b"unit.parent", UnitParent),
        (b"unit.nuke", Nuke),
        (b"unit.currently_building", CurrentlyBuilding),
        (b"unit.target", UnitTarget),
        (b"unit.transport", Transport),
        (b"unit.addon", Addon),
        (b"unit.subunit", Subunit),
        (b"unit.linked_nydus", LinkedNydus),
        (b"unit.powerup", Powerup),
        (b"unit.rally_target", RallyTarget),
        (b"unit.irradiated_by", IrradiatedBy),
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
    full_refs: FxHashMap<&'static [u8], UnitObject>,
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

    pub fn parse(&mut self, text: &[u8]) -> Result<UnitRefId, Error> {
        let error = || {
            Error::Dynamic(format!("Cannot parse '{}' as object place", text.as_bstr()))
        };

        if let Some(&obj) = self.full_refs.get(&text) {
            return Ok(UnitRefId(obj as u16));
        }
        self.buffer.clear();
        let (first, mut rest) = text.find_byte(b'.')
            .and_then(|first_dot| {
                let second_dot = first_dot + 1 + (&text[first_dot + 1..]).find_byte(b'.')?;
                let (first, rest) = text.split_at(second_dot);
                let &first = self.full_refs.get(&first)?;
                Some((first, rest))
            })
            .ok_or_else(error)?;
        self.buffer.push(first);
        while !rest.is_empty() {
            if rest[0] != b'.' {
                return Err(error());
            }
            rest = &rest[1..];
            let (next, new_rest) = rest.find_byte(b'.')
                .or_else(|| Some(rest.len()))
                .and_then(|dot| {
                    let (next, rest) = rest.split_at(dot);
                    let &next = self.unit_objects.get(&next)?;
                    Some((next, rest))
                })
                .ok_or_else(error)?;
            self.buffer.push(next);
            rest = new_rest;
        }
        self.add_multi_component_ref()
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
