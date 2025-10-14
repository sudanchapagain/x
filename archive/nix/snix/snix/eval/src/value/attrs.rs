//! This module implements Nix attribute sets. They have flexible
//! backing implementations, as they are used in very versatile
//! use-cases that are all exposed the same way in the language
//! surface.
//!
//! Due to this, construction and management of attribute sets has
//! some peculiarities that are encapsulated within this module.
use std::borrow::Borrow;
use std::collections::{BTreeMap, hash_map};
use std::iter::FromIterator;
use std::rc::Rc;
use std::sync::LazyLock;

use bstr::{BStr, ByteSlice};
use itertools::Itertools as _;
use rustc_hash::FxHashMap;
use serde::Deserialize;
use serde::de::{Deserializer, Error, Visitor};

use super::TotalDisplay;
use super::Value;
use super::string::NixString;
use super::thunk::ThunkSet;
use crate::CatchableErrorKind;
use crate::errors::ErrorKind;

static NAME: LazyLock<NixString> = LazyLock::new(|| "name".into());
static VALUE: LazyLock<NixString> = LazyLock::new(|| "value".into());

#[cfg(test)]
mod tests;

#[derive(Clone, Debug, Deserialize, Default)]
pub(super) enum AttrsRep {
    #[default]
    Empty,

    Map(FxHashMap<NixString, Value>),

    /// Warning: this represents a **two**-attribute attrset, with
    /// attribute names "name" and "value", like `{name="foo";
    /// value="bar";}`, *not* `{foo="bar";}`!
    KV {
        name: Value,
        value: Value,
    },
}

impl AttrsRep {
    fn select(&self, key: &BStr) -> Option<&Value> {
        match self {
            AttrsRep::Empty => None,

            AttrsRep::KV { name, value } => match &**key {
                b"name" => Some(name),
                b"value" => Some(value),
                _ => None,
            },

            AttrsRep::Map(map) => map.get(key),
        }
    }

    fn contains(&self, key: &BStr) -> bool {
        match self {
            AttrsRep::Empty => false,
            AttrsRep::KV { .. } => key == "name" || key == "value",
            AttrsRep::Map(map) => map.contains_key(key),
        }
    }
}

#[repr(transparent)]
#[derive(Clone, Debug, Default)]
pub struct NixAttrs(pub(super) Rc<AttrsRep>);

impl From<AttrsRep> for NixAttrs {
    fn from(rep: AttrsRep) -> Self {
        NixAttrs(Rc::new(rep))
    }
}

impl<K, V> FromIterator<(K, V)> for NixAttrs
where
    NixString: From<K>,
    Value: From<V>,
{
    fn from_iter<T>(iter: T) -> NixAttrs
    where
        T: IntoIterator<Item = (K, V)>,
    {
        AttrsRep::Map(
            iter.into_iter()
                .map(|(k, v)| (k.into(), v.into()))
                .collect(),
        )
        .into()
    }
}

impl From<BTreeMap<NixString, Value>> for NixAttrs {
    fn from(map: BTreeMap<NixString, Value>) -> Self {
        AttrsRep::Map(map.into_iter().collect()).into()
    }
}

impl From<FxHashMap<NixString, Value>> for NixAttrs {
    fn from(map: FxHashMap<NixString, Value>) -> Self {
        AttrsRep::Map(map).into()
    }
}

impl TotalDisplay for NixAttrs {
    fn total_fmt(&self, f: &mut std::fmt::Formatter<'_>, set: &mut ThunkSet) -> std::fmt::Result {
        if self.is_derivation() {
            write!(f, "«derivation")?;
            // We cant use the default TotalDisplay implementation, because nix doesn't put quotes
            // around the path here.
            if let Some(Value::Thunk(p)) = self.select("drvPath") {
                if p.is_forced()
                    && let Ok(drv) = p.value().to_contextful_str()
                {
                    write!(f, " {}", drv.to_str_lossy())?;
                };
            }
            return write!(f, "»");
        }

        f.write_str("{ ")?;

        for (name, value) in self.iter_sorted() {
            write!(f, "{} = ", name.ident_str())?;
            value.total_fmt(f, set)?;
            f.write_str("; ")?;
        }

        f.write_str("}")
    }
}

impl<'de> Deserialize<'de> for NixAttrs {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct MapVisitor;

        impl<'de> Visitor<'de> for MapVisitor {
            type Value = NixAttrs;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("a valid Nix attribute set")
            }

            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut stack_array = Vec::with_capacity(map.size_hint().unwrap_or(0) * 2);

                while let Some((key, value)) = map.next_entry()? {
                    stack_array.push(key);
                    stack_array.push(value);
                }

                Ok(NixAttrs::construct(stack_array.len() / 2, stack_array)
                    .map_err(A::Error::custom)?
                    .expect("Catchable values are unreachable here"))
            }
        }

        deserializer.deserialize_map(MapVisitor)
    }
}

impl NixAttrs {
    pub fn empty() -> Self {
        AttrsRep::Empty.into()
    }

    /// Compare two attribute sets by pointer equality. Only makes
    /// sense for some attribute set representations, i.e. returning
    /// `false` does not mean that the two attribute sets do not have
    /// equal *content*.
    pub fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    /// Return an attribute set containing the merge of the two
    /// provided sets. Keys from the `other` set have precedence.
    pub fn update(self, other: Self) -> Self {
        // Short-circuit on some optimal cases:
        match (self.0.as_ref(), other.0.as_ref()) {
            (AttrsRep::Empty, AttrsRep::Empty) => return self,
            (AttrsRep::Empty, _) => return other,
            (_, AttrsRep::Empty) => return self,
            (AttrsRep::KV { .. }, AttrsRep::KV { .. }) => return other,

            // Explicitly handle all branches instead of falling
            // through, to ensure that we get at least some compiler
            // errors if variants are modified.
            (AttrsRep::Map(_), AttrsRep::Map(_))
            | (AttrsRep::Map(_), AttrsRep::KV { .. })
            | (AttrsRep::KV { .. }, AttrsRep::Map(_)) => {}
        };

        // Slightly more advanced, but still optimised updates
        match (Rc::unwrap_or_clone(self.0), Rc::unwrap_or_clone(other.0)) {
            (AttrsRep::Map(mut m), AttrsRep::KV { name, value }) => {
                m.insert(NAME.clone(), name);
                m.insert(VALUE.clone(), value);
                AttrsRep::Map(m).into()
            }

            (AttrsRep::KV { name, value }, AttrsRep::Map(mut m)) => {
                match m.entry(NAME.clone()) {
                    hash_map::Entry::Vacant(e) => {
                        e.insert(name);
                    }

                    hash_map::Entry::Occupied(_) => { /* name from `m` has precedence */ }
                };

                match m.entry(VALUE.clone()) {
                    hash_map::Entry::Vacant(e) => {
                        e.insert(value);
                    }

                    hash_map::Entry::Occupied(_) => { /* value from `m` has precedence */ }
                };

                AttrsRep::Map(m).into()
            }

            // Plain merge of maps.
            (AttrsRep::Map(mut m1), AttrsRep::Map(mut m2)) => {
                let map = if m1.len() >= m2.len() {
                    m1.extend(m2);
                    m1
                } else {
                    for (key, val) in m1.into_iter() {
                        m2.entry(key).or_insert(val);
                    }
                    m2
                };
                AttrsRep::Map(map).into()
            }

            // Cases handled above by the borrowing match:
            _ => unreachable!(),
        }
    }

    /// Return the number of key-value entries in an attrset.
    pub fn len(&self) -> usize {
        match self.0.as_ref() {
            AttrsRep::Map(map) => map.len(),
            AttrsRep::Empty => 0,
            AttrsRep::KV { .. } => 2,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self.0.as_ref() {
            AttrsRep::Map(map) => map.is_empty(),
            AttrsRep::Empty => true,
            AttrsRep::KV { .. } => false,
        }
    }

    /// Select a value from an attribute set by key.
    pub fn select<K>(&self, key: &K) -> Option<&Value>
    where
        K: Borrow<BStr> + ?Sized,
    {
        self.0.select(key.borrow())
    }

    /// Select a required value from an attribute set by key, return
    /// an `AttributeNotFound` error if it is missing.
    pub fn select_required<K>(&self, key: &K) -> Result<&Value, ErrorKind>
    where
        K: Borrow<BStr> + ?Sized,
    {
        self.select(key)
            .ok_or_else(|| ErrorKind::AttributeNotFound {
                name: key.borrow().to_string(),
            })
    }

    pub fn contains<'a, K: 'a>(&self, key: K) -> bool
    where
        &'a BStr: From<K>,
    {
        self.0.contains(key.into())
    }

    /// Construct an iterator over all the key-value pairs in the attribute set.
    #[allow(clippy::needless_lifetimes)]
    pub fn iter<'a>(&'a self) -> Iter<KeyValue<'a>> {
        Iter(match &self.0.as_ref() {
            AttrsRep::Map(map) => KeyValue::Map(map.iter()),
            AttrsRep::Empty => KeyValue::Empty,

            AttrsRep::KV { name, value } => KeyValue::KV {
                name,
                value,
                at: IterKV::default(),
            },
        })
    }

    /// Same as iter(), but marks call sites which rely on the
    /// iteration being lexicographic.
    pub fn iter_sorted(&self) -> Iter<KeyValue<'_>> {
        Iter(match self.0.as_ref() {
            AttrsRep::Empty => KeyValue::Empty,
            AttrsRep::Map(map) => {
                let sorted = map.iter().sorted_by_key(|x| x.0);
                KeyValue::Sorted(sorted)
            }
            AttrsRep::KV { name, value } => KeyValue::KV {
                name,
                value,
                at: IterKV::default(),
            },
        })
    }

    /// Same as [IntoIterator::into_iter], but marks call sites which rely on the
    /// iteration being lexicographic.
    pub fn into_iter_sorted(self) -> OwnedAttrsIterator {
        let iter = match Rc::<AttrsRep>::try_unwrap(self.0) {
            Ok(attrs) => match attrs {
                AttrsRep::Empty => IntoIterRepr::Empty,
                AttrsRep::Map(map) => {
                    IntoIterRepr::Finite(map.into_iter().sorted_by(|(a, _), (b, _)| a.cmp(b)))
                }
                AttrsRep::KV { name, value } => IntoIterRepr::Finite(
                    vec![(NAME.clone(), name), (VALUE.clone(), value)].into_iter(),
                ),
            },
            Err(rc) => match rc.as_ref() {
                AttrsRep::Empty => IntoIterRepr::Empty,
                AttrsRep::Map(map) => IntoIterRepr::Finite(
                    map.iter()
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .sorted_by(|(a, _), (b, _)| a.cmp(b)),
                ),
                AttrsRep::KV { name, value } => IntoIterRepr::Finite(
                    vec![(NAME.clone(), name.clone()), (VALUE.clone(), value.clone())].into_iter(),
                ),
            },
        };
        OwnedAttrsIterator(iter)
    }

    /// Construct an iterator over all the keys of the attribute set
    pub fn keys(&self) -> Keys {
        Keys(match self.0.as_ref() {
            AttrsRep::Empty => KeysInner::Empty,
            AttrsRep::KV { .. } => KeysInner::KV(IterKV::default()),

            // TODO(tazjin): only sort when required, not always.
            AttrsRep::Map(m) => KeysInner::Map(m.keys()),
        })
    }

    /// Same as [Self::keys], but marks call sites which rely on the
    /// iteration being lexicographic.
    pub fn keys_sorted(&self) -> Keys {
        Keys(match self.0.as_ref() {
            AttrsRep::Map(map) => KeysInner::Sorted(map.keys().sorted()),
            AttrsRep::Empty => KeysInner::Empty,
            AttrsRep::KV { .. } => KeysInner::KV(IterKV::default()),
        })
    }

    /// Implement construction logic of an attribute set, to encapsulate
    /// logic about attribute set optimisations inside of this module.
    pub fn construct(
        count: usize,
        mut stack_slice: Vec<Value>,
    ) -> Result<Result<Self, CatchableErrorKind>, ErrorKind> {
        debug_assert!(
            stack_slice.len() == count * 2,
            "construct_attrs called with count == {}, but slice.len() == {}",
            count,
            stack_slice.len(),
        );

        // Optimisation: Empty attribute set
        if count == 0 {
            return Ok(Ok(AttrsRep::Empty.into()));
        }

        // Optimisation: KV pattern
        if count == 2 {
            if let Some(kv) = attempt_optimise_kv(&mut stack_slice) {
                return Ok(Ok(kv));
            }
        }

        let mut attrs_map = FxHashMap::with_capacity_and_hasher(count, rustc_hash::FxBuildHasher);

        for _ in 0..count {
            let value = stack_slice.pop().unwrap();
            let key = stack_slice.pop().unwrap();

            match key {
                Value::String(ks) => set_attr(&mut attrs_map, ks, value)?,

                Value::Null => {
                    // This is in fact valid, but leads to the value
                    // being ignored and nothing being set, i.e. `{
                    // ${null} = 1; } => { }`.
                    continue;
                }

                Value::Catchable(err) => return Ok(Err(*err)),

                other => return Err(ErrorKind::InvalidAttributeName(other)),
            }
        }

        Ok(Ok(AttrsRep::Map(attrs_map).into()))
    }

    /// Construct an optimized "KV"-style attribute set given the value for the
    /// `"name"` key, and the value for the `"value"` key
    pub(crate) fn from_kv(name: Value, value: Value) -> Self {
        AttrsRep::KV { name, value }.into()
    }

    /// Calculate the intersection of the attribute sets.
    /// The right side value is used when the keys match.
    pub(crate) fn intersect(&self, other: &Self) -> NixAttrs {
        match (self.0.as_ref(), other.0.as_ref()) {
            (AttrsRep::Empty, _) | (_, AttrsRep::Empty) => AttrsRep::Empty.into(),
            (AttrsRep::Map(lhs), AttrsRep::Map(rhs)) => {
                let mut out = FxHashMap::with_capacity_and_hasher(
                    std::cmp::min(lhs.len(), rhs.len()),
                    rustc_hash::FxBuildHasher,
                );
                if lhs.len() < rhs.len() {
                    for key in lhs.keys() {
                        if let Some(val) = rhs.get(key) {
                            out.insert(key.clone(), val.clone());
                        }
                    }
                } else {
                    for (key, val) in rhs.iter() {
                        if lhs.contains_key(key) {
                            out.insert(key.clone(), val.clone());
                        }
                    }
                };
                out.into()
            }
            (AttrsRep::Map(map), AttrsRep::KV { name, value }) => {
                let mut out = FxHashMap::with_capacity_and_hasher(2, rustc_hash::FxBuildHasher);
                if map.contains_key(NAME.as_bstr()) {
                    out.insert(NAME.clone(), name.clone());
                }
                if map.contains_key(VALUE.as_bstr()) {
                    out.insert(VALUE.clone(), value.clone());
                }

                if out.is_empty() {
                    NixAttrs::empty()
                } else {
                    out.into()
                }
            }
            (AttrsRep::KV { .. }, AttrsRep::Map(map)) => {
                let mut out = FxHashMap::with_capacity_and_hasher(2, rustc_hash::FxBuildHasher);
                if let Some(name) = map.get(NAME.as_bstr()) {
                    out.insert(NAME.clone(), name.clone());
                }
                if let Some(value) = map.get(VALUE.as_bstr()) {
                    out.insert(VALUE.clone(), value.clone());
                }

                if out.is_empty() {
                    NixAttrs::empty()
                } else {
                    out.into()
                }
            }
            (AttrsRep::KV { .. }, AttrsRep::KV { .. }) => other.clone(),
        }
    }

    /// Returns whether this attr set is a derivation.
    pub fn is_derivation(&self) -> bool {
        let Some(Value::String(kind)) = self.select("type") else {
            return false;
        };
        *kind == "derivation"
    }
}

impl IntoIterator for NixAttrs {
    type Item = (NixString, Value);
    type IntoIter = OwnedAttrsIterator;

    fn into_iter(self) -> Self::IntoIter {
        match Rc::unwrap_or_clone(self.0) {
            AttrsRep::Empty => OwnedAttrsIterator(IntoIterRepr::Empty),
            AttrsRep::KV { name, value } => OwnedAttrsIterator(IntoIterRepr::Finite(
                vec![(NAME.clone(), name), (VALUE.clone(), value)].into_iter(),
            )),
            AttrsRep::Map(map) => OwnedAttrsIterator(IntoIterRepr::Map(map.into_iter())),
        }
    }
}

/// In Nix, name/value attribute pairs are frequently constructed from
/// literals. This particular case should avoid allocation of a map,
/// additional heap values etc. and use the optimised `KV` variant
/// instead.
///
/// ```norust
/// `slice` is the top of the stack from which the attrset is being
/// constructed, e.g.
///
///   slice: [ "value" 5 "name" "foo" ]
///   index:   0       1 2      3
///   stack:   3       2 1      0
/// ```
fn attempt_optimise_kv(slice: &mut [Value]) -> Option<NixAttrs> {
    let (name_idx, value_idx) = {
        match (&slice[2], &slice[0]) {
            (Value::String(s1), Value::String(s2)) if (*s1 == *NAME && *s2 == *VALUE) => (3, 1),
            (Value::String(s1), Value::String(s2)) if (*s1 == *VALUE && *s2 == *NAME) => (1, 3),

            // Technically this branch lets type errors pass,
            // but they will be caught during normal attribute
            // set construction instead.
            _ => return None,
        }
    };

    Some(NixAttrs::from_kv(
        slice[name_idx].clone(),
        slice[value_idx].clone(),
    ))
}

/// Set an attribute on an in-construction attribute set, while
/// checking against duplicate keys.
fn set_attr(
    map: &mut FxHashMap<NixString, Value>,
    key: NixString,
    value: Value,
) -> Result<(), ErrorKind> {
    match map.entry(key) {
        hash_map::Entry::Occupied(entry) => Err(ErrorKind::DuplicateAttrsKey {
            key: entry.key().to_string(),
        }),

        hash_map::Entry::Vacant(entry) => {
            entry.insert(value);
            Ok(())
        }
    }
}

/// Internal helper type to track the iteration status of an iterator
/// over the name/value representation.
#[derive(Debug, Default)]
pub enum IterKV {
    #[default]
    Name,
    Value,
    Done,
}

impl IterKV {
    fn next(&mut self) {
        match *self {
            Self::Name => *self = Self::Value,
            Self::Value => *self = Self::Done,
            Self::Done => {}
        }
    }
}

/// Iterator representation over the keys *and* values of an attribute
/// set.
pub enum KeyValue<'a> {
    Empty,

    KV {
        name: &'a Value,
        value: &'a Value,
        at: IterKV,
    },

    Map(hash_map::Iter<'a, NixString, Value>),

    Sorted(std::vec::IntoIter<(&'a NixString, &'a Value)>),
}

/// Iterator over a Nix attribute set.
// This wrapper type exists to make the inner "raw" iterator
// inaccessible.
#[repr(transparent)]
pub struct Iter<T>(T);

impl<'a> Iterator for Iter<KeyValue<'a>> {
    type Item = (&'a NixString, &'a Value);

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            KeyValue::Map(inner) => inner.next(),
            KeyValue::Empty => None,
            KeyValue::KV { name, value, at } => match at {
                IterKV::Name => {
                    at.next();
                    Some((&NAME, name))
                }

                IterKV::Value => {
                    at.next();
                    Some((&VALUE, value))
                }

                IterKV::Done => None,
            },
            KeyValue::Sorted(inner) => inner.next(),
        }
    }
}

impl ExactSizeIterator for Iter<KeyValue<'_>> {
    fn len(&self) -> usize {
        match &self.0 {
            KeyValue::Empty => 0,
            KeyValue::KV { .. } => 2,
            KeyValue::Map(inner) => inner.len(),
            KeyValue::Sorted(inner) => inner.len(),
        }
    }
}

enum KeysInner<'a> {
    Empty,
    KV(IterKV),
    Map(hash_map::Keys<'a, NixString, Value>),
    Sorted(std::vec::IntoIter<&'a NixString>),
}

pub struct Keys<'a>(KeysInner<'a>);

impl<'a> Iterator for Keys<'a> {
    type Item = &'a NixString;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            KeysInner::Empty => None,
            KeysInner::KV(at @ IterKV::Name) => {
                at.next();
                Some(&NAME)
            }
            KeysInner::KV(at @ IterKV::Value) => {
                at.next();
                Some(&VALUE)
            }
            KeysInner::KV(IterKV::Done) => None,
            KeysInner::Map(m) => m.next(),
            KeysInner::Sorted(v) => v.next(),
        }
    }
}

impl<'a> IntoIterator for &'a NixAttrs {
    type Item = (&'a NixString, &'a Value);

    type IntoIter = Iter<KeyValue<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl ExactSizeIterator for Keys<'_> {
    fn len(&self) -> usize {
        match &self.0 {
            KeysInner::Empty => 0,
            KeysInner::KV(_) => 2,
            KeysInner::Map(m) => m.len(),
            KeysInner::Sorted(v) => v.len(),
        }
    }
}

/// Internal representation of an owning attrset iterator
pub enum IntoIterRepr {
    Empty,
    Finite(std::vec::IntoIter<(NixString, Value)>),
    Map(hash_map::IntoIter<NixString, Value>),
}

/// Wrapper type which hides the internal implementation details from
/// users.
#[repr(transparent)]
pub struct OwnedAttrsIterator(IntoIterRepr);

impl Iterator for OwnedAttrsIterator {
    type Item = (NixString, Value);

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            IntoIterRepr::Empty => None,
            IntoIterRepr::Finite(inner) => inner.next(),
            IntoIterRepr::Map(m) => m.next(),
        }
    }
}

impl ExactSizeIterator for OwnedAttrsIterator {
    fn len(&self) -> usize {
        match &self.0 {
            IntoIterRepr::Empty => 0,
            IntoIterRepr::Finite(inner) => inner.len(),
            IntoIterRepr::Map(inner) => inner.len(),
        }
    }
}

impl DoubleEndedIterator for OwnedAttrsIterator {
    fn next_back(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            IntoIterRepr::Empty => None,
            IntoIterRepr::Finite(inner) => inner.next_back(),
            // hashmaps have arbitary iteration order, so reversing it would not make a difference
            IntoIterRepr::Map(inner) => inner.next(),
        }
    }
}
