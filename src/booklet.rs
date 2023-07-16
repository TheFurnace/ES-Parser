use std::ops::Index;

use indexmap::IndexMap;

#[derive(Debug, PartialEq)]
pub enum SubValue {
    Single(String),
    List(Vec<SubValue>),
    Object(IndexMap<String, SubValue>),
}

impl SubValue {
    pub fn len(&self) -> usize {
        match self {
            SubValue::Single(s) => s.len(),
            SubValue::List(v) => v.len(),
            SubValue::Object(d) => d.len(),
        }
    }
}

impl FromIterator<(String, SubValue)> for SubValue {
    fn from_iter<T: IntoIterator<Item = (String, SubValue)>>(iter: T) -> Self {
        SubValue::Object(IndexMap::from_iter(iter))
    }
}

impl FromIterator<SubValue> for SubValue {
    fn from_iter<T: IntoIterator<Item = SubValue>>(iter: T) -> Self {
        SubValue::List(Vec::from_iter(iter))
    }
}

impl From<&str> for SubValue {
    fn from(s: &str) -> Self {
        SubValue::Single(s.into())
    }
}

impl Index<&str> for SubValue {
    type Output<'a> = Option<&'a SubValue>;

    fn index(&self, index: &str) -> &Self::Output {
        match self {
            SubValue::Single(x) => x[index],
            SubValue::List(_) => &None,
            SubValue::Object(d) => &d[index],
        }
    }
}

impl Index<usize> for SubValue {
    type Output = SubValue;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            SubValue::Single(_) => panic!("Cannot index by number from a single"),
            SubValue::List(v) => &v[index],
            SubValue::Object(_) => panic!("Cannot index by number from an object"),
        }
    }
}