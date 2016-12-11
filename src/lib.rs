//! TODO FITZGEN

#![allow(dead_code, unused_variables)]
#![deny(missing_docs)]
#![deny(missing_debug_implementations)]
// #![deny(warnings)]

// `error_chain!` can recurse deeply
#![recursion_limit = "1024"]

#[macro_use]
extern crate error_chain;

pub mod error;
mod index_str;

use error::Result;
use index_str::IndexStr;

/// TODO FITZGEN
pub type OwnedSymbol = Symbol<String>;

/// TODO FITZGEN
pub type BorrowedSymbol<'a> = Symbol<&'a str>;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Symbol<T> {
    raw: T,
    parsed: MangledName,
}

impl<T> Symbol<T>
    where T: AsRef<str>
{
    /// TODO FITZGEN
    pub fn new(raw: T) -> Result<Symbol<T>> {
        let input = IndexStr::new(raw.as_ref());
        let _ = input;
        unimplemented!()
    }
}

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MangledName(usize, Encoding);

impl MangledName {
    fn parse(input: IndexStr) -> Result<(MangledName, IndexStr)> {
        unimplemented!()
    }
}

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Encoding {
    /// TODO FITZGEN
    Function(Name, BareFunctionType),

    /// TODO FITZGEN
    Data(Name),

    /// TODO FITZGEN
    Special(SpecialName),
}

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Name {
    /// TODO FITZGEN
    Nested(NestedName),

    /// TODO FITZGEN
    Unscoped(UnscopedName),

    /// TODO FITZGEN
    UnscopedTemplate(UnscopedTemplateName, TemplateArgs),

    /// TODO FITZGEN
    Local(LocalName),
}

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnscopedName {
    /// TODO FITZGEN
    Unqualified(UnqualifiedName),

    /// TODO FITZGEN
    Std(UnqualifiedName),
}

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnscopedTemplateName {
    /// TODO FITZGEN
    Unscoped(UnscopedName),

    /// TODO FITZGEN
    Substitution(Substitution),
}

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum NestedName {
    /// TODO FITZGEN
    Unqualified(CvQualifiers, RefQualifier, Prefix, UnqualifiedName),

    /// TODO FITZGEN
    Template(CvQualifiers, RefQualifier, TemplatePrefix, TemplateArgs),
}

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Prefix {
    /// TODO FITZGEN
    Unqualified(UnqualifiedName, Option<PrefixTail>),
    /// TODO FITZGEN
    Template(TemplatePrefix, TemplateArgs, Option<PrefixTail>),
    /// TODO FITZGEN
    TemplateParam(TemplateParam, Option<PrefixTail>),
}

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Substitution(Option<SeqId>);

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BareFunctionType(Type);

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SeqId(usize);

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Type;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SpecialName;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplateArgs;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LocalName;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct UnqualifiedName;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CvQualifiers;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct RefQualifier;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplatePrefix;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PrefixTail;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplateParam;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
