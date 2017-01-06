//! TODO FITZGEN

use ast;
use std::iter::FromIterator;
use std::ops::Deref;

/// An enumeration of all of the types that can end up in the substitution
/// table.
#[doc(hidden)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Substitutable {
    /// An `<unscoped-template-name>` production.
    UnscopedTemplateName(ast::UnscopedTemplateName),

    /// A `<template-prefix>` production.
    TemplatePrefix(ast::TemplatePrefix),

    /// A `<type>` production.
    Type(ast::Type),

    /// A `<template-template-param>` production.
    TemplateTemplateParam(ast::TemplateTemplateParam),

    /// An `<unresolved-type>` production.
    UnresolvedType(ast::UnresolvedType),

    /// A `<prefix>` production.
    Prefix(ast::Prefix),
}

/// The table of substitutable components that we have parsed thus far, and for
/// which there are potential back-references.
#[doc(hidden)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SubstitutionTable(Vec<Substitutable>);

impl SubstitutionTable {
    /// Construct a new `SubstitutionTable`.
    pub fn new() -> SubstitutionTable {
        SubstitutionTable(Vec::new())
    }

    /// Insert a freshly-parsed substitutable component into the table and
    /// return the index at which it now lives.
    pub fn insert(&mut self, entity: Substitutable) -> usize {
        let idx = self.0.len();
        log!("SubstitutionTable::insert @ {}: {:?}", idx, entity);
        self.0.push(entity);
        idx
    }

    /// Does this substitution table contain a component at the given index?
    pub fn contains(&self, idx: usize) -> bool {
        idx < self.0.len()
    }
}

impl FromIterator<Substitutable> for SubstitutionTable {
    fn from_iter<I: IntoIterator<Item = Substitutable>>(iter: I) -> Self {
        SubstitutionTable(Vec::from_iter(iter))
    }
}

impl Deref for SubstitutionTable {
    type Target = [Substitutable];

    fn deref(&self) -> &Self::Target {
        &self.0[..]
    }
}
