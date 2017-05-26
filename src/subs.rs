//! Types dealing with the substitutions table.

use ast;
use std::fmt;
use std::io;
use std::iter::FromIterator;
use std::ops::Deref;

/// An enumeration of all of the types that can end up in the substitution
/// table.
#[doc(hidden)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Substitutable {
    /// An `<unscoped-template-name>` production.
    UnscopedTemplateName(ast::UnscopedTemplateName),

    /// A `<type>` production.
    Type(ast::Type),

    /// A `<template-template-param>` production.
    TemplateTemplateParam(ast::TemplateTemplateParam),

    /// An `<unresolved-type>` production.
    UnresolvedType(ast::UnresolvedType),

    /// A `<prefix>` production.
    Prefix(ast::Prefix),
}

impl<'subs, W> ast::Demangle<'subs, W> for Substitutable
    where W: 'subs + io::Write
{
    fn demangle<'prev, 'ctx>(&'subs self,
                             ctx: &'ctx mut ast::DemangleContext<'subs, W>,
                             stack: Option<ast::ArgScopeStack<'prev, 'subs>>)
                             -> io::Result<()> {
        match *self {
            Substitutable::UnscopedTemplateName(ref name) => name.demangle(ctx, stack),
            Substitutable::Type(ref ty) => ty.demangle(ctx, stack),
            Substitutable::TemplateTemplateParam(ref ttp) => ttp.demangle(ctx, stack),
            Substitutable::UnresolvedType(ref ty) => ty.demangle(ctx, stack),
            Substitutable::Prefix(ref prefix) => prefix.demangle(ctx, stack),
        }
    }
}

/// The table of substitutable components that we have parsed thus far, and for
/// which there are potential back-references.
#[doc(hidden)]
#[derive(Clone, Default, PartialEq, Eq)]
pub struct SubstitutionTable(Vec<Substitutable>);

impl fmt::Debug for SubstitutionTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(f.pad("SubstitutionTable "));
        f.debug_map().entries(self.0.iter().enumerate()).finish()
    }
}

impl SubstitutionTable {
    /// Construct a new `SubstitutionTable`.
    pub fn new() -> SubstitutionTable {
        Default::default()
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

    /// Get the type referenced by the given handle, or None if there is no such
    /// entry, or there is an entry that is not a type.
    pub fn get_type(&self, handle: &ast::TypeHandle) -> Option<&ast::Type> {
        if let ast::TypeHandle::BackReference(idx) = *handle {
            self.0.get(idx).and_then(|s| match *s {
                Substitutable::Type(ref ty) => Some(ty),
                _ => None,
            })
        } else {
            None
        }
    }

    /// Remove the last entry from the substitutions table and return it, or
    /// `None` if the table is empty.
    pub fn pop(&mut self) -> Option<Substitutable> {
        log!("SubstitutionTable::pop @ {}: {:?}", self.len(), self.last());
        self.0.pop()
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
