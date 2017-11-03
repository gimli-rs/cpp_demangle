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
where
    W: 'subs + io::Write,
{
    fn demangle<'prev, 'ctx>(
        &'subs self,
        ctx: &'ctx mut ast::DemangleContext<'subs, W>,
        stack: Option<ast::ArgScopeStack<'prev, 'subs>>,
    ) -> io::Result<()> {
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
pub struct SubstitutionTable {
    substitutions: Vec<Substitutable>,
    // There are components which are typically candidates for substitution, but
    // in some particular circumstances are not. Instances of such components
    // which are not candidates for substitution end up in this part of the
    // table. See `<prefix>` parsing for further details.
    non_substitutions: Vec<Substitutable>,
}

impl fmt::Debug for SubstitutionTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad("SubstitutionTable ")?;
        f.debug_map()
            .entries(self.substitutions.iter().enumerate())
            .finish()
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
        let idx = self.substitutions.len();
        log!("SubstitutionTable::insert @ {}: {:?}", idx, entity);
        self.substitutions.push(entity);
        idx
    }

    /// Insert a an entity into the table that is not a candidate for
    /// substitution.
    pub fn insert_non_substitution(&mut self, entity: Substitutable) -> usize {
        let idx = self.non_substitutions.len();
        self.non_substitutions.push(entity);
        idx
    }

    /// Does this substitution table contain a component at the given index?
    pub fn contains(&self, idx: usize) -> bool {
        idx < self.substitutions.len()
    }

    /// Get the type referenced by the given handle, or None if there is no such
    /// entry, or there is an entry that is not a type.
    pub fn get_type(&self, handle: &ast::TypeHandle) -> Option<&ast::Type> {
        if let ast::TypeHandle::BackReference(idx) = *handle {
            self.substitutions.get(idx).and_then(|s| match *s {
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
        self.substitutions.pop()
    }

    /// Get the `idx`th entity that is not a candidate for substitution. Panics
    /// if `idx` is out of bounds.
    pub fn non_substitution(&self, idx: usize) -> &Substitutable {
        &self.non_substitutions[idx]
    }

    /// Get the `idx`th entity that is not a candidate for substitution. Returns
    /// `None` if `idx` is out of bounds.
    pub fn get_non_substitution(&self, idx: usize) -> Option<&Substitutable> {
        self.non_substitutions.get(idx)
    }
}

impl FromIterator<Substitutable> for SubstitutionTable {
    fn from_iter<I: IntoIterator<Item = Substitutable>>(iter: I) -> Self {
        SubstitutionTable {
            substitutions: Vec::from_iter(iter),
            non_substitutions: vec![],
        }
    }
}

impl Deref for SubstitutionTable {
    type Target = [Substitutable];

    fn deref(&self) -> &Self::Target {
        &self.substitutions[..]
    }
}
