//! Types dealing with the substitutions table.

use ast;
use error::Result;
use res::{self, Resolve};
use stable_deref_trait::StableDeref;
use std::fmt;
use std::io;
use std::iter::FromIterator;
use std::mem;
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

impl Resolve for Substitutable {
    fn resolve<'subs, 'prev, 'ctx, 'res>(&'subs self,
                                         ctx: &'ctx mut res::ResolveContext<'subs, 'res>,
                                         scope: Option<res::ArgScopeStack<'prev, 'subs>>)
                                         -> Result<()>
        where 'subs: 'prev + 'res,
              'res: 'ctx
    {
        match *self {
            Substitutable::UnscopedTemplateName(ref name) => name.resolve(ctx, scope),
            Substitutable::Type(ref ty) => ty.resolve(ctx, scope),
            Substitutable::TemplateTemplateParam(ref ttp) => ttp.resolve(ctx, scope),
            Substitutable::UnresolvedType(ref ty) => ty.resolve(ctx, scope),
            Substitutable::Prefix(ref prefix) => prefix.resolve(ctx, scope),
        }
    }
}

impl<'subs, W> ast::Demangle<'subs, W> for Substitutable
    where W: 'subs + io::Write
{
    fn demangle<'ctx>(&'subs self,
                      ctx: &'ctx mut ast::DemangleContext<'subs, W>)
                      -> io::Result<()> {
        match *self {
            Substitutable::UnscopedTemplateName(ref name) => name.demangle(ctx),
            Substitutable::Type(ref ty) => ty.demangle(ctx),
            Substitutable::TemplateTemplateParam(ref ttp) => ttp.demangle(ctx),
            Substitutable::UnresolvedType(ref ty) => ty.demangle(ctx),
            Substitutable::Prefix(ref prefix) => prefix.demangle(ctx),
        }
    }
}

/// A shared, immutable reference to a `SubstitutionTable`'s elements. A
/// `SubstitutionTableRef` is to a `SubstitutionTable` as a `str` is to a
/// `String`.
#[doc(hidden)]
#[derive(Debug, PartialEq, Eq)]
pub struct SubstitutionTableRef([Substitutable]);

impl Deref for SubstitutionTableRef {
    type Target = [Substitutable];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> From<&'a SubstitutionTable> for &'a SubstitutionTableRef {
    #[allow(unsafe_code)]
    fn from(table: &SubstitutionTable) -> &SubstitutionTableRef {
        let slice = &table.0[..];
        unsafe {
            mem::transmute(slice)
        }
    }
}

impl SubstitutionTableRef {
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

impl FromIterator<Substitutable> for SubstitutionTable {
    fn from_iter<I: IntoIterator<Item = Substitutable>>(iter: I) -> Self {
        SubstitutionTable(Vec::from_iter(iter))
    }
}

impl Deref for SubstitutionTable {
    type Target = SubstitutionTableRef;

    fn deref(&self) -> &Self::Target {
        self.into()
    }
}

// This is safe because `SubstitutionTable` is a newtype over
// `Vec<Substitutable>`, which is `StableDeref` because it heap allocates its
// underlying elements.
#[allow(unsafe_code)]
unsafe impl StableDeref for SubstitutionTable {}

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
}
