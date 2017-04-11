//! Infrastructure for scope resolution.
//!
//! After parsing the mangled C++ symbol into an AST, we need to resolve
//! abstract template parameters into the appropriate concrete template
//! arguments. This involves things like keeping track of which template
//! definition scope a given AST node falls within.
//!
//! Scope resolution is currently implemented as a full AST pass after
//! parsing. There are a couple potential performance improvements to be
//! had. First, we could completely avoid the pass if the AST doesn't contain
//! anything that needs scope resolution. Second, scope resolution could mostly
//! happen during parsing; there are only a few edge cases that need a whole
//! separate pass, like templatized operators. We could integrate scope
//! resolution into the parsing pass, and only fall back to a full pass when we
//! detect the more problematic edge cases.

use ast;
use error::{Error, Result};
use fixedbitset::FixedBitSet;
use std::collections::HashMap;
use std::fmt;
use subs;

/// The final result of scope resolution.
#[derive(Debug)]
pub struct Resolution<'a> {
    template_params: HashMap<&'a ast::TemplateParam, &'a ast::TemplateArg>,
}

impl<'a> Resolution<'a> {
    /// Construct a new `Resolution` by performing the scope resolution pass
    /// over the AST.
    pub fn new(subs: &'a subs::SubstitutionTableRef,
               ast: &'a ast::MangledName)
               -> Result<Resolution<'a>> {
        let mut resolution = Resolution {
            // TODO: we could count how many `TemplateParam`s we parsed and use
            // `with_capacity` here for micro optimizations. In fact, if the
            // count is zero for both template params and function params, we
            // could completely skip the `Resolve` pass.
            template_params: HashMap::new(),
        };

        {
            let mut ctx = ResolveContext::new(subs, &mut resolution);
            try!(ast.resolve(&mut ctx, None));
        }

        Ok(resolution)
    }

    /// Clear this resolution so that it is empty.
    ///
    /// See the `encapsulate_unsafety` module and `SymbolData`'s `Drop`
    /// implementation for details as to why this is needed.
    pub fn clear(&mut self) {
        self.template_params.clear();
    }

    /// Resolve the given generic template parameter to the given concrete
    /// template argument.
    pub fn resolve_template_param_to(&mut self,
                                     param: &'a ast::TemplateParam,
                                     arg: &'a ast::TemplateArg) {
        log!("Resolution::resolve_template_param_to: {:?} to {:?}", param, arg);
        let old = self.template_params.insert(param, arg);
        debug_assert!(old.is_none(),
                      "We should never be resolving the same `TemplateParam` twice");
    }

    /// Get the concrete template argument we resolved the given generic
    /// template parameter to.
    pub fn template_param_resolution(&self,
                                     param: &'a ast::TemplateParam)
                                     -> &'a ast::TemplateArg {
        self.template_params[&param]
    }
}

impl<'a> Drop for Resolution<'a> {
    fn drop(&mut self) {
        debug_assert!(self.template_params.len() == 0,
                      "Should have already been cleared by `SymbolData`'s \
                       `Drop` implementation!");
    }
}

/// Common context needed during the scope resolution phase.
pub struct ResolveContext<'subs, 'res>
    where 'subs: 'res
{
    substitutions: &'subs subs::SubstitutionTableRef,
    resolution: &'res mut Resolution<'subs>,

    // To avoid walking substitutions more than just the once, we set the
    // corresponding bit once we start walking a substitutable back reference
    // during the scope resolution phase.
    substitutions_visited: FixedBitSet,
}

impl<'subs, 'res> ResolveContext<'subs, 'res>
    where 'subs: 'res
{
    fn new(subs: &'subs subs::SubstitutionTableRef,
           resolution: &'res mut Resolution<'subs>)
           -> ResolveContext<'subs, 'res> {
        ResolveContext {
            substitutions: subs,
            resolution: resolution,
            substitutions_visited: FixedBitSet::with_capacity(subs.len()),
        }
    }

    /// Get a reference to the substitutions table.
    pub fn substitutions(&self) -> &'subs subs::SubstitutionTableRef {
        self.substitutions
    }

    /// Get a reference to the scope resolutions being constructed.
    pub fn resolution(&mut self) -> &mut Resolution<'subs> {
        self.resolution
    }

    /// Mark the substitutable at the given index as visited, so we don't walk
    /// it repeatedly if we see another back reference to it.
    pub fn set_visited(&mut self, idx: usize) {
        self.substitutions_visited.set(idx, true);
    }

    /// Have we already visited the substitutable at the given index?
    pub fn is_visited(&mut self, idx: usize) -> bool{
        self.substitutions_visited[idx]
    }
}

/// The trait whose implementations collectively form the scope resolution
/// phase. Implemented by AST nodes which need scope resolution, or transitively
/// contain other such AST nodes.
pub trait Resolve {
    /// Resolve the scope for this AST node.
    fn resolve<'subs, 'prev, 'ctx, 'res>(&'subs self,
                                         ctx: &'ctx mut ResolveContext<'subs, 'res>,
                                         scope: Option<ArgScopeStack<'prev, 'subs>>)
                                         -> Result<()>
        where 'subs: 'prev + 'res,
              'res: 'ctx;
}

/// When formatting a mangled symbol's parsed AST as a demangled symbol, we need
/// to resolve indirect references to template and function arguments with
/// direct `TemplateArg` and `Type` references respectively.
///
/// Note that which set of arguments are implicitly referenced change as we
/// enter and leave different functions' scope. One might usually use de Brujin
/// indices to keep arguments within scopes separated from each other, but the
/// Itanium C++ ABI does not allow us the luxury. AFAIK, when the ABI was first
/// drafted, C++ did not have lambdas, and the issue did not come up at all
/// since a function simply couldn't refer to the types of closed over
/// variables.
///
/// This trait is implemented by anything that can potentially resolve arguments
/// for us.
pub trait ArgScope<'me, 'ctx>: fmt::Debug {
    /// Get the current scope's `idx`th template argument.
    fn get_template_arg(&'me self, idx: usize) -> Result<&'ctx ast::TemplateArg>;

    /// Get the current scope's `idx`th function argument's type.
    fn get_function_arg(&'me self, idx: usize) -> Result<&'ctx ast::Type>;
}

/// An `ArgScopeStack` represents the current function and template demangling
/// scope we are within. As we enter new demangling scopes, we construct new
/// `ArgScopeStack`s whose `prev` references point back to the old ones. These
/// `ArgScopeStack`s are kept on the native stack, and as functions return, they
/// go out of scope and we use the previous `ArgScopeStack`s again.
#[derive(Copy, Clone, Debug)]
pub struct ArgScopeStack<'prev, 'subs>
    where 'subs: 'prev
{
    item: &'subs ArgScope<'subs, 'subs>,
    prev: Option<&'prev ArgScopeStack<'prev, 'subs>>,
}

/// When we first begin demangling, we haven't entered any function or template
/// demangling scope and we don't have any useful `ArgScopeStack`. Therefore, we
/// are never actually dealing with `ArgScopeStack` directly in practice, but
/// always an `Option<ArgScopeStack>` instead. Nevertheless, we want to define
/// useful methods on `Option<ArgScopeStack>`.
///
/// A custom "extension" trait with exactly one implementor: Rust's principled
/// monkey patching!
pub trait ArgScopeStackExt<'prev, 'subs> {
    /// Push a new `ArgScope` onto this `ArgScopeStack` and return the new
    /// `ArgScopeStack` with the pushed resolver on top.
    fn push(&'prev self, item: &'subs ArgScope<'subs, 'subs>) -> Option<ArgScopeStack<'prev, 'subs>>;
}

impl<'prev, 'subs> ArgScopeStackExt<'prev, 'subs> for Option<ArgScopeStack<'prev, 'subs>> {
    fn push(&'prev self, item: &'subs ArgScope<'subs, 'subs>) -> Option<ArgScopeStack<'prev, 'subs>> {
        Some(ArgScopeStack {
            prev: self.as_ref(),
            item: item,
        })
    }
}

/// A stack of `ArgScope`s is itself an `ArgScope`!
impl<'prev, 'subs> ArgScope<'prev, 'subs> for Option<ArgScopeStack<'prev, 'subs>> {
    fn get_template_arg(&'prev self, idx: usize) -> Result<&'subs ast::TemplateArg> {
        let mut stack = *self;
        while let Some(s) = stack {
            if let Ok(arg) = s.item.get_template_arg(idx) {
                return Ok(arg);
            }
            stack = s.prev.cloned();
        }

        Err(Error::BadTemplateArgReference)
    }

    fn get_function_arg(&'prev self, idx: usize) -> Result<&'subs ast::Type> {
        let mut stack = *self;
        while let Some(s) = stack {
            if let Ok(arg) = s.item.get_function_arg(idx) {
                return Ok(arg);
            }
            stack = s.prev.cloned();
        }

        Err(Error::BadFunctionArgReference)
    }
}
