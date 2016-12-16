//! Abstract syntax tree types for mangled symbols.

use error::{ErrorKind, Result};
use index_str::IndexStr;
use std::fmt;

/// TODO FITZGEN: enum of all types that can be substituted.
#[doc(hidden)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Substitutable {
    /// An `<unscoped-template-name>` production.
    UnscopedTemplateName(UnscopedTemplateName),
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
        self.0.push(entity);
        idx
    }

    /// Does this substitution table contain a component at the given index?
    pub fn contains(&self, idx: usize) -> bool {
        idx < self.0.len()
    }
}

/// A trait for anything that can be parsed from an `IndexStr` and return a
/// `Result` of the parsed `Self` value and the rest of the `IndexStr` input
/// that has not been consumed in parsing the `Self` value.
///
/// For AST types representing productions which have `<substitution>` as a
/// possible right hand side, do not implement this trait directly. Instead,
/// make a newtype over `usize`, parse either the `<substitution>` back
/// reference or "real" value, insert the "real" value into the substitution
/// table if needed, and *always* return the newtype index into the substitution
/// table.
#[doc(hidden)]
pub trait Parse: Sized {
    /// Parse the `Self` value from `input` and return it, updating the
    /// substitution table as needed.
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(Self, IndexStr<'b>)>;
}

/// Define a "vocabulary" nonterminal, something like `OperatorName` or
/// `CtorDtorName` that's basically a big list of constant strings.
///
/// This declares:
///
/// - the enum itself
/// - a `Parse` impl
/// - a `std::fmt::Display` impl
///
/// See the definition of `CTorDtorName` for an example of its use.
macro_rules! define_vocabulary {
    ( $(#[$attr:meta])* pub enum $typename:ident {
        $($variant:ident ( $mangled:expr, $printable:expr )),*
    } ) => {

        $(#[$attr])*
        pub enum $typename {
            $(
                #[doc=$printable]
                $variant
            ),*
        }

        impl Parse for $typename {
            fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable, input: IndexStr<'b>) -> Result<($typename, IndexStr<'b>)> {
                let mut found_prefix = false;
                $(
                    if let Some((head, tail)) = input.try_split_at($mangled.len()) {
                        if head.as_ref() == $mangled {
                            return Ok(($typename::$variant, tail));
                        }
                    } else {
                        found_prefix |= 0 < input.len() &&
                            input.len() < $mangled.len() &&
                            input.as_ref() == &$mangled[..input.len()];
                    }
                )*

                if input.is_empty() || found_prefix {
                    Err(ErrorKind::UnexpectedEnd.into())
                } else {
                    Err(ErrorKind::UnexpectedText.into())
                }
            }
        }

        impl fmt::Display for $typename {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                f.write_str(match *self {
                    $(
                        $typename::$variant => $printable
                    ),*
                })
            }
        }
    }
}

/// The root AST node, and starting production.
///
/// ```text
/// <mangled-name> ::= _Z <encoding>
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct MangledName(Encoding);

impl Parse for MangledName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(MangledName, IndexStr<'b>)> {
        let tail = try!(consume(b"_Z", input));
        let (encoding, tail) = try!(Encoding::parse(subs, tail));
        Ok((MangledName(encoding), tail))
    }
}

/// The `<encoding>` production.
///
/// ```text
/// <encoding> ::= <function name> <bare-function-type>
///            ::= <data name>
///            ::= <special-name>
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum Encoding {
    /// An encoded function.
    Function(Name, BareFunctionType),

    /// An encoded static variable.
    Data(Name),

    /// A special encoding.
    Special(SpecialName),
}

impl Parse for Encoding {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(Encoding, IndexStr<'b>)> {
        if input.is_empty() {
            return Err(ErrorKind::UnexpectedEnd.into());
        }

        if let Ok((name, tail)) = Name::parse(subs, input) {
            if let Ok((ty, tail)) = BareFunctionType::parse(subs, tail) {
                return Ok((Encoding::Function(name, ty), tail));
            } else {
                return Ok((Encoding::Data(name), tail));
            }
        }

        if let Ok((name, tail)) = SpecialName::parse(subs, input) {
            return Ok((Encoding::Special(name), tail));
        }

        Err(ErrorKind::UnexpectedText.into())
    }
}

/// The `<name>` production.
///
/// ```text
/// <name> ::= <nested-name>
///        ::= <unscoped-name>
///        ::= <unscoped-template-name> <template-args>
///        ::= <local-name>
///        ::= St <unqualified-name> # ::std::
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum Name {
    /// A nested name
    Nested(NestedName),

    /// An unscoped name.
    Unscoped(UnscopedName),

    /// An unscoped template.
    UnscopedTemplate(UnscopedTemplateNameHandle, TemplateArgs),

    /// A local name.
    Local(LocalName),
}

impl Parse for Name {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(Name, IndexStr<'b>)> {
        if let Ok((name, tail)) = NestedName::parse(subs, input) {
            return Ok((Name::Nested(name), tail));
        }

        if let Ok((name, tail)) = UnscopedName::parse(subs, input) {
            return Ok((Name::Unscoped(name), tail));
        }

        if let Ok((name, tail)) = UnscopedTemplateNameHandle::parse(subs, input) {
            let (args, tail) = try!(TemplateArgs::parse(subs, tail));
            return Ok((Name::UnscopedTemplate(name, args), tail));
        }

        if let Ok((name, tail)) = LocalName::parse(subs, input) {
            return Ok((Name::Local(name), tail));
        }

        // TODO: the `std` variant
        Err(ErrorKind::UnexpectedText.into())
    }
}

/// The `<unscoped-name>` production.
///
/// ```text
/// <unscoped-name> ::= <unqualified-name>
///                 ::= St <unqualified-name>   # ::std::
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnscopedName {
    /// An unqualified name.
    Unqualified(UnqualifiedName),

    /// A name within the `std::` namespace.
    Std(UnqualifiedName),
}

impl Parse for UnscopedName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(UnscopedName, IndexStr<'b>)> {
        if let Ok(tail) = consume(b"St", input) {
            let (name, tail) = try!(UnqualifiedName::parse(subs, tail));
            return Ok((UnscopedName::Std(name), tail));
        }

        let (name, tail) = try!(UnqualifiedName::parse(subs, input));
        Ok((UnscopedName::Unqualified(name), tail))
    }
}

/// The `<unscoped-template-name>` production.
///
/// ```text
/// <unscoped-template-name> ::= <unscoped-name>
///                          ::= <substitution>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct UnscopedTemplateName(UnscopedName);

/// A handle to an `UnscopedTemplateName`.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnscopedTemplateNameHandle {
    /// A reference to a "well-known" component.
    WellKnown(WellKnownComponent),

    /// A reference to an `UnscopedTemplateName` that we have already parsed,
    /// and is at the given index in the substitution table.
    BackReference(usize),
}

impl Parse for UnscopedTemplateNameHandle {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(UnscopedTemplateNameHandle, IndexStr<'b>)> {
        if let Ok((name, tail)) = UnscopedName::parse(subs, input) {
            let name = UnscopedTemplateName(name);
            let idx = subs.insert(Substitutable::UnscopedTemplateName(name));
            let handle = UnscopedTemplateNameHandle::BackReference(idx);
            return Ok((handle, tail));
        }

        let (sub, tail) = try!(Substitution::parse(subs, input));

        match sub {
            Substitution::WellKnown(component) => {
                Ok((UnscopedTemplateNameHandle::WellKnown(component), tail))
            }
            Substitution::BackReference(idx) => {
                // TODO: should this check/assert that subs[idx] is an
                // UnscopedTemplateName?
                Ok((UnscopedTemplateNameHandle::BackReference(idx), tail))
            }
        }
    }
}

/// The `<nested-name>` production.
///
/// ```text
/// <nested-name> ::= N [<CV-qualifiers>] [<ref-qualifier>] <prefix> <unqualified-name> E
///               ::= N [<CV-qualifiers>] [<ref-qualifier>] <template-prefix> <template-args> E
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum NestedName {
    /// An unqualified name.
    Unqualified(CvQualifiers, Option<RefQualifier>, Prefix, UnqualifiedName),

    /// A template name.
    Template(CvQualifiers, Option<RefQualifier>, TemplatePrefix, TemplateArgs),
}

impl Parse for NestedName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(NestedName, IndexStr<'b>)> {
        let tail = try!(consume(b"N", input));

        let (cv_qualifiers, tail) = if let Ok((q, tail)) = CvQualifiers::parse(subs,
                                                                               tail) {
            (q, tail)
        } else {
            (Default::default(), tail)
        };

        let (ref_qualifier, tail) = if let Ok((r, tail)) = RefQualifier::parse(subs,
                                                                               tail) {
            (Some(r), tail)
        } else {
            (None, tail)
        };

        if let Ok((prefix, tail)) = Prefix::parse(subs, tail) {
            let (name, tail) = try!(UnqualifiedName::parse(subs, tail));
            let tail = try!(consume(b"E", tail));
            return Ok((NestedName::Unqualified(cv_qualifiers,
                                               ref_qualifier,
                                               prefix,
                                               name),
                       tail));
        }

        let (prefix, tail) = try!(TemplatePrefix::parse(subs, tail));
        let (args, tail) = try!(TemplateArgs::parse(subs, tail));
        let tail = try!(consume(b"E", tail));
        Ok((NestedName::Template(cv_qualifiers, ref_qualifier, prefix, args), tail))
    }
}

/// The `<prefix>` production.
///
/// Note that there is left-recursion directly within `<prefix>` itself, as well
/// as indirectly via `<template-prefix>`. Our parser is a naive recursive
/// descent parser, and this left-recursion will cause us to go into an infinite
/// loop and blow the stack. So, we've refactored the grammar a little to remove
/// the left-recursion. First, we inlined `<template-prefix>` into `<prefix>` to
/// make all of the left-recursion direct rather than indirect. Second, we split
/// `<prefix>` into `<prefix>` and `<prefix-rest>` using the usual algorithm for
/// removing direct left-recursion.
///
/// Here are the original `<prefix>` and `<template-prefix>` productions.
///
/// ```text
/// <prefix> ::= <unqualified-name>
///          ::= <prefix> <unqualified-name>
///          ::= <template-prefix> <template-args>
///          ::= <template-param>
///          ::= <decltype>
///          ::= <prefix> <data-member-prefix>
///          ::= <substitution>
///
/// <template-prefix> ::= <template unqualified-name>
///                   ::= <prefix> <template unqualified-name>
///                   ::= <template-param>
///                   ::= <substitution>
/// ```
///
/// Here is the `<prefix>` production after we inline `<template-prefix>` into
/// it.
///
/// ```text
/// <prefix> ::= <unqualified-name>
///          ::= <prefix> <unqualified-name>
///          # ... inlining begins ...
///          ::= <unqualified-name> <template-args>
///          ::= <prefix> <unqualified-name> <template-args>
///          ::= <template-param> <template-args>
///          ::= <substitution> <template-args>
///          # ... inlining ends ...
///          ::= <template-param>
///          ::= <decltype>
///          ::= <prefix> <data-member-prefix>
///          ::= <substitution>
/// ```
///
/// And here are the final `<prefix>` and `<prefix-rest>` productions once the
/// left-recursion has been eliminated.
///
/// ```text
/// <prefix> ::= <unqualified-name>                 <prefix-rest>
///          ::= <unqualified-name> <template-args> <prefix-rest>
///          ::= <template-param>   <template-args> <prefix-rest>
///          ::= <substitution>     <template-args> <prefix-rest>
///          ::= <template-param>                   <prefix-rest>
///          ::= <decltype>                         <prefix-rest>
///          ::= <substitution>                     <prefix-rest>
///
/// <prefix-rest> ::= <unqualified-name>                 <prefix-rest>
///               ::= <unqualified-name> <template-args> <prefix-rest>
///               ::= <data-member-prefix>               <prefix-rest>
///               ::= nil
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum Prefix {
    /// An unqualified name and optionally more prefix.
    Unqualified(UnqualifiedName, Option<Box<PrefixRest>>),

    /// A template and optionally more prefix.
    Template(UnqualifiedName, TemplateArgs, Option<Box<PrefixRest>>),

    /// A template parameter and optionally more prefix.
    TemplateParam(TemplateParam, Option<TemplateArgs>, Option<Box<PrefixRest>>),

    /// A `decltype` and optionally more prefix.
    Decltype(Decltype, Option<Box<PrefixRest>>), // TODO: substitution variant
}

impl Parse for Prefix {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     _input: IndexStr<'b>)
                     -> Result<(Prefix, IndexStr<'b>)> {
        Err("Not yet implemented".into())
    }
}

/// The second half of the <prefix> production with left-recursion factored out.
#[derive(Clone, Debug, PartialEq)]
pub enum PrefixRest {
    /// An unqualified name and optionally more prefix.
    Unqualified(UnqualifiedName, Option<Box<PrefixRest>>),

    /// An a template, its arguments, and optionally more prefix.
    Template(UnqualifiedName, TemplateArgs, Option<Box<PrefixRest>>),

    /// A data member and optionally more prefix.
    DataMember(DataMemberPrefix, Option<Box<PrefixRest>>),
}

impl Parse for PrefixRest {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(PrefixRest, IndexStr<'b>)> {
        fn parse_prefix_tail<'a, 'b>(subs: &'a mut SubstitutionTable,
                                     input: IndexStr<'b>)
                                     -> (Option<Box<PrefixRest>>, IndexStr<'b>) {
            if let Ok((prefix_tail, tail)) = PrefixRest::parse(subs, input) {
                (Some(Box::new(prefix_tail)), tail)
            } else {
                (None, input)
            }
        }

        if let Ok((name, tail)) = UnqualifiedName::parse(subs, input) {
            let (prefix_tail, tail) = parse_prefix_tail(subs, tail);
            return Ok((PrefixRest::Unqualified(name, prefix_tail), tail));
        }

        let (data, tail) = try!(DataMemberPrefix::parse(subs, input));
        let (prefix_tail, tail) = parse_prefix_tail(subs, tail);
        Ok((PrefixRest::DataMember(data, prefix_tail), tail))
    }
}

/// The `<template-prefix>` production.
///
/// ```text
/// <template-prefix> ::= <template unqualified-name>            # global template
///                   ::= <prefix> <template unqualified-name>   # nested template
///                   ::= <template-param>                       # template template parameter
///                   ::= <substitution>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplatePrefix;

impl Parse for TemplatePrefix {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     _input: IndexStr<'b>)
                     -> Result<(TemplatePrefix, IndexStr<'b>)> {
        Err("Not yet implemented".into())
    }
}

/// The `<unqualified-name>` production.
///
/// ```text
/// <unqualified-name> ::= <operator-name>
///                    ::= <ctor-dtor-name>
///                    ::= <source-name>
///                    ::= <unnamed-type-name>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnqualifiedName {
    /// An operator name.
    Operator(OperatorName),
    /// A constructor or destructor name.
    CtorDtor(CtorDtorName),
    /// A source name.
    Source(SourceName),
    /// A generated name for an unnamed type.
    UnnamedType(UnnamedTypeName),
}

impl Parse for UnqualifiedName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(UnqualifiedName, IndexStr<'b>)> {
        if let Ok((op, tail)) = OperatorName::parse(subs, input) {
            return Ok((UnqualifiedName::Operator(op), tail));
        }

        if let Ok((ctor_dtor, tail)) = CtorDtorName::parse(subs, input) {
            return Ok((UnqualifiedName::CtorDtor(ctor_dtor), tail));
        }

        if let Ok((source, tail)) = SourceName::parse(subs, input) {
            return Ok((UnqualifiedName::Source(source), tail));
        }

        UnnamedTypeName::parse(subs, input)
            .map(|(unnamed, tail)| (UnqualifiedName::UnnamedType(unnamed), tail))
    }
}

/// The `<source-name>` non-terminal.
///
/// ```text
/// <source-name> ::= <positive length number> <identifier>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SourceName(Identifier);

impl Parse for SourceName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(SourceName, IndexStr<'b>)> {
        let (source_name_len, input) = try!(parse_number(10, false, input));
        debug_assert!(source_name_len >= 0);
        if source_name_len == 0 {
            return Err(ErrorKind::UnexpectedText.into());
        }

        let (head, tail) = match input.try_split_at(source_name_len as _) {
            Some((head, tail)) => (head, tail),
            None => return Err(ErrorKind::UnexpectedEnd.into()),
        };

        let (identifier, empty) = try!(Identifier::parse(subs, head));
        if !empty.is_empty() {
            return Err(ErrorKind::UnexpectedText.into());
        }

        let source_name = SourceName(identifier);
        Ok((source_name, tail))
    }
}

/// The `<identifier>` pseudo-terminal.
///
/// ```text
/// <identifier> ::= <unqualified source code identifier>
/// ```
///
/// > `<identifier>` is a pseudo-terminal representing the characters in the
/// > unqualified identifier for the entity in the source code. This ABI does not
/// > yet specify a mangling for identifiers containing characters outside of
/// > `_A-Za-z0-9`.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Identifier {
    start: usize,
    end: usize,
}

impl Parse for Identifier {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(Identifier, IndexStr<'b>)> {
        if input.is_empty() {
            return Err(ErrorKind::UnexpectedEnd.into());
        }

        let end = input.as_ref()
            .iter()
            .map(|&c| c as char)
            .take_while(|&c| c == '_' || c.is_digit(36))
            .count();

        if end == 0 {
            return Err(ErrorKind::UnexpectedText.into());
        }

        let tail = input.range_from(end..);

        let identifier = Identifier {
            start: input.index(),
            end: tail.index(),
        };

        Ok((identifier, tail))
    }
}

/// The `<number>` production.
///
/// ```text
/// <number> ::= [n] <non-negative decimal integer>
/// ```
type Number = isize;

impl Parse for Number {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(isize, IndexStr<'b>)> {
        parse_number(10, true, input)
    }
}

/// A <seq-id> production encoding a base-36 positive number.
///
/// ```text
/// <seq-id> ::= <0-9A-Z>+
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SeqId(usize);

impl Parse for SeqId {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(SeqId, IndexStr<'b>)> {
        parse_number(36, false, input).map(|(num, tail)| (SeqId(num as _), tail))
    }
}

// TODO: support the rest of <operator-name>:
//
// ::= cv <type>               # (cast)
// ::= li <source-name>        # operator ""
// ::= v <digit> <source-name> # vendor extended operator
define_vocabulary! {
    /// The `<operator-name>` production.
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub enum OperatorName {
        // enum variant(mangled form, printable description)
        New              (b"nw",  "`new`"),
        NewArray         (b"na",  "`new[]`"),
        Delete           (b"dl",  "`delete`"),
        DeleteArray      (b"da",  "`delete[]`"),
        UnaryPlus        (b"ps",  "`+` (unary)"),
        Neg              (b"ng",  "`-` (unary)"),
        AddressOf        (b"ad",  "`&` (unary)"),
        Deref            (b"de",  "`*` (unary)"),
        BitNot           (b"co",  "`~`"),
        Add              (b"pl",  "`+`"),
        Sub              (b"mi",  "`-`"),
        Mul              (b"ml",  "`*`"),
        Div              (b"dv",  "`/`"),
        Rem              (b"rm",  "`%`"),
        BitAnd           (b"an",  "`&`"),
        BitOr            (b"or",  "`|`"),
        BitXor           (b"eo",  "`^`"),
        Assign           (b"aS",  "`=`"),
        AddAssign        (b"pL",  "`+=`"),
        SubAssign        (b"mI",  "`-=`"),
        MulAssign        (b"mL",  "`*=`"),
        DivAssign        (b"dV",  "`/=`"),
        RemAssign        (b"rM",  "`%=`"),
        BitAndAssign     (b"aN",  "`&=`"),
        BitOrAssign      (b"oR",  "`|=`"),
        BitXorAssign     (b"eO",  "`^=`"),
        Shl              (b"ls",  "`<<`"),
        Shr              (b"rs",  "`>>`"),
        ShlAssign        (b"lS",  "`<<=`"),
        ShrAssign        (b"rS",  "`>>=`"),
        Eq               (b"eq",  "`==`"),
        Ne               (b"ne",  "`!=`"),
        Less             (b"lt",  "`<`"),
        Greater          (b"gt",  "`>`"),
        LessEq           (b"le",  "`<=`"),
        GreaterEq        (b"ge",  "`>=`"),
        Not              (b"nt",  "`!`"),
        LogicalAnd       (b"aa",  "`&&`"),
        LogicalOr        (b"oo",  "`||`"),
        PostInc          (b"pp",  "`++` (postfix in <expression> context)"),
        PostDec          (b"mm",  "`--` (postfix in <expression> context)"),
        Comma            (b"cm",  "`,`"),
        DerefMemberPtr   (b"pm",  "`->*`"),
        DerefMember      (b"pt",  "`->`"),
        Call             (b"cl",  "`()`"),
        Index            (b"ix",  "`[]`"),
        Question         (b"qu",  "`?:`")
    }
}

/// The `<call-offset>` production.
///
/// ```text
/// <call-offset> ::= h <nv-offset> _
///               ::= v <v-offset> _
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CallOffset {
    /// A non-virtual offset.
    NonVirtual(NvOffset),
    /// A virtual offset.
    Virtual(VOffset),
}

impl Parse for CallOffset {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(CallOffset, IndexStr<'b>)> {
        if input.is_empty() {
            return Err(ErrorKind::UnexpectedEnd.into());
        }

        if let Ok(tail) = consume(b"h", input) {
            let (offset, tail) = try!(NvOffset::parse(subs, tail));
            let tail = try!(consume(b"_", tail));
            return Ok((CallOffset::NonVirtual(offset), tail));
        }

        if let Ok(tail) = consume(b"v", input) {
            let (offset, tail) = try!(VOffset::parse(subs, tail));
            let tail = try!(consume(b"_", tail));
            return Ok((CallOffset::Virtual(offset), tail));
        }

        Err(ErrorKind::UnexpectedText.into())
    }
}

/// A non-virtual offset, as described by the <nv-offset> production.
///
/// ```text
/// <nv-offset> ::= <offset number>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NvOffset(isize);

impl Parse for NvOffset {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(NvOffset, IndexStr<'b>)> {
        Number::parse(subs, input).map(|(num, tail)| (NvOffset(num), tail))
    }
}

/// A virtual offset, as described by the <v-offset> production.
///
/// ```text
/// <v-offset> ::= <offset number> _ <virtual offset number>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct VOffset(isize, isize);

impl Parse for VOffset {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(VOffset, IndexStr<'b>)> {
        let (offset, tail) = try!(Number::parse(subs, input));
        let tail = try!(consume(b"_", tail));
        let (virtual_offset, tail) = try!(Number::parse(subs, tail));
        Ok((VOffset(offset, virtual_offset), tail))
    }
}

define_vocabulary! {
    /// The `<ctor-dtor-name>` production.
    ///
    /// ```text
    /// <ctor-dtor-name> ::= C1  # complete object constructor
    ///                  ::= C2  # base object constructor
    ///                  ::= C3  # complete object allocating constructor
    ///                  ::= D0  # deleting destructor
    ///                  ::= D1  # complete object destructor
    ///                  ::= D2  # base object destructor
    /// ```
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub enum CtorDtorName {
        CompleteConstructor             (b"C1", "complete object constructor"),
        BaseConstructor                 (b"C2", "base object constructor"),
        CompleteAllocatingConstructor   (b"C3", "complete object allocating constructor"),
        DeletingDestructor              (b"D0", "deleting destructor"),
        CompleteDestructor              (b"D1", "complete object destructor"),
        BaseDestructor                  (b"D2", "base object destructor")
    }
}

/// The `<type>` production.
///
/// ```text
///
/// <type> ::= <builtin-type>
///        ::= <function-type>
///        ::= <class-enum-type>
///        ::= <array-type>
///        ::= <pointer-to-member-type>
///        ::= <template-param>
///        ::= <template-template-param> <template-args>
///        ::= <decltype>
///        ::= <CV-qualifiers> <type>
///        ::= P <type>                                 # pointer-to
///        ::= R <type>                                 # reference-to
///        ::= O <type>                                 # rvalue reference-to (C++0x)
///        ::= C <type>                                 # complex pair (C 2000)
///        ::= G <type>                                 # imaginary (C 2000)
///        ::= U <source-name> [<template-args>] <type> # vendor extended type qualifier
///        ::= Dp <type>                                # pack expansion (C++0x)
///        ::= <substitution>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Type;

impl Parse for Type {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     _input: IndexStr<'b>)
                     -> Result<(Type, IndexStr<'b>)> {
        Err("Not yet implemented".into())
    }
}

/// The `<CV-qualifiers>` production.
///
/// ```text
/// <CV-qualifiers> ::= [r] [V] [K]   # restrict (C99), volatile, const
/// ```
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct CvQualifiers {
    /// Is this `restrict` qualified?
    pub restrict: bool,
    /// Is this `volatile` qualified?
    pub volatile: bool,
    /// Is this `const` qualified?
    pub const_: bool,
}

impl Parse for CvQualifiers {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(CvQualifiers, IndexStr<'b>)> {
        let (restrict, tail) = if let Ok(tail) = consume(b"r", input) {
            (true, tail)
        } else {
            (false, input)
        };

        let (volatile, tail) = if let Ok(tail) = consume(b"V", tail) {
            (true, tail)
        } else {
            (false, tail)
        };

        let (const_, tail) = if let Ok(tail) = consume(b"K", tail) {
            (true, tail)
        } else {
            (false, tail)
        };

        let qualifiers = CvQualifiers {
            restrict: restrict,
            volatile: volatile,
            const_: const_,
        };

        Ok((qualifiers, tail))
    }
}

define_vocabulary! {
    /// A <ref-qualifier> production.
    ///
    /// ```text
    /// <ref-qualifier> ::= R   # & ref-qualifier
    ///                 ::= O   # && ref-qualifier
    /// ```
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub enum RefQualifier {
        LValueRef(b"R", "& ref-qualifier"),
        RValueRef(b"O", "&& ref-qualifier")
    }
}

define_vocabulary! {
    /// A one of the standard variants of the <builtin-type> production.
    ///
    /// ```text
    /// <builtin-type> ::= v  # void
    ///                ::= w  # wchar_t
    ///                ::= b  # bool
    ///                ::= c  # char
    ///                ::= a  # signed char
    ///                ::= h  # unsigned char
    ///                ::= s  # short
    ///                ::= t  # unsigned short
    ///                ::= i  # int
    ///                ::= j  # unsigned int
    ///                ::= l  # long
    ///                ::= m  # unsigned long
    ///                ::= x  # long long, __int64
    ///                ::= y  # unsigned long long, __int64
    ///                ::= n  # __int128
    ///                ::= o  # unsigned __int128
    ///                ::= f  # float
    ///                ::= d  # double
    ///                ::= e  # long double, __float80
    ///                ::= g  # __float128
    ///                ::= z  # ellipsis
    ///                ::= Dd # IEEE 754r decimal floating point (64 bits)
    ///                ::= De # IEEE 754r decimal floating point (128 bits)
    ///                ::= Df # IEEE 754r decimal floating point (32 bits)
    ///                ::= Dh # IEEE 754r half-precision floating point (16 bits)
    ///                ::= Di # char32_t
    ///                ::= Ds # char16_t
    ///                ::= Da # auto
    ///                ::= Dc # decltype(auto)
    ///                ::= Dn # std::nullptr_t (i.e., decltype(nullptr))
    /// ```
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub enum StandardBuiltinType {
        Void             (b"v",  "void"),
        Wchar            (b"w",  "wchar_t"),
        Bool             (b"b",  "bool"),
        Char             (b"c",  "char"),
        SignedChar       (b"a",  "signed char"),
        UnsignedChar     (b"h",  "unsigned char"),
        Short            (b"s",  "short"),
        UnsignedShort    (b"t",  "unsigned short"),
        Int              (b"i",  "int"),
        UnsignedInt      (b"j",  "unsigned int"),
        Long             (b"l",  "long"),
        UnsignedLong     (b"m",  "unsigned long"),
        LongLong         (b"x",  "long long, __int64"),
        UnsignedLongLong (b"y",  "unsigned long long, __int64"),
        Int128           (b"n",  "__int128"),
        Uint128          (b"o",  "unsigned __int128"),
        Float            (b"f",  "float"),
        Double           (b"d",  "double"),
        LongDouble       (b"e",  "long double, __float80"),
        Float128         (b"g",  "__float128"),
        Ellipsis         (b"z",  "ellipsis"),
        DecimalFloat64   (b"Dd", "IEEE 754r decimal floating point (64 bits)"),
        DecimalFloat128  (b"De", "IEEE 754r decimal floating point (128 bits)"),
        DecimalFloat32   (b"Df", "IEEE 754r decimal floating point (32 bits)"),
        DecimalFloat16   (b"Dh", "IEEE 754r half-precision floating point (16 bits)"),
        Char32           (b"Di", "char32_t"),
        Char16           (b"Ds", "char16_t"),
        Auto             (b"Da", "auto"),
        Decltype         (b"Dc", "decltype(auto)"),
        Nullptr          (b"Dn", "std::nullptr_t (i.e., decltype(nullptr))")
    }
}

/// The `<builtin-type>` production.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum BuiltinType {
    /// A standards compliant builtin type.
    Standard(StandardBuiltinType),

    /// A non-standard, vendor extension type.
    ///
    /// ```text
    /// <builtin-type> ::= u <source-name>   # vendor extended type
    /// ```
    Extension(SourceName),
}

impl Parse for BuiltinType {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(BuiltinType, IndexStr<'b>)> {
        if let Ok((ty, tail)) = StandardBuiltinType::parse(subs, input) {
            return Ok((BuiltinType::Standard(ty), tail));
        }

        let tail = try!(consume(b"u", input));
        let (name, tail) = try!(SourceName::parse(subs, tail));
        Ok((BuiltinType::Extension(name), tail))
    }
}

/// The `<function-type>` production.
///
/// ```text
/// <function-type> ::= [<CV-qualifiers>] [Dx] F [Y] <bare-function-type> [<ref-qualifier>] E
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionType {
    cv_qualifiers: CvQualifiers,
    transaction_safe: bool,
    extern_c: bool,
    bare: BareFunctionType,
    ref_qualifier: Option<RefQualifier>,
}

impl Parse for FunctionType {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(FunctionType, IndexStr<'b>)> {
        let (cv_qualifiers, tail) = if let Ok((cv_qualifiers, tail)) =
                                           CvQualifiers::parse(subs, input) {
            (cv_qualifiers, tail)
        } else {
            (Default::default(), input)
        };

        let (transaction_safe, tail) = if let Ok(tail) = consume(b"Dx", tail) {
            (true, tail)
        } else {
            (false, tail)
        };

        let tail = try!(consume(b"F", tail));

        let (extern_c, tail) = if let Ok(tail) = consume(b"Y", tail) {
            (true, tail)
        } else {
            (false, tail)
        };

        let (bare, tail) = try!(BareFunctionType::parse(subs, tail));

        let (ref_qualifier, tail) = if let Ok((ref_qualifier, tail)) =
                                           RefQualifier::parse(subs, tail) {
            (Some(ref_qualifier), tail)
        } else {
            (None, tail)
        };

        let tail = try!(consume(b"E", tail));

        let func_ty = FunctionType {
            cv_qualifiers: cv_qualifiers,
            transaction_safe: transaction_safe,
            extern_c: extern_c,
            bare: bare,
            ref_qualifier: ref_qualifier,
        };
        Ok((func_ty, tail))
    }
}

/// The `<bare-function-type>` production.
///
/// ```text
/// <bare-function-type> ::= <signature type>+
///      # types are possible return type, then parameter types
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BareFunctionType(Vec<Type>);

impl Parse for BareFunctionType {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(BareFunctionType, IndexStr<'b>)> {
        let (ty, mut tail) = try!(Type::parse(subs, input));
        let mut types = vec![ty];

        loop {
            if let Ok((ty, tail_tail)) = Type::parse(subs, tail) {
                types.push(ty);
                tail = tail_tail;
            } else {
                return Ok((BareFunctionType(types), tail));
            }
        }
    }
}

/// The `<decltype>` production.
///
/// ```text
/// <decltype> ::= Dt <expression> E
///            ::= DT <expression> E
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Decltype {
    /// A `decltype` of an id-expression or class member access (C++0x).
    IdExpression(Expression),

    /// A `decltype` of an expression (C++0x).
    Expression(Expression),
}

impl Parse for Decltype {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(Decltype, IndexStr<'b>)> {
        let tail = try!(consume(b"D", input));

        if let Ok(tail) = consume(b"t", tail) {
            let (expr, tail) = try!(Expression::parse(subs, tail));
            let tail = try!(consume(b"E", tail));
            return Ok((Decltype::IdExpression(expr), tail));
        }

        let tail = try!(consume(b"T", tail));
        let (expr, tail) = try!(Expression::parse(subs, tail));
        let tail = try!(consume(b"E", tail));
        Ok((Decltype::Expression(expr), tail))
    }
}

/// The `<class-enum-type>` production.
///
/// ```text
/// <class-enum-type> ::= <name>
///                   ::= Ts <name>
///                   ::= Tu <name>
///                   ::= Te <name>
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum ClassEnumType {
    /// A non-dependent type name, dependent type name, or dependent
    /// typename-specifier.
    Named(Name),

    /// A dependent elaborated type specifier using 'struct' or 'class'.
    ElaboratedStruct(Name),

    /// A dependent elaborated type specifier using 'union'.
    ElaboratedUnion(Name),

    /// A dependent elaborated type specifier using 'enum'.
    ElaboratedEnum(Name),
}

impl Parse for ClassEnumType {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(ClassEnumType, IndexStr<'b>)> {
        if let Ok((name, tail)) = Name::parse(subs, input) {
            return Ok((ClassEnumType::Named(name), tail));
        }

        let tail = try!(consume(b"T", input));

        if let Ok(tail) = consume(b"s", tail) {
            let (name, tail) = try!(Name::parse(subs, tail));
            return Ok((ClassEnumType::ElaboratedStruct(name), tail));
        }

        if let Ok(tail) = consume(b"u", tail) {
            let (name, tail) = try!(Name::parse(subs, tail));
            return Ok((ClassEnumType::ElaboratedEnum(name), tail));
        }

        let tail = try!(consume(b"e", tail));
        let (name, tail) = try!(Name::parse(subs, tail));
        Ok((ClassEnumType::ElaboratedEnum(name), tail))
    }
}

/// The `<unnamed-type-name>` production.
///
/// ```text
/// <unnamed-type-name> ::= Ut [ <nonnegative number> ] _
///                     ::= <closure-type-name>
/// ```
///
/// TODO: parse the <closure-type-name> variant
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct UnnamedTypeName(Option<usize>);

impl Parse for UnnamedTypeName {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(UnnamedTypeName, IndexStr<'b>)> {
        let input = try!(consume(b"Ut", input));
        let (number, input) = match parse_number(10, false, input) {
            Ok((number, input)) => (Some(number as _), input),
            Err(_) => (None, input),
        };
        let input = try!(consume(b"_", input));
        Ok((UnnamedTypeName(number), input))
    }
}

/// The `<array-type>` production.
///
/// ```text
/// <array-type> ::= A <positive dimension number> _ <element type>
///              ::= A [<dimension expression>] _ <element type>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ArrayType {
    /// An array with a number-literal dimension.
    DimensionNumber(usize, Type),

    /// An array with an expression for its dimension.
    DimensionExpression(Expression, Type),
}

impl Parse for ArrayType {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(ArrayType, IndexStr<'b>)> {
        let tail = try!(consume(b"A", input));

        if let Ok((num, tail)) = parse_number(10, false, tail) {
            debug_assert!(num >= 0);
            let tail = try!(consume(b"_", tail));
            let (ty, tail) = try!(Type::parse(subs, tail));
            return Ok((ArrayType::DimensionNumber(num as _, ty), tail));
        }

        let (expr, tail) = try!(Expression::parse(subs, tail));
        let tail = try!(consume(b"_", tail));
        let (ty, tail) = try!(Type::parse(subs, tail));
        Ok((ArrayType::DimensionExpression(expr, ty), tail))
    }
}

/// The `<pointer-to-member-type>` production.
///
/// ```text
/// <pointer-to-member-type> ::= M <class type> <member type>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PointerToMemberType(Type, Type);

impl Parse for PointerToMemberType {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(PointerToMemberType, IndexStr<'b>)> {
        let tail = try!(consume(b"M", input));
        let (ty1, tail) = try!(Type::parse(subs, tail));
        let (ty2, tail) = try!(Type::parse(subs, tail));
        Ok((PointerToMemberType(ty1, ty2), tail))
    }
}

/// The `<template-param>` production.
///
/// ```text
/// <template-param> ::= T_ # first template parameter
///                  ::= T <parameter-2 non-negative number> _
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplateParam(Option<usize>);

impl Parse for TemplateParam {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(TemplateParam, IndexStr<'b>)> {
        let input = try!(consume(b"T", input));
        let (number, input) = match parse_number(10, false, input) {
            Ok((number, input)) => (Some(number as _), input),
            Err(_) => (None, input),
        };
        let input = try!(consume(b"_", input));
        Ok((TemplateParam(number), input))
    }
}

/// The `<template-template-param>` production.
///
/// ```text
/// <template-template-param> ::= <template-param>
///                           ::= <substitution>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplateTemplateParam;

impl Parse for TemplateTemplateParam {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     _input: IndexStr<'b>)
                     -> Result<(TemplateTemplateParam, IndexStr<'b>)> {
        Err("Not yet implemented".into())
    }
}

/// The <function-param> production.
///
/// ```text
/// <function-param> ::= fp <top-level CV-qualifiers> _
///                          # L == 0, first parameter
///                  ::= fp <top-level CV-qualifiers> <parameter-2 non-negative number> _
///                          # L == 0, second and later parameters
///                  ::= fL <L-1 non-negative number> p <top-level CV-qualifiers> _
///                          # L > 0, first parameter
///                  ::= fL <L-1 non-negative number> p <top-level CV-qualifiers> <parameter-2 non-negative number> _
///                          # L > 0, second and later parameters
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct FunctionParam(usize, CvQualifiers, Option<usize>);

impl Parse for FunctionParam {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(FunctionParam, IndexStr<'b>)> {
        let tail = try!(consume(b"f", input));
        if tail.is_empty() {
            return Err(ErrorKind::UnexpectedEnd.into());
        }

        let (scope, tail) = if let Ok(tail) = consume(b"L", tail) {
            try!(parse_number(10, false, tail))
        } else {
            (0, tail)
        };

        let tail = try!(consume(b"p", tail));

        let (qualifiers, tail) = try!(CvQualifiers::parse(subs, tail));

        let (param, tail) = if let Ok((num, tail)) = parse_number(10, false, tail) {
            (Some(num as _), tail)
        } else {
            (None, tail)
        };

        let tail = try!(consume(b"_", tail));
        Ok((FunctionParam(scope as _, qualifiers, param), tail))
    }
}

/// The `<template-args>` production.
///
/// ```text
/// <template-args> ::= I <template-arg>+ E
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct TemplateArgs(Vec<TemplateArg>);

impl Parse for TemplateArgs {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(TemplateArgs, IndexStr<'b>)> {
        let tail = try!(consume(b"I", input));

        let (arg, mut tail) = try!(TemplateArg::parse(subs, tail));
        let mut args = vec![arg];

        loop {
            if let Ok((arg, tail_tail)) = TemplateArg::parse(subs, tail) {
                args.push(arg);
                tail = tail_tail;
            } else {
                let tail = try!(consume(b"E", tail));
                return Ok((TemplateArgs(args), tail));
            }
        }
    }
}

/// A <template-arg> production.
///
/// ```text
/// <template-arg> ::= <type>                # type or template
///                ::= X <expression> E      # expression
///                ::= <expr-primary>        # simple expressions
///                ::= J <template-arg>* E   # argument pack
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum TemplateArg {
    /// A type or template.
    Type(Type),

    /// An expression.
    Expression(Expression),

    /// A simple expression.
    SimpleExpression(ExprPrimary),

    /// An argument pack.
    ArgPack(Vec<TemplateArg>),
}

impl Parse for TemplateArg {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(TemplateArg, IndexStr<'b>)> {
        if let Ok((ty, tail)) = Type::parse(subs, input) {
            return Ok((TemplateArg::Type(ty), tail));
        }

        if let Ok(tail) = consume(b"X", input) {
            let (expr, tail) = try!(Expression::parse(subs, tail));
            let tail = try!(consume(b"E", tail));
            return Ok((TemplateArg::Expression(expr), tail));
        }

        if let Ok((expr, tail)) = ExprPrimary::parse(subs, input) {
            return Ok((TemplateArg::SimpleExpression(expr), tail));
        }

        let mut tail = try!(consume(b"J", input));
        let mut args = vec![];
        loop {
            if let Ok((arg, tail_tail)) = TemplateArg::parse(subs, tail) {
                args.push(arg);
                tail = tail_tail;
            } else {
                let tail = try!(consume(b"E", tail));
                return Ok((TemplateArg::ArgPack(args), tail));
            }
        }
    }
}

/// The `<expression>` production.
///
/// ```text
///  <expression> ::= <unary operator-name> <expression>
///               ::= <binary operator-name> <expression> <expression>
///               ::= <ternary operator-name> <expression> <expression> <expression>
///               ::= pp_ <expression>                             # prefix ++
///               ::= mm_ <expression>                             # prefix --
///               ::= cl <expression>+ E                           # expression (expr-list), call
///               ::= cv <type> <expression>                       # type (expression), conversion with one argument
///               ::= cv <type> _ <expression>* E                  # type (expr-list), conversion with other than one argument
///               ::= tl <type> <expression>* E                    # type {expr-list}, conversion with braced-init-list argument
///               ::= il <expression> E                            # {expr-list}, braced-init-list in any other context
///               ::= [gs] nw <expression>* _ <type> E             # new (expr-list) type
///               ::= [gs] nw <expression>* _ <type> <initializer> # new (expr-list) type (init)
///               ::= [gs] na <expression>* _ <type> E             # new[] (expr-list) type
///               ::= [gs] na <expression>* _ <type> <initializer> # new[] (expr-list) type (init)
///               ::= [gs] dl <expression>                         # delete expression
///               ::= [gs] da <expression>                         # delete[] expression
///               ::= dc <type> <expression>                       # dynamic_cast<type> (expression)
///               ::= sc <type> <expression>                       # static_cast<type> (expression)
///               ::= cc <type> <expression>                       # const_cast<type> (expression)
///               ::= rc <type> <expression>                       # reinterpret_cast<type> (expression)
///               ::= ti <type>                                    # typeid (type)
///               ::= te <expression>                              # typeid (expression)
///               ::= st <type>                                    # sizeof (type)
///               ::= sz <expression>                              # sizeof (expression)
///               ::= at <type>                                    # alignof (type)
///               ::= az <expression>                              # alignof (expression)
///               ::= nx <expression>                              # noexcept (expression)
///               ::= <template-param>
///               ::= <function-param>
///               ::= dt <expression> <unresolved-name>            # expr.name
///               ::= pt <expression> <unresolved-name>            # expr->name
///               ::= ds <expression> <expression>                 # expr.*expr
///               ::= sZ <template-param>                          # sizeof...(T), size of a template parameter pack
///               ::= sZ <function-param>                          # sizeof...(parameter), size of a function parameter pack
///               ::= sP <template-arg>* E                         # sizeof...(T), size of a captured template parameter pack from an alias template
///               ::= sp <expression>                              # expression..., pack expansion
///               ::= tw <expression>                              # throw expression
///               ::= tr                                           # throw with no operand (rethrow)
///               ::= <unresolved-name>                            # f(p), N::f(p), ::f(p),
///                                                                # freestanding dependent name (e.g., T::x),
///                                                                # objectless nonstatic member reference
///               ::= <expr-primary>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Expression;

impl Parse for Expression {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     _input: IndexStr<'b>)
                     -> Result<(Expression, IndexStr<'b>)> {
        Err("Not yet implemented".into())
    }
}

/// The `<unresolved-name>` production.
///
/// ```text
/// <unresolved-name> ::= [gs] <base-unresolved-name>
///                          # x or (with "gs") ::x
///                   ::= sr <unresolved-type> <base-unresolved-name>
///                          # T::x / decltype(p)::x
///                   ::= srN <unresolved-type> <unresolved-qualifier-level>+ E <base-unresolved-name>
///                          # T::N::x /decltype(p)::N::x
///                   ::= [gs] sr <unresolved-qualifier-level>+ E <base-unresolved-name>
///                          # A::x, N::y, A<T>::z; "gs" means leading "::"
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct UnresolvedName;

impl Parse for UnresolvedName {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     _input: IndexStr<'b>)
                     -> Result<(UnresolvedName, IndexStr<'b>)> {
        Err("Not yet implemented".into())
    }
}

/// The `<unresolved-type>` production.
///
/// ```text
/// <unresolved-type> ::= <template-param> [ <template-args> ]  # T:: or T<X,Y>::
///                   ::= <decltype>                            # decltype(p)::
///                   ::= <substitution>
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum UnresolvedType {
    /// An unresolved template type.
    Template(TemplateParam, Option<TemplateArgs>),

    /// An unresolved `decltype`.
    Decltype(Decltype),
}

impl Parse for UnresolvedType {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(UnresolvedType, IndexStr<'b>)> {
        if let Ok((param, tail)) = TemplateParam::parse(subs, input) {
            let (args, tail) = if let Ok((args, tail)) = TemplateArgs::parse(subs,
                                                                             tail) {
                (Some(args), tail)
            } else {
                (None, tail)
            };
            return Ok((UnresolvedType::Template(param, args), tail));
        }

        if let Ok((decltype, tail)) = Decltype::parse(subs, input) {
            return Ok((UnresolvedType::Decltype(decltype), tail));
        }

        // TODO: substitution variant

        Err("Not yet implemented".into())
    }
}

/// The `<unresolved-qualifier-level>` production.
///
/// ```text
/// <unresolved-qualifier-level> ::= <simple-id>
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct UnresolvedQualifierLevel(SimpleId);

impl Parse for UnresolvedQualifierLevel {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(UnresolvedQualifierLevel, IndexStr<'b>)> {
        let (id, tail) = try!(SimpleId::parse(subs, input));
        Ok((UnresolvedQualifierLevel(id), tail))
    }
}

/// The `<simple-id>` production.
///
/// ```text
/// <simple-id> ::= <source-name> [ <template-args> ]
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct SimpleId(SourceName, Option<TemplateArgs>);

impl Parse for SimpleId {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(SimpleId, IndexStr<'b>)> {
        let (name, tail) = try!(SourceName::parse(subs, input));
        let (args, tail) = if let Ok((args, tail)) = TemplateArgs::parse(subs, tail) {
            (Some(args), tail)
        } else {
            (None, tail)
        };
        Ok((SimpleId(name, args), tail))
    }
}

/// The `<base-unresolved-name>` production.
///
/// ```text
/// <base-unresolved-name> ::= <simple-id>                        # unresolved name
///                        ::= on <operator-name>                 # unresolved operator-function-id
///                        ::= on <operator-name> <template-args> # unresolved operator template-id
///                        ::= dn <destructor-name>               # destructor or pseudo-destructor;
///                                                               # e.g. ~X or ~X<N-1>
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum BaseUnresolvedName {
    /// An unresolved name.
    Name(SimpleId),

    /// An unresolved function or template function name.
    Operator(OperatorName, Option<TemplateArgs>),

    /// An unresolved destructor name.
    Destructor(DestructorName),
}

impl Parse for BaseUnresolvedName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(BaseUnresolvedName, IndexStr<'b>)> {
        if let Ok((name, tail)) = SimpleId::parse(subs, input) {
            return Ok((BaseUnresolvedName::Name(name), tail));
        }

        if let Ok(tail) = consume(b"on", input) {
            let (opname, tail) = try!(OperatorName::parse(subs, tail));
            let (args, tail) = if let Ok((args, tail)) = TemplateArgs::parse(subs,
                                                                             tail) {
                (Some(args), tail)
            } else {
                (None, tail)
            };
            return Ok((BaseUnresolvedName::Operator(opname, args), tail));
        }

        let tail = try!(consume(b"dn", input));
        let (name, tail) = try!(DestructorName::parse(subs, tail));
        Ok((BaseUnresolvedName::Destructor(name), tail))
    }
}

/// The `<destructor-name>` production.
///
/// ```text
/// <destructor-name> ::= <unresolved-type> # e.g., ~T or ~decltype(f())
///                   ::= <simple-id>       # e.g., ~A<2*N>
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum DestructorName {
    /// A destructor for an unresolved type.
    Unresolved(UnresolvedType),

    /// A destructor for a resolved type name.
    Name(SimpleId),
}

impl Parse for DestructorName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(DestructorName, IndexStr<'b>)> {
        if let Ok((ty, tail)) = UnresolvedType::parse(subs, input) {
            return Ok((DestructorName::Unresolved(ty), tail));
        }

        let (name, tail) = try!(SimpleId::parse(subs, input));
        Ok((DestructorName::Name(name), tail))
    }
}

/// The `<expr-primary>` production.
///
/// ```text
/// <expr-primary> ::= L <type> <value number> E                        # integer literal
///                ::= L <type> <value float> E                         # floating literal
///                ::= L <string type> E                                # string literal
///                ::= L <nullptr type> E                               # nullptr literal (i.e., "LDnE")
///                ::= L <pointer type> 0 E                             # null pointer template argument
///                ::= L <type> <real-part float> _ <imag-part float> E # complex floating point literal (C 2000)
///                ::= L <mangled-name> E                               # external name
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum ExprPrimary {
    /// An integer literal.
    Integer(Type, isize),

    /// A float literal.
    Float(Type, f64),

    /// A string literal.
    String(Type),

    /// A nullptr literal.
    Nullptr(Type),

    /// A nullptr template argument.
    TemplateNullptr(Type),

    /// A complex floating point literal.
    Complex(Type, f64, f64),

    /// An external name.
    External(MangledName),
}

impl Parse for ExprPrimary {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(ExprPrimary, IndexStr<'b>)> {
        let _tail = try!(consume(b"L", input));
        Err("Not yet implemented".into())
    }
}

/// The `<initializer>` production.
///
/// ```text
/// <initializer> ::= pi <expression>* E # parenthesized initialization
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Initializer(Vec<Expression>);

impl Parse for Initializer {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(Initializer, IndexStr<'b>)> {
        let mut tail = try!(consume(b"pi", input));
        let mut exprs = vec![];
        loop {
            if let Ok((expr, tail_tail)) = Expression::parse(subs, tail) {
                exprs.push(expr);
                tail = tail_tail;
            } else {
                let tail = try!(consume(b"E", tail));
                return Ok((Initializer(exprs), tail));
            }
        }
    }
}

/// The `<local-name>` production.
///
/// ```text
/// <local-name> := Z <function encoding> E <entity name> [<discriminator>]
///              := Z <function encoding> E s [<discriminator>]
///              := Z <function encoding> Ed [ <parameter number> ] _ <entity name>
/// ```
#[derive(Clone, Debug, PartialEq)]
pub enum LocalName {
    /// The mangling of the enclosing function, the mangling of the entity
    /// relative to the function, and an optional discriminator.
    Relative(Box<Encoding>, Option<Box<Name>>, Option<Discriminator>),

    /// A default argument in a class definition.
    Default(Box<Encoding>, Option<usize>, Box<Name>),
}

impl Parse for LocalName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(LocalName, IndexStr<'b>)> {
        let tail = try!(consume(b"Z", input));
        let (encoding, tail) = try!(Encoding::parse(subs, tail));

        if let Ok(tail) = consume(b"s", tail) {
            let (disc, tail) = try!(Discriminator::parse(subs, tail));
            return Ok((LocalName::Relative(Box::new(encoding), None, Some(disc)), tail));
        }

        if let Ok(tail) = consume(b"d", tail) {
            let (param, tail) = if let Ok((num, tail)) = Number::parse(subs, tail) {
                (Some(num as _), tail)
            } else {
                (None, tail)
            };
            let tail = try!(consume(b"_", tail));
            let (name, tail) = try!(Name::parse(subs, tail));
            return Ok((LocalName::Default(Box::new(encoding), param, Box::new(name)),
                       tail));
        }

        let (name, tail) = try!(Name::parse(subs, tail));
        let (disc, tail) = if let Ok((disc, tail)) = Discriminator::parse(subs, tail) {
            (Some(disc), tail)
        } else {
            (None, tail)
        };

        Ok((LocalName::Relative(Box::new(encoding), Some(Box::new(name)), disc), tail))
    }
}

/// The `<discriminator>` production.
///
/// ```text
/// <discriminator> := _ <non-negative number>      # when number < 10
///                 := __ <non-negative number> _   # when number >= 10
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Discriminator(usize);

impl Parse for Discriminator {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(Discriminator, IndexStr<'b>)> {
        let tail = try!(consume(b"_", input));

        if let Ok(tail) = consume(b"_", tail) {
            let (num, tail) = try!(parse_number(10, false, tail));
            debug_assert!(num >= 0);
            if num < 10 {
                return Err(ErrorKind::UnexpectedText.into());
            }
            let tail = try!(consume(b"_", tail));
            return Ok((Discriminator(num as _), tail));
        }

        match tail.try_split_at(1) {
            None => Err(ErrorKind::UnexpectedEnd.into()),
            Some((head, tail)) => {
                match head.as_ref()[0] {
                    b'0' => Ok((Discriminator(0), tail)),
                    b'1' => Ok((Discriminator(1), tail)),
                    b'2' => Ok((Discriminator(2), tail)),
                    b'3' => Ok((Discriminator(3), tail)),
                    b'4' => Ok((Discriminator(4), tail)),
                    b'5' => Ok((Discriminator(5), tail)),
                    b'6' => Ok((Discriminator(6), tail)),
                    b'7' => Ok((Discriminator(7), tail)),
                    b'8' => Ok((Discriminator(8), tail)),
                    b'9' => Ok((Discriminator(9), tail)),
                    _ => Err(ErrorKind::UnexpectedText.into()),
                }
            }
        }
    }
}

/// The `<closure-type-name>` production.
///
/// ```text
/// <closure-type-name> ::= Ul <lambda-sig> E [ <nonnegative number> ] _
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ClosureTypeName(LambdaSig, Option<usize>);

impl Parse for ClosureTypeName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(ClosureTypeName, IndexStr<'b>)> {
        let tail = try!(consume(b"Ul", input));
        let (sig, tail) = try!(LambdaSig::parse(subs, tail));
        let tail = try!(consume(b"E", tail));
        let (num, tail) = if let Ok((num, tail)) = parse_number(10, false, tail) {
            (Some(num as _), tail)
        } else {
            (None, tail)
        };
        let tail = try!(consume(b"_", tail));
        Ok((ClosureTypeName(sig, num), tail))
    }
}

/// The `<lambda-sig>` production.
///
/// ```text
/// <lambda-sig> ::= <parameter type>+  # Parameter types or "v" if the lambda has no parameters
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LambdaSig(Vec<Type>);

impl Parse for LambdaSig {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(LambdaSig, IndexStr<'b>)> {
        let (ty, mut tail) = try!(Type::parse(subs, input));
        let mut types = vec![ty];
        loop {
            if let Ok((ty, tail_tail)) = Type::parse(subs, tail) {
                types.push(ty);
                tail = tail_tail;
            } else {
                return Ok((LambdaSig(types), tail));
            }
        }
    }
}

/// The `<data-member-prefix>` production.
///
/// ```text
/// <data-member-prefix> := <member source-name> M
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct DataMemberPrefix(SourceName);

impl Parse for DataMemberPrefix {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(DataMemberPrefix, IndexStr<'b>)> {
        let (name, tail) = try!(SourceName::parse(subs, input));
        let tail = try!(consume(b"M", tail));
        Ok((DataMemberPrefix(name), tail))
    }
}

/// The `<substitution>` form: a back-reference to some component we've already
/// parsed.
///
/// ```text
/// <substitution> ::= S <seq-id> _
///                ::= S_
///                ::= St # ::std::
///                ::= Sa # ::std::allocator
///                ::= Sb # ::std::basic_string
///                ::= Ss # ::std::basic_string < char,
///                                               ::std::char_traits<char>,
///                                               ::std::allocator<char> >
///                ::= Si # ::std::basic_istream<char,  std::char_traits<char> >
///                ::= So # ::std::basic_ostream<char,  std::char_traits<char> >
///                ::= Sd # ::std::basic_iostream<char, std::char_traits<char> >
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Substitution {
    /// A reference to an entity that already occurred, ie the `S_` and `S
    /// <seq-id> _` forms.
    BackReference(usize),

    /// A well-known substitution component. These are the components that do
    /// not appear in the substitution table, but have abbreviations specified
    /// directly in the grammar.
    WellKnown(WellKnownComponent),
}

impl Parse for Substitution {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(Substitution, IndexStr<'b>)> {
        if let Ok((well_known, tail)) = WellKnownComponent::parse(subs, input) {
            return Ok((Substitution::WellKnown(well_known), tail));
        }

        let tail = try!(consume(b"S", input));
        let (idx, tail) = if let Ok((idx, tail)) = SeqId::parse(subs, tail) {
            (idx.0 + 1, tail)
        } else {
            (0, tail)
        };

        if !subs.contains(idx) {
            return Err(ErrorKind::BadBackReference.into());
        }

        let tail = try!(consume(b"_", tail));
        Ok((Substitution::BackReference(idx), tail))
    }
}

define_vocabulary! {
/// The `<substitution>` variants that are encoded directly in the grammar,
/// rather than as back references to other components in the substitution
/// table.
    #[derive(Clone, Debug, Hash, PartialEq, Eq)]
    pub enum WellKnownComponent {
        Std          (b"St", "::std::"),
        StdAllocator (b"Sa", "::std::allocator"),
        StdString1   (b"Sb", "::std::basic_string"),
        StdString2   (b"Ss", "::std::basic_string < char, ::std::char_traits<char>, ::std::allocator<char> >"),
        StdIstream   (b"Si", "::std::basic_istream<char, std::char_traits<char> >"),
        StdOstream   (b"So", "::std::basic_ostream<char, std::char_traits<char> >"),
        StdIostream  (b"Sd", "::std::basic_iostream<char, std::char_traits<char> >")
    }
}

/// The `<special-name>` production.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SpecialName;

impl Parse for SpecialName {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     _input: IndexStr<'b>)
                     -> Result<(SpecialName, IndexStr<'b>)> {
        Err("Not yet implemented".into())
    }
}

/// Expect and consume the given byte str, and return the advanced `IndexStr` if
/// we saw the expectation. Otherwise return an error of kind
/// `ErrorKind::UnexpectedText` if the input doesn't match, or
/// `ErrorKind::UnexpectedEnd` if it isn't long enough.
#[inline]
fn consume<'a>(expected: &[u8], input: IndexStr<'a>) -> Result<IndexStr<'a>> {
    match input.try_split_at(expected.len()) {
        Some((head, tail)) if head == expected => Ok(tail),
        Some(_) => Err(ErrorKind::UnexpectedText.into()),
        None => Err(ErrorKind::UnexpectedEnd.into()),
    }
}

/// Parse a number with the given `base`. Do not allow negative numbers
/// (prefixed with an 'n' instead of a '-') if `allow_signed` is false.
fn parse_number(base: u32,
                allow_signed: bool,
                mut input: IndexStr)
                -> Result<(isize, IndexStr)> {
    if input.is_empty() {
        return Err(ErrorKind::UnexpectedEnd.into());
    }

    let num_is_negative = if allow_signed && input.as_ref()[0] == b'n' {
        input = input.range_from(1..);

        if input.is_empty() {
            return Err(ErrorKind::UnexpectedEnd.into());
        }

        true
    } else {
        false
    };

    let num_numeric = input.as_ref()
        .iter()
        .map(|&c| c as char)
        .take_while(|c| c.is_digit(base) && (c.is_numeric() || c.is_uppercase()))
        .count();
    if num_numeric == 0 {
        return Err(ErrorKind::UnexpectedText.into());
    }

    let (head, tail) = input.split_at(num_numeric);
    let head = head.as_ref();

    if num_numeric > 1 && head[0] == b'0' {
        // "<number>s appearing in mangled names never have leading zeroes,
        // except for the value zero, represented as '0'."
        return Err(ErrorKind::UnexpectedText.into());
    }

    let head = unsafe {
        // Safe because we know we only have valid numeric chars in this
        // slice.
        ::std::str::from_utf8_unchecked(head)
    };

    let mut number = isize::from_str_radix(head, base)
        .expect("We should only have numeric characters");
    if num_is_negative {
        number = -number;
    }

    Ok((number, tail))
}

#[cfg(test)]
mod tests {
    use error::ErrorKind;
    use super::{BuiltinType, CallOffset, CtorDtorName, CvQualifiers, DataMemberPrefix,
                Discriminator, FunctionParam, Identifier, Number, NvOffset,
                OperatorName, Parse, RefQualifier, SeqId, SourceName,
                StandardBuiltinType, SubstitutionTable, TemplateParam, UnnamedTypeName,
                UnqualifiedName, UnscopedName, VOffset};

    /// Try to parse something, and check the result. For example:
    ///
    ///     assert_parse!(OperatorName: "quokka" => Ok(OperatorName::Question, "okka"));
    ///     assert_parse!(OperatorName: "quokka" => Err(ErrorKind::UnexpectedEnd)));
    ///
    /// This asserts that calling `OperatorName::parse` on the text `"qu"`
    /// succeeds, producing the value `OperatorName::Question`, and leaving the
    /// unparsed text `"okka"`.
    macro_rules! assert_parse {
        ($nonterm:ty : $input:expr => Ok($ex_value:expr, $ex_tail:expr)) => {
            let input = $input as &[u8];
            let input_printable = String::from_utf8_lossy(input).into_owned();
            let ex_value = $ex_value;
            let ex_tail = $ex_tail as &[u8];
            let mut subs = SubstitutionTable::new();
            match <$nonterm>::parse(&mut subs, ::index_str::IndexStr::from(input)) {
                Err(e) => panic!("Parsing {:?} as {} failed: {}",
                                 input_printable, stringify!($nonterm), e),
                Ok((value, tail)) => {
                    if value != ex_value {
                        panic!("Parsing {:?} as {} produced {:?}, expected {:?}",
                               input_printable, stringify!($nonterm), value, ex_value);
                    }
                    if tail != ex_tail {
                        panic!("Parsing {:?} as {} left a tail of {:?}, expected {:?}",
                               input_printable, stringify!($nonterm), tail.as_ref(), ex_tail);
                    }
                }
            }
        };

        ($nonterm:ty : $input:expr => Err($ex_error:pat)) => {
            let input = $input as &[u8];
            let input_printable = String::from_utf8_lossy(input).into_owned();
            let mut subs = SubstitutionTable::new();
            match <$nonterm>::parse(&mut subs, ::index_str::IndexStr::from(input)) {
                Err(::error::Error($ex_error, _)) => { },
                Err(err) => {
                    panic!("Parsing {:?} as {} should fail with {},\n\
                            failed with {:?} instead",
                           input_printable, stringify!($nonterm), stringify!($ex_error), err.kind());
                }
                Ok((value, tail)) => {
                    panic!("Parsing {:?} as {} should fail with {},\n\
                            but succeeded with value {:?}, tail {:?}",
                           input_printable, stringify!($nonterm), stringify!($ex_error), value, tail);
                }
            }
        }
    }

    #[test]
    fn parse_function_param() {
        assert_parse!(FunctionParam: b"fpK_..." =>
                      Ok(FunctionParam(0, CvQualifiers {
                          restrict: false,
                          volatile: false,
                          const_: true,
                      }, None), b"..."));
        assert_parse!(FunctionParam: b"fL1pK_..." =>
                      Ok(FunctionParam(1, CvQualifiers {
                          restrict: false,
                          volatile: false,
                          const_: true,
                      }, None), b"..."));
        assert_parse!(FunctionParam: b"fpK3_..." =>
                      Ok(FunctionParam(0, CvQualifiers {
                          restrict: false,
                          volatile: false,
                          const_: true,
                      }, Some(3)), b"..."));
        assert_parse!(FunctionParam: b"fL1pK4_..." =>
                      Ok(FunctionParam(1, CvQualifiers {
                          restrict: false,
                          volatile: false,
                          const_: true,
                      }, Some(4)), b"..."));
        assert_parse!(FunctionParam: b"fz" => Err(ErrorKind::UnexpectedText));
        assert_parse!(FunctionParam: b"fLp_" => Err(ErrorKind::UnexpectedText));
        assert_parse!(FunctionParam: b"fpL_" => Err(ErrorKind::UnexpectedText));
        assert_parse!(FunctionParam: b"fL1pK4z" => Err(ErrorKind::UnexpectedText));
        assert_parse!(FunctionParam: b"fL1pK4" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(FunctionParam: b"fL1p" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(FunctionParam: b"fL1" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(FunctionParam: b"fL" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(FunctionParam: b"f" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(FunctionParam: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_discriminator() {
        assert_parse!(Discriminator: b"_0..." => Ok(Discriminator(0), b"..."));
        assert_parse!(Discriminator: b"_9..." => Ok(Discriminator(9), b"..."));
        assert_parse!(Discriminator: b"__99_..." => Ok(Discriminator(99), b"..."));
        assert_parse!(Discriminator: b"_n1" => Err(ErrorKind::UnexpectedText));
        assert_parse!(Discriminator: b"__99..." => Err(ErrorKind::UnexpectedText));
        assert_parse!(Discriminator: b"__99" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(Discriminator: b"..." => Err(ErrorKind::UnexpectedText));
    }

    #[test]
    fn parse_data_member_prefix() {
        assert_parse!(DataMemberPrefix: b"3fooM..." =>
                      Ok(DataMemberPrefix(SourceName(Identifier {
                          start: 1,
                          end: 4,
                      })),
                         b"..."));
        assert_parse!(DataMemberPrefix: b"zzz" => Err(ErrorKind::UnexpectedText));
    }

    #[test]
    fn parse_ref_qualifier() {
        assert_parse!(RefQualifier: b"R..." => Ok(RefQualifier::LValueRef, b"..."));
        assert_parse!(RefQualifier: b"O..." => Ok(RefQualifier::RValueRef, b"..."));
        assert_parse!(RefQualifier: b"..." => Err(ErrorKind::UnexpectedText));
        assert_parse!(RefQualifier: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_cv_qualifiers() {
        assert_parse!(CvQualifiers: b"" =>
                      Ok(CvQualifiers { restrict: false, volatile: false, const_: false },
                         b""));
        assert_parse!(CvQualifiers: b"..." =>
                      Ok(CvQualifiers { restrict: false, volatile: false, const_: false },
                         b"..."));

        assert_parse!(CvQualifiers: b"r..." =>
                      Ok(CvQualifiers { restrict: true, volatile: false, const_: false },
                         b"..."));
        assert_parse!(CvQualifiers: b"rV..." =>
                      Ok(CvQualifiers { restrict: true, volatile: true, const_: false },
                         b"..."));
        assert_parse!(CvQualifiers: b"rVK..." =>
                      Ok(CvQualifiers { restrict: true, volatile: true, const_: true },
                         b"..."));

        assert_parse!(CvQualifiers: b"V" =>
                      Ok(CvQualifiers { restrict: false, volatile: true, const_: false },
                         b""));
        assert_parse!(CvQualifiers: b"VK" =>
                      Ok(CvQualifiers { restrict: false, volatile: true, const_: true },
                         b""));

        assert_parse!(CvQualifiers: b"K..." =>
                      Ok(CvQualifiers { restrict: false, volatile: false, const_: true },
                         b"..."));
    }

    #[test]
    fn parse_builtin_type() {
        assert_parse!(BuiltinType: b"c..." =>
                      Ok(BuiltinType::Standard(StandardBuiltinType::Char), b"..."));
        assert_parse!(BuiltinType: b"c" =>
                      Ok(BuiltinType::Standard(StandardBuiltinType::Char), b""));
        assert_parse!(BuiltinType: b"u3abc..." =>
                      Ok(BuiltinType::Extension(SourceName(Identifier {
                          start: 2,
                          end: 5,
                      })),
                         b"..."));
        assert_parse!(BuiltinType: b"." => Err(ErrorKind::UnexpectedText));
        assert_parse!(BuiltinType: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_template_param() {
        assert_parse!(TemplateParam: b"T_..." => Ok(TemplateParam(None), b"..."));
        assert_parse!(TemplateParam: b"T3_..." => Ok(TemplateParam(Some(3)), b"..."));
        assert_parse!(TemplateParam: b"wtf" => Err(ErrorKind::UnexpectedText));
        assert_parse!(TemplateParam: b"Twtf" => Err(ErrorKind::UnexpectedText));
        assert_parse!(TemplateParam: b"T3wtf" => Err(ErrorKind::UnexpectedText));
        assert_parse!(TemplateParam: b"T" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(TemplateParam: b"T3" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(TemplateParam: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_unscoped_name() {
        assert_parse!(UnscopedName: b"St5hello..." =>
                      Ok(UnscopedName::Std(UnqualifiedName::Source(SourceName(Identifier {
                          start: 3,
                          end: 8,
                      }))),
                         b"..."));
        assert_parse!(UnscopedName: b"5hello..." =>
                      Ok(UnscopedName::Unqualified(UnqualifiedName::Source(SourceName(Identifier {
                          start: 1,
                          end: 6,
                      }))),
                         b"..."));
        assert_parse!(UnscopedName: b"St..." => Err(ErrorKind::UnexpectedText));
        assert_parse!(UnscopedName: b"..." => Err(ErrorKind::UnexpectedText));
        assert_parse!(UnscopedName: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_unqualified_name() {
        assert_parse!(UnqualifiedName: b"qu.." =>
                      Ok(UnqualifiedName::Operator(OperatorName::Question), b".."));
        assert_parse!(UnqualifiedName: b"C1.." =>
                      Ok(UnqualifiedName::CtorDtor(CtorDtorName::CompleteConstructor), b".."));
        assert_parse!(UnqualifiedName: b"10abcdefghij..." =>
                      Ok(UnqualifiedName::Source(SourceName(Identifier {
                          start: 2,
                          end: 12,
                      })),
                         b"..."));
        assert_parse!(UnqualifiedName: b"Ut5_..." =>
                      Ok(UnqualifiedName::UnnamedType(UnnamedTypeName(Some(5))),
                         b"..."));
    }

    #[test]
    fn parse_unnamed_type_name() {
        assert_parse!(UnnamedTypeName: b"Ut_abc" => Ok(UnnamedTypeName(None), b"abc"));
        assert_parse!(UnnamedTypeName: b"Ut42_abc" => Ok(UnnamedTypeName(Some(42)), b"abc"));
        assert_parse!(UnnamedTypeName: b"Ut42_" => Ok(UnnamedTypeName(Some(42)), b""));
        assert_parse!(UnnamedTypeName: b"ut_" => Err(ErrorKind::UnexpectedText));
        assert_parse!(UnnamedTypeName: b"u" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(UnnamedTypeName: b"Ut" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(UnnamedTypeName: b"Ut._" => Err(ErrorKind::UnexpectedText));
        assert_parse!(UnnamedTypeName: b"Ut42" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_identifier() {
        assert_parse!(Identifier: b"1abc" =>
                      Ok(Identifier { start: 0, end: 4 }, b""));
        assert_parse!(Identifier: b"_Az1..." =>
                      Ok(Identifier { start: 0, end: 4 }, b"..."));
        assert_parse!(Identifier: b"..." => Err(ErrorKind::UnexpectedText));
        assert_parse!(Identifier: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_source_name() {
        assert_parse!(SourceName: b"1abc" =>
                      Ok(SourceName(Identifier { start: 1, end: 2 }), b"bc"));
        assert_parse!(SourceName: b"10abcdefghijklm" =>
                      Ok(SourceName(Identifier { start: 2, end: 12 }), b"klm"));
        assert_parse!(SourceName: b"0abc" => Err(ErrorKind::UnexpectedText));
        assert_parse!(SourceName: b"n1abc" => Err(ErrorKind::UnexpectedText));
        assert_parse!(SourceName: b"10abcdef" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(SourceName: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_number() {
        assert_parse!(Number: b"n2n3" => Ok(-2, b"n3"));
        assert_parse!(Number: b"12345abcdef" => Ok(12345, b"abcdef"));
        assert_parse!(Number: b"0abcdef" => Ok(0, b"abcdef"));
        assert_parse!(Number: b"42" => Ok(42, b""));
        assert_parse!(Number: b"001" => Err(ErrorKind::UnexpectedText));
        assert_parse!(Number: b"wutang" => Err(ErrorKind::UnexpectedText));
        assert_parse!(Number: b"n" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(Number: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_call_offset() {
        assert_parse!(CallOffset: b"hn42_..." =>
                      Ok(CallOffset::NonVirtual(NvOffset(-42)), b"..."));
        assert_parse!(CallOffset: b"vn42_36_..." =>
                      Ok(CallOffset::Virtual(VOffset(-42, 36)), b"..."));
        assert_parse!(CallOffset: b"h1..." => Err(ErrorKind::UnexpectedText));
        assert_parse!(CallOffset: b"v1_1..." => Err(ErrorKind::UnexpectedText));
        assert_parse!(CallOffset: b"hh" => Err(ErrorKind::UnexpectedText));
        assert_parse!(CallOffset: b"vv" => Err(ErrorKind::UnexpectedText));
        assert_parse!(CallOffset: b"z" => Err(ErrorKind::UnexpectedText));
        assert_parse!(CallOffset: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_v_offset() {
        assert_parse!(VOffset: b"n2_n3abcdef" => Ok(VOffset(-2, -3), b"abcdef"));
        assert_parse!(VOffset: b"12345_12345abcdef" => Ok(VOffset(12345, 12345), b"abcdef"));
        assert_parse!(VOffset: b"0_0abcdef" => Ok(VOffset(0, 0), b"abcdef"));
        assert_parse!(VOffset: b"42_n3" => Ok(VOffset(42, -3), b""));
        assert_parse!(VOffset: b"001" => Err(ErrorKind::UnexpectedText));
        assert_parse!(VOffset: b"1_001" => Err(ErrorKind::UnexpectedText));
        assert_parse!(VOffset: b"wutang" => Err(ErrorKind::UnexpectedText));
        assert_parse!(VOffset: b"n_" => Err(ErrorKind::UnexpectedText));
        assert_parse!(VOffset: b"1_n" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(VOffset: b"1_" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(VOffset: b"n" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(VOffset: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_nv_offset() {
        assert_parse!(NvOffset: b"n2n3" => Ok(NvOffset(-2), b"n3"));
        assert_parse!(NvOffset: b"12345abcdef" => Ok(NvOffset(12345), b"abcdef"));
        assert_parse!(NvOffset: b"0abcdef" => Ok(NvOffset(0), b"abcdef"));
        assert_parse!(NvOffset: b"42" => Ok(NvOffset(42), b""));
        assert_parse!(NvOffset: b"001" => Err(ErrorKind::UnexpectedText));
        assert_parse!(NvOffset: b"wutang" => Err(ErrorKind::UnexpectedText));
        assert_parse!(NvOffset: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_seq_id() {
        assert_parse!(SeqId: b"1_" => Ok(SeqId(1), b"_"));
        assert_parse!(SeqId: b"42" => Ok(SeqId(146), b""));
        assert_parse!(SeqId: b"ABCabc" => Ok(SeqId(13368), b"abc"));
        assert_parse!(SeqId: b"abc" => Err(ErrorKind::UnexpectedText));
        assert_parse!(SeqId: b"001" => Err(ErrorKind::UnexpectedText));
        assert_parse!(SeqId: b"wutang" => Err(ErrorKind::UnexpectedText));
        assert_parse!(SeqId: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_ctor_dtor_name() {
        assert_parse!(CtorDtorName: b"D0" => Ok(CtorDtorName::DeletingDestructor, b""));
        assert_parse!(CtorDtorName: b"C101" => Ok(CtorDtorName::CompleteConstructor, b"01"));
        assert_parse!(CtorDtorName: b"gayagaya" => Err(ErrorKind::UnexpectedText));
        assert_parse!(CtorDtorName: b"C" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(CtorDtorName: b"" => Err(ErrorKind::UnexpectedEnd));
    }

    #[test]
    fn parse_operator_name() {
        assert_parse!(OperatorName: b"qu" => Ok(OperatorName::Question, b""));
        assert_parse!(OperatorName: b"quokka" => Ok(OperatorName::Question, b"okka"));
        assert_parse!(OperatorName: b"bu-buuu" => Err(ErrorKind::UnexpectedText));
        assert_parse!(OperatorName: b"q" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(OperatorName: b"" => Err(ErrorKind::UnexpectedEnd));
    }
}
