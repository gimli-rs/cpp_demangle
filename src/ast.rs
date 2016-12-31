//! Abstract syntax tree types for mangled symbols.

use error::{ErrorKind, Result};
use index_str::IndexStr;
#[cfg(feature = "logging")]
use std::cell::RefCell;
use std::fmt;
use subs::{Substitutable, SubstitutionTable};

struct AutoLogParse;

thread_local! {
    #[cfg(feature = "logging")]
    static PARSE_DEPTH: RefCell<usize> = RefCell::new(0);
}

impl AutoLogParse {
    #[cfg(feature = "logging")]
    fn new<'a>(production: &'static str, input: IndexStr<'a>) -> AutoLogParse {
        PARSE_DEPTH.with(|depth| {
            let indent: String = (0..*depth.borrow() * 4).map(|_| ' ').collect();
            log!("{}({}::parse: {:?}", indent, production, input);
            *depth.borrow_mut() += 1;
        });
        AutoLogParse
    }

    #[cfg(not(feature = "logging"))]
    fn new<'a>(_: &'static str, _: IndexStr<'a>) -> AutoLogParse {
        AutoLogParse
    }
}

#[cfg(feature = "logging")]
impl Drop for AutoLogParse {
    fn drop(&mut self) {
        PARSE_DEPTH.with(|depth| {
            *depth.borrow_mut() -= 1;
            let indent: String = (0..*depth.borrow() * 4).map(|_| ' ').collect();
            log!("{})", indent);
        });
    }
}

/// Automatically log start and end parsing in an s-expression format, when the
/// `logging` feature is enabled.
macro_rules! log_parse {
    ( $production:expr , $input:expr ) => {
        let _log = AutoLogParse::new($production, $input);
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

/// Define a handle to a AST type that lives inside the substitution table. A
/// handle is always either an index into the substitution table, or it is a
/// reference to a "well-known" component.
macro_rules! define_handle {
    ( $(#[$attr:meta])* pub enum $typename:ident ) => {
        $(#[$attr])*
            #[derive(Clone, Debug, Hash, PartialEq, Eq)]
        pub enum $typename {
            /// A reference to a "well-known" component.
            WellKnown(WellKnownComponent),

            /// A back-reference into the substitution table to a component we
            /// have already parsed.
            BackReference(usize),
        }
    }
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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct MangledName(Encoding);

impl Parse for MangledName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(MangledName, IndexStr<'b>)> {
        log_parse!("MangledName", input);

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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
        log_parse!("Encoding", input);

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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
        log_parse!("Name", input);

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
        log_parse!("UnscopedName", input);

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

define_handle! {
    /// A handle to an `UnscopedTemplateName`.
    pub enum UnscopedTemplateNameHandle
}

impl Parse for UnscopedTemplateNameHandle {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(UnscopedTemplateNameHandle, IndexStr<'b>)> {
        log_parse!("UnscopedTemplateNameHandle", input);

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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum NestedName {
    /// An unqualified name.
    Unqualified(CvQualifiers, Option<RefQualifier>, PrefixHandle, UnqualifiedName),

    /// A template name.
    Template(CvQualifiers, Option<RefQualifier>, TemplatePrefixHandle, TemplateArgs),
}

impl Parse for NestedName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(NestedName, IndexStr<'b>)> {
        log_parse!("NestedName", input);

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

        if let Ok((prefix, tail)) = PrefixHandle::parse(subs, tail) {
            let (name, tail) = try!(UnqualifiedName::parse(subs, tail));
            let tail = try!(consume(b"E", tail));
            return Ok((NestedName::Unqualified(cv_qualifiers,
                                               ref_qualifier,
                                               prefix,
                                               name),
                       tail));
        }

        let (prefix, tail) = try!(TemplatePrefixHandle::parse(subs, tail));
        let (args, tail) = try!(TemplateArgs::parse(subs, tail));
        let tail = try!(consume(b"E", tail));
        Ok((NestedName::Template(cv_qualifiers, ref_qualifier, prefix, args), tail))
    }
}

/// The `<prefix>` production.
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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Prefix {
    /// An unqualified name.
    Unqualified(UnqualifiedName),

    /// Some nested name.
    Nested(PrefixHandle, UnqualifiedName),

    /// A prefix and template arguments.
    Template(PrefixHandle, TemplateArgs),

    /// A template parameter.
    TemplateParam(TemplateParam),

    /// A decltype.
    Decltype(Decltype),

    /// A prefix and data member.
    DataMember(PrefixHandle, DataMemberPrefix),
}

define_handle! {
    /// A reference to a parsed `<prefix>` production.
    pub enum PrefixHandle
}

impl Parse for PrefixHandle {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(PrefixHandle, IndexStr<'b>)> {
        let mut tail = input;
        let mut current = None;

        loop {
            log_parse!("PrefixHandle", tail);

            if let Ok((name, tail_tail)) = UnqualifiedName::parse(subs, tail) {
                if let Some(handle) = current {
                    match tail_tail.peek() {
                        Some(b'I') => {
                            // This is a <template-prefix>.
                            let (args, tail_tail) = try!(TemplateArgs::parse(subs,
                                                                             tail_tail));
                            let prefix = Prefix::Template(handle, args);
                            let idx = subs.insert(Substitutable::Prefix(prefix));
                            current = Some(PrefixHandle::BackReference(idx));
                            tail = tail_tail;
                        }
                        // TODO: ...
                        // Some(b'M') => {
                        //     // This is a <data-member-prefix>.
                        //     tail = consume(b"M", tail_tail).unwrap();
                        //     let prefix = Prefix::DataMember(handle)
                        // }
                        _ => {
                            // This is a nested prefix.
                            let prefix = Prefix::Nested(handle, name);
                            let idx = subs.insert(Substitutable::Prefix(prefix));
                            current = Some(PrefixHandle::BackReference(idx));
                            tail = tail_tail;
                        }
                    }
                } else {
                    let prefix = Prefix::Unqualified(name);
                    let idx = subs.insert(Substitutable::Prefix(prefix));
                    current = Some(PrefixHandle::BackReference(idx));
                    tail = tail_tail;
                }

                continue;
            }

            if let Ok((param, tail_tail)) = TemplateParam::parse(subs, tail) {
                let prefix = Prefix::TemplateParam(param);
                let idx = subs.insert(Substitutable::Prefix(prefix));
                current = Some(PrefixHandle::BackReference(idx));
                tail = tail_tail;
                continue;
            }

            if let Ok((decltype, tail_tail)) = Decltype::parse(subs, tail) {
                let prefix = Prefix::Decltype(decltype);
                let idx = subs.insert(Substitutable::Prefix(prefix));
                current = Some(PrefixHandle::BackReference(idx));
                tail = tail_tail;
                continue;
            }

            if let Ok((sub, tail_tail)) = Substitution::parse(subs, tail) {
                current = Some(match sub {
                    Substitution::WellKnown(component) => {
                        PrefixHandle::WellKnown(component)
                    }
                    Substitution::BackReference(idx) => {
                        // TODO: do we need to check that the idx actually points to
                        // a Prefix or TemplatePrefix?
                        PrefixHandle::BackReference(idx)
                    }
                });
                tail = tail_tail;
                continue;
            }

            if let Some(handle) = current {
                return Ok((handle, tail));
            } else {
                // TODO: or UnexpectedEnd if EOF...
                return Err(ErrorKind::UnexpectedText.into());
            }
        }
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
pub enum TemplatePrefix {
    /// A template name.
    UnqualifiedName(UnqualifiedName),

    /// A nested template name.
    Nested(PrefixHandle, UnqualifiedName),

    /// A template template parameter.
    TemplateTemplate(TemplateParam),
}

define_handle! {
    /// A reference to a parsed `TemplatePrefix`.
    pub enum TemplatePrefixHandle
}

impl Parse for TemplatePrefixHandle {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(TemplatePrefixHandle, IndexStr<'b>)> {
        log_parse!("TemplatePrefixHandle", input);

        if let Ok((name, tail)) = UnqualifiedName::parse(subs, input) {
            let prefix = TemplatePrefix::UnqualifiedName(name);
            let prefix = Substitutable::TemplatePrefix(prefix);
            let idx = subs.insert(prefix);
            let handle = TemplatePrefixHandle::BackReference(idx);
            return Ok((handle, tail));
        }

        if let Ok((prefix, tail)) = PrefixHandle::parse(subs, input) {
            let (name, tail) = try!(UnqualifiedName::parse(subs, tail));
            let nested = TemplatePrefix::Nested(prefix, name);
            let prefix = Substitutable::TemplatePrefix(nested);
            let idx = subs.insert(prefix);
            let handle = TemplatePrefixHandle::BackReference(idx);
            return Ok((handle, tail));
        }

        if let Ok((param, tail)) = TemplateParam::parse(subs, input) {
            let prefix = TemplatePrefix::TemplateTemplate(param);
            let prefix = Substitutable::TemplatePrefix(prefix);
            let idx = subs.insert(prefix);
            let handle = TemplatePrefixHandle::BackReference(idx);
            return Ok((handle, tail));
        }

        let (sub, tail) = try!(Substitution::parse(subs, input));
        match sub {
            Substitution::WellKnown(component) => {
                Ok((TemplatePrefixHandle::WellKnown(component), tail))
            }
            Substitution::BackReference(idx) => {
                // TODO: should this check if the back reference actually points
                // to a <template-prefix> ?
                Ok((TemplatePrefixHandle::BackReference(idx), tail))
            }
        }
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
        log_parse!("UnqualifiedName", input);

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
        log_parse!("SourceName", input);

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
        log_parse!("Identifier", input);

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
        log_parse!("Number", input);

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
        log_parse!("SeqId", input);

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
        log_parse!("CallOffset", input);

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
        log_parse!("NvOffset", input);

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
        log_parse!("VOffset", input);

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
pub enum Type {
    /// A builtin type.
    Builtin(BuiltinType),

    /// A function type.
    Function(FunctionType),

    /// A class, union, or enum type.
    ClassEnum(ClassEnumType),

    /// An array type.
    Array(ArrayType),

    /// A pointer-to-member type.
    PointerToMember(PointerToMemberType),

    /// A named template parameter type.
    TemplateParam(TemplateParam),

    /// A template template type.
    TemplateTemplate(TemplateTemplateParamHandle, TemplateArgs),

    /// A decltype.
    Decltype(Decltype),

    /// A const-, restrict-, and/or volatile-qualified type.
    Qualified(CvQualifiers, TypeHandle),

    /// A pointer to a type.
    PointerTo(TypeHandle),

    /// An lvalue reference to a type.
    LvalueRef(TypeHandle),

    /// An rvalue reference to a type.
    RvalueRef(TypeHandle),

    /// A complex pair of the given type.
    Complex(TypeHandle),

    /// An imaginary of the given type.
    Imaginary(TypeHandle),

    /// A vendor extended type qualifier.
    VendorExtension(SourceName, Option<TemplateArgs>, TypeHandle),

    /// A pack expansion.
    PackExpansion(TypeHandle),
}

define_handle! {
    /// A reference to a parsed `Type` production.
    pub enum TypeHandle
}

impl Parse for TypeHandle {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(TypeHandle, IndexStr<'b>)> {
        log_parse!("TypeHandle", input);

        if let Ok((sub, tail)) = Substitution::parse(subs, input) {
            match sub {
                Substitution::WellKnown(component) => {
                    return Ok((TypeHandle::WellKnown(component), tail));
                }
                Substitution::BackReference(idx) => {
                    // TODO: should this check if the back reference actually points
                    // to a <type>?
                    return Ok((TypeHandle::BackReference(idx), tail));
                }
            }
        }

        /// Insert the given type into the substitution table, and return a
        /// handle referencing the index in the table where it ended up.
        fn insert_and_return_handle<'a, 'b>(ty: Type,
                                            subs: &'a mut SubstitutionTable,
                                            tail: IndexStr<'b>)
                                            -> Result<(TypeHandle, IndexStr<'b>)> {
            let ty = Substitutable::Type(ty);
            let idx = subs.insert(ty);
            let handle = TypeHandle::BackReference(idx);
            Ok((handle, tail))
        }

        if let Ok((builtin, tail)) = BuiltinType::parse(subs, input) {
            let ty = Type::Builtin(builtin);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((funty, tail)) = FunctionType::parse(subs, input) {
            let ty = Type::Function(funty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((ty, tail)) = ClassEnumType::parse(subs, input) {
            let ty = Type::ClassEnum(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((ty, tail)) = ArrayType::parse(subs, input) {
            let ty = Type::Array(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((ty, tail)) = PointerToMemberType::parse(subs, input) {
            let ty = Type::PointerToMember(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((param, tail)) = TemplateParam::parse(subs, input) {
            let ty = Type::TemplateParam(param);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((ttp, tail)) = TemplateTemplateParamHandle::parse(subs, input) {
            let (args, tail) = try!(TemplateArgs::parse(subs, tail));
            let ty = Type::TemplateTemplate(ttp, args);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((param, tail)) = Decltype::parse(subs, input) {
            let ty = Type::Decltype(param);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok((qualifiers, tail)) = CvQualifiers::parse(subs, input) {
            // CvQualifiers can parse successfully without consuming any input,
            // but we don't want to recurse unless we know we did consume some
            // input, lest we go into an infinite loop and blow the stack.
            if tail.len() < input.len() {
                let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                let ty = Type::Qualified(qualifiers, ty);
                return insert_and_return_handle(ty, subs, tail);
            }
        }

        if let Ok(tail) = consume(b"P", input) {
            let (ty, tail) = try!(TypeHandle::parse(subs, tail));
            let ty = Type::PointerTo(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok(tail) = consume(b"R", input) {
            let (ty, tail) = try!(TypeHandle::parse(subs, tail));
            let ty = Type::LvalueRef(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok(tail) = consume(b"O", input) {
            let (ty, tail) = try!(TypeHandle::parse(subs, tail));
            let ty = Type::RvalueRef(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok(tail) = consume(b"C", input) {
            let (ty, tail) = try!(TypeHandle::parse(subs, tail));
            let ty = Type::Complex(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok(tail) = consume(b"G", input) {
            let (ty, tail) = try!(TypeHandle::parse(subs, tail));
            let ty = Type::Imaginary(ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        if let Ok(tail) = consume(b"U", input) {
            let (name, tail) = try!(SourceName::parse(subs, tail));
            let (args, tail) = if let Ok((args, tail)) = TemplateArgs::parse(subs,
                                                                             tail) {
                (Some(args), tail)
            } else {
                (None, tail)
            };
            let (ty, tail) = try!(TypeHandle::parse(subs, tail));
            let ty = Type::VendorExtension(name, args, ty);
            return insert_and_return_handle(ty, subs, tail);
        }

        let tail = try!(consume(b"Dp", input));
        let (ty, tail) = try!(TypeHandle::parse(subs, tail));
        let ty = Type::PackExpansion(ty);
        insert_and_return_handle(ty, subs, tail)
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
        log_parse!("CvQualifiers", input);

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
        log_parse!("BuiltinType", input);

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
        log_parse!("FunctionType", input);

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
pub struct BareFunctionType(Vec<TypeHandle>);

impl Parse for BareFunctionType {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(BareFunctionType, IndexStr<'b>)> {
        log_parse!("BareFunctionType", input);

        let (types, tail) = try!(one_or_more::<TypeHandle>(subs, input));
        Ok((BareFunctionType(types), tail))
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
        log_parse!("Decltype", input);

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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
        log_parse!("ClassEnumType", input);

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
        log_parse!("UnnamedTypeName", input);

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
    DimensionNumber(usize, TypeHandle),

    /// An array with an expression for its dimension.
    DimensionExpression(Expression, TypeHandle),

    /// An array with no dimension.
    NoDimension(TypeHandle),
}

impl Parse for ArrayType {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(ArrayType, IndexStr<'b>)> {
        log_parse!("ArrayType", input);

        let tail = try!(consume(b"A", input));

        if let Ok((num, tail)) = parse_number(10, false, tail) {
            debug_assert!(num >= 0);
            let tail = try!(consume(b"_", tail));
            let (ty, tail) = try!(TypeHandle::parse(subs, tail));
            return Ok((ArrayType::DimensionNumber(num as _, ty), tail));
        }

        if let Ok((expr, tail)) = Expression::parse(subs, tail) {
            let tail = try!(consume(b"_", tail));
            let (ty, tail) = try!(TypeHandle::parse(subs, tail));
            return Ok((ArrayType::DimensionExpression(expr, ty), tail));
        }

        let tail = try!(consume(b"_", tail));
        let (ty, tail) = try!(TypeHandle::parse(subs, tail));
        Ok((ArrayType::NoDimension(ty), tail))
    }
}

/// The `<pointer-to-member-type>` production.
///
/// ```text
/// <pointer-to-member-type> ::= M <class type> <member type>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PointerToMemberType(TypeHandle, TypeHandle);

impl Parse for PointerToMemberType {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(PointerToMemberType, IndexStr<'b>)> {
        log_parse!("PointerToMemberType", input);

        let tail = try!(consume(b"M", input));
        let (ty1, tail) = try!(TypeHandle::parse(subs, tail));
        let (ty2, tail) = try!(TypeHandle::parse(subs, tail));
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
pub struct TemplateParam(usize);

impl Parse for TemplateParam {
    fn parse<'a, 'b>(_subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(TemplateParam, IndexStr<'b>)> {
        log_parse!("TemplateParam", input);

        let input = try!(consume(b"T", input));
        let (number, input) = match parse_number(10, false, input) {
            Ok((number, input)) => ((number + 1) as _, input),
            Err(_) => (0, input),
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
pub struct TemplateTemplateParam(TemplateParam);

define_handle! {
    /// A reference to a parsed `TemplateTemplateParam`.
    pub enum TemplateTemplateParamHandle
}

impl Parse for TemplateTemplateParamHandle {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(TemplateTemplateParamHandle, IndexStr<'b>)> {
        log_parse!("TemplateTemplateParamHandle", input);


        if let Ok((sub, tail)) = Substitution::parse(subs, input) {
            match sub {
                Substitution::WellKnown(component) => {
                    return Ok((TemplateTemplateParamHandle::WellKnown(component), tail));
                }
                Substitution::BackReference(idx) => {
                    // TODO: should this check if the thing at idx is a
                    // template-template-param? There could otherwise be ambiguity
                    // with <type>'s <substitution> form...
                    return Ok((TemplateTemplateParamHandle::BackReference(idx), tail));
                }
            }
        }

        let (param, tail) = try!(TemplateParam::parse(subs, input));
        let ttp = TemplateTemplateParam(param);
        let ttp = Substitutable::TemplateTemplateParam(ttp);
        let idx = subs.insert(ttp);
        let handle = TemplateTemplateParamHandle::BackReference(idx);
        Ok((handle, tail))
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
        log_parse!("FunctionParam", input);

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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplateArgs(Vec<TemplateArg>);

impl Parse for TemplateArgs {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(TemplateArgs, IndexStr<'b>)> {
        log_parse!("TemplateArgs", input);

        let tail = try!(consume(b"I", input));

        let (args, tail) = try!(one_or_more::<TemplateArg>(subs, tail));
        let tail = try!(consume(b"E", tail));
        Ok((TemplateArgs(args), tail))
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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TemplateArg {
    /// A type or template.
    Type(TypeHandle),

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
        log_parse!("TemplateArg", input);

        if let Ok(tail) = consume(b"X", input) {
            let (expr, tail) = try!(Expression::parse(subs, tail));
            let tail = try!(consume(b"E", tail));
            return Ok((TemplateArg::Expression(expr), tail));
        }

        if let Ok((expr, tail)) = ExprPrimary::parse(subs, input) {
            return Ok((TemplateArg::SimpleExpression(expr), tail));
        }

        if let Ok((ty, tail)) = TypeHandle::parse(subs, input) {
            return Ok((TemplateArg::Type(ty), tail));
        }

        let tail = try!(consume(b"J", input));
        let (args, tail) = if tail.peek() == Some(b'E') {
            (vec![], tail)
        } else {
            try!(zero_or_more::<TemplateArg>(subs, tail))
        };
        let tail = try!(consume(b"E", tail));
        Ok((TemplateArg::ArgPack(args), tail))
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
pub enum Expression {
    /// A unary operator expression.
    Unary(OperatorName, Box<Expression>),

    /// A binary operator expression.
    Binary(OperatorName, Box<Expression>, Box<Expression>),

    /// A ternary operator expression.
    Ternary(OperatorName, Box<Expression>, Box<Expression>, Box<Expression>),

    /// A prefix `++`.
    PrefixInc(Box<Expression>),

    /// A prefix `--`.
    PrefixDec(Box<Expression>),

    /// A call with functor and arguments.
    Call(Box<Expression>, Vec<Expression>),

    /// A type conversion with one argument.
    ConversionOne(TypeHandle, Box<Expression>),

    /// A type conversion with many arguments.
    ConversionMany(TypeHandle, Vec<Expression>),

    /// A type conversion with many arguments.
    ConversionBraced(TypeHandle, Vec<Expression>),

    /// A braced init list expression.
    BracedInitList(Box<Expression>),

    /// The `new` operator.
    New(Vec<Expression>, TypeHandle, Option<Initializer>),

    /// The global `::new` operator.
    GlobalNew(Vec<Expression>, TypeHandle, Option<Initializer>),

    /// The `new[]` operator.
    NewArray(Vec<Expression>, TypeHandle, Option<Initializer>),

    /// The global `::new[]` operator.
    GlobalNewArray(Vec<Expression>, TypeHandle, Option<Initializer>),

    /// The `delete` operator.
    Delete(Box<Expression>),

    /// The global `::delete` operator.
    GlobalDelete(Box<Expression>),

    /// The `delete[]` operator.
    DeleteArray(Box<Expression>),

    /// The global `::delete[]` operator.
    GlobalDeleteArray(Box<Expression>),

    /// `dynamic_cast<type> (expression)`
    DynamicCast(TypeHandle, Box<Expression>),

    /// `static_cast<type> (expression)`
    StaticCast(TypeHandle, Box<Expression>),

    /// `const_cast<type> (expression)`
    ConstCast(TypeHandle, Box<Expression>),

    /// `reinterpret_cast<type> (expression)`
    ReinterpretCast(TypeHandle, Box<Expression>),

    /// `typeid (type)`
    TypeidType(TypeHandle),

    /// `typeid (expression)`
    TypeidExpr(Box<Expression>),

    /// `sizeof (type)`
    SizeofType(TypeHandle),

    /// `sizeof (expression)`
    SizeofExpr(Box<Expression>),

    /// `alignof (type)`
    AlignofType(TypeHandle),

    /// `alignof (expression)`
    AlignofExpr(Box<Expression>),

    /// `noexcept (expression)`
    Noexcept(Box<Expression>),

    /// A named template parameter.
    TemplateParam(TemplateParam),

    /// A function parameter.
    FunctionParam(FunctionParam),

    /// `expr.name`
    Member(Box<Expression>, UnresolvedName),

    /// `expr->name`
    DerefMember(Box<Expression>, UnresolvedName),

    /// `expr.*expr`
    PointerToMember(Box<Expression>, Box<Expression>),

    /// `sizeof...(T)`, size of a template parameter pack.
    SizeofTemplatePack(TemplateParam),

    /// `sizeof...(parameter)`, size of a function parameter pack.
    SizeofFunctionPack(FunctionParam),

    /// `sizeof...(T)`, size of a captured template parameter pack from an alias
    /// template.
    SizeofCapturedTemplatePack(Vec<TemplateArg>),

    /// `expression...`, pack expansion.
    PackExpansion(Box<Expression>),

    /// `throw expression`
    Throw(Box<Expression>),

    /// `throw` with no operand
    Rethrow,

    /// `f(p)`, `N::f(p)`, `::f(p)`, freestanding dependent name (e.g., `T::x`),
    /// objectless nonstatic member reference.
    UnresolvedName(UnresolvedName),

    /// An `<expr-primary>` production.
    Primary(ExprPrimary),
}

impl Parse for Expression {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(Expression, IndexStr<'b>)> {
        log_parse!("Expression", input);

        if let Ok(tail) = consume(b"pp_", input) {
            let (expr, tail) = try!(Expression::parse(subs, tail));
            let expr = Expression::PrefixInc(Box::new(expr));
            return Ok((expr, tail));
        }

        if let Ok(tail) = consume(b"mm_", input) {
            let (expr, tail) = try!(Expression::parse(subs, tail));
            let expr = Expression::PrefixDec(Box::new(expr));
            return Ok((expr, tail));
        }

        if let Some((head, tail)) = input.try_split_at(2) {
            match head.as_ref() {
                b"cl" => {
                    let (func, tail) = try!(Expression::parse(subs, tail));
                    let (args, tail) = try!(zero_or_more::<Expression>(subs, tail));
                    let expr = Expression::Call(Box::new(func), args);
                    return Ok((expr, tail));
                }
                b"cv" => {
                    let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                    if let Ok(tail) = consume(b"_", tail) {
                        let (exprs, tail) = try!(zero_or_more::<Expression>(subs, tail));
                        let tail = try!(consume(b"E", tail));
                        let expr = Expression::ConversionMany(ty, exprs);
                        return Ok((expr, tail));
                    } else {
                        let (expr, tail) = try!(Expression::parse(subs, tail));
                        let expr = Expression::ConversionOne(ty, Box::new(expr));
                        return Ok((expr, tail));
                    }
                }
                b"tl" => {
                    let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                    let (exprs, tail) = try!(zero_or_more::<Expression>(subs, tail));
                    let expr = Expression::ConversionBraced(ty, exprs);
                    return Ok((expr, tail));
                }
                b"il" => {
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let tail = try!(consume(b"E", tail));
                    let expr = Expression::BracedInitList(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"dc" => {
                    let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let expr = Expression::DynamicCast(ty, Box::new(expr));
                    return Ok((expr, tail));
                }
                b"sc" => {
                    let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let expr = Expression::StaticCast(ty, Box::new(expr));
                    return Ok((expr, tail));
                }
                b"cc" => {
                    let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let expr = Expression::ConstCast(ty, Box::new(expr));
                    return Ok((expr, tail));
                }
                b"rc" => {
                    let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let expr = Expression::ReinterpretCast(ty, Box::new(expr));
                    return Ok((expr, tail));
                }
                b"ti" => {
                    let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                    let expr = Expression::TypeidType(ty);
                    return Ok((expr, tail));
                }
                b"te" => {
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let expr = Expression::TypeidExpr(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"st" => {
                    let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                    let expr = Expression::SizeofType(ty);
                    return Ok((expr, tail));
                }
                b"sz" => {
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let expr = Expression::SizeofExpr(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"at" => {
                    let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                    let expr = Expression::AlignofType(ty);
                    return Ok((expr, tail));
                }
                b"az" => {
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let expr = Expression::AlignofExpr(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"nx" => {
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let expr = Expression::Noexcept(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"dt" => {
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let (name, tail) = try!(UnresolvedName::parse(subs, tail));
                    let expr = Expression::Member(Box::new(expr), name);
                    return Ok((expr, tail));
                }
                b"pt" => {
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let (name, tail) = try!(UnresolvedName::parse(subs, tail));
                    let expr = Expression::DerefMember(Box::new(expr), name);
                    return Ok((expr, tail));
                }
                b"ds" => {
                    let (first, tail) = try!(Expression::parse(subs, tail));
                    let (second, tail) = try!(Expression::parse(subs, tail));
                    let expr = Expression::PointerToMember(Box::new(first),
                                                           Box::new(second));
                    return Ok((expr, tail));
                }
                b"sZ" => {
                    if let Ok((param, tail)) = TemplateParam::parse(subs, tail) {
                        let expr = Expression::SizeofTemplatePack(param);
                        return Ok((expr, tail));
                    }

                    let (param, tail) = try!(FunctionParam::parse(subs, tail));
                    let expr = Expression::SizeofFunctionPack(param);
                    return Ok((expr, tail));
                }
                b"sP" => {
                    let (args, tail) = try!(zero_or_more::<TemplateArg>(subs, tail));
                    let expr = Expression::SizeofCapturedTemplatePack(args);
                    return Ok((expr, tail));
                }
                b"sp" => {
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let expr = Expression::PackExpansion(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"tw" => {
                    let (expr, tail) = try!(Expression::parse(subs, tail));
                    let expr = Expression::Throw(Box::new(expr));
                    return Ok((expr, tail));
                }
                b"tr" => {
                    let expr = Expression::Rethrow;
                    return Ok((expr, tail));
                }
                b"gs" => {
                    return can_be_global(true, subs, tail);
                }
                _ => {}
            }
        }

        if let Ok((expr, tail)) = can_be_global(false, subs, input) {
            return Ok((expr, tail));
        }

        if let Ok((param, tail)) = TemplateParam::parse(subs, input) {
            let expr = Expression::TemplateParam(param);
            return Ok((expr, tail));
        }

        if let Ok((param, tail)) = FunctionParam::parse(subs, input) {
            let expr = Expression::FunctionParam(param);
            return Ok((expr, tail));
        }

        if let Ok((name, tail)) = UnresolvedName::parse(subs, input) {
            let expr = Expression::UnresolvedName(name);
            return Ok((expr, tail));
        }

        if let Ok((prim, tail)) = ExprPrimary::parse(subs, input) {
            let expr = Expression::Primary(prim);
            return Ok((expr, tail));
        }

        // "A production for <expression> that directly specifies an operation
        // code (e.g., for the -> operator) takes precedence over one that is
        // expressed in terms of (unary/binary/ternary) <operator-name>." So try
        // and parse unary/binary/ternary expressions last.
        //
        // TODO: Should we check if the operator matches the arity here?
        let (opname, tail) = try!(OperatorName::parse(subs, input));
        let (first, tail) = try!(Expression::parse(subs, tail));
        return if let Ok((second, tail)) = Expression::parse(subs, tail) {
            if let Ok((third, tail)) = Expression::parse(subs, tail) {
                let expr = Expression::Ternary(opname,
                                               Box::new(first),
                                               Box::new(second),
                                               Box::new(third));
                Ok((expr, tail))
            } else {
                let expr = Expression::Binary(opname, Box::new(first), Box::new(second));
                Ok((expr, tail))
            }
        } else {
            let expr = Expression::Unary(opname, Box::new(first));
            Ok((expr, tail))
        };

        // Parse the various expressions that can optionally have a leading "gs"
        // to indicate that they are in the global namespace. The input is after
        // we have already detected consumed the optional "gs" and if we did
        // find it, then `is_global` should be true.
        fn can_be_global<'a, 'b>(is_global: bool,
                                 subs: &'a mut SubstitutionTable,
                                 input: IndexStr<'b>)
                                 -> Result<(Expression, IndexStr<'b>)> {
            match input.try_split_at(2) {
                None => Err(ErrorKind::UnexpectedEnd.into()),
                Some((head, tail)) => {
                    match head.as_ref() {
                        b"nw" => {
                            let (exprs, tail) = try!(zero_or_more::<Expression>(subs,
                                                                                tail));
                            let tail = try!(consume(b"_", tail));
                            let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                            if let Ok(tail) = consume(b"E", tail) {
                                let expr = if is_global {
                                    Expression::GlobalNew(exprs, ty, None)
                                } else {
                                    Expression::New(exprs, ty, None)
                                };
                                Ok((expr, tail))
                            } else {
                                let (init, tail) = try!(Initializer::parse(subs, tail));
                                let expr = if is_global {
                                    Expression::GlobalNew(exprs, ty, Some(init))
                                } else {
                                    Expression::New(exprs, ty, Some(init))
                                };
                                Ok((expr, tail))
                            }
                        }
                        b"na" => {
                            let (exprs, tail) = try!(zero_or_more::<Expression>(subs,
                                                                                tail));
                            let tail = try!(consume(b"_", tail));
                            let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                            if let Ok(tail) = consume(b"E", tail) {
                                let expr = if is_global {
                                    Expression::GlobalNewArray(exprs, ty, None)
                                } else {
                                    Expression::NewArray(exprs, ty, None)
                                };
                                Ok((expr, tail))
                            } else {
                                let (init, tail) = try!(Initializer::parse(subs, tail));
                                let expr = if is_global {
                                    Expression::GlobalNewArray(exprs, ty, Some(init))
                                } else {
                                    Expression::NewArray(exprs, ty, Some(init))
                                };
                                Ok((expr, tail))
                            }
                        }
                        b"dl" => {
                            let (expr, tail) = try!(Expression::parse(subs, tail));
                            let expr = if is_global {
                                Expression::GlobalDelete(Box::new(expr))
                            } else {
                                Expression::Delete(Box::new(expr))
                            };
                            Ok((expr, tail))
                        }
                        b"da" => {
                            let (expr, tail) = try!(Expression::parse(subs, tail));
                            let expr = if is_global {
                                Expression::GlobalDeleteArray(Box::new(expr))
                            } else {
                                Expression::DeleteArray(Box::new(expr))
                            };
                            Ok((expr, tail))
                        }
                        _ => Err(ErrorKind::UnexpectedText.into()),
                    }
                }
            }
        }
    }
}

/// The `<unresolved-name>` production.
///
/// ```text
/// <unresolved-name> ::= [gs] <base-unresolved-name>
///                          #
///                   ::= sr <unresolved-type> <base-unresolved-name>
///                          #
///                   ::= srN <unresolved-type> <unresolved-qualifier-level>+ E <base-unresolved-name>
///                          #
///                   ::= [gs] sr <unresolved-qualifier-level>+ E <base-unresolved-name>
///                          # A::x, N::y, A<T>::z; "gs" means leading "::"
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnresolvedName {
    /// `x`
    Name(BaseUnresolvedName),

    /// `::x`
    Global(BaseUnresolvedName),

    /// `T::x`  or `decltype(p)::x` or `T::N::x` or `decltype(p)::N::x`
    Nested1(UnresolvedTypeHandle, Vec<UnresolvedQualifierLevel>, BaseUnresolvedName),

    /// `A::x` or `N::y` or `A<T>::z`
    Nested2(Vec<UnresolvedQualifierLevel>, BaseUnresolvedName),

    /// `::A::x` or `::N::y` or `::A<T>::z`
    GlobalNested2(Vec<UnresolvedQualifierLevel>, BaseUnresolvedName),
}

impl Parse for UnresolvedName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(UnresolvedName, IndexStr<'b>)> {
        log_parse!("UnresolvedName", input);

        if let Ok(tail) = consume(b"gs", input) {
            if let Ok((name, tail)) = BaseUnresolvedName::parse(subs, tail) {
                return Ok((UnresolvedName::Global(name), tail));
            }

            let tail = try!(consume(b"sr", tail));
            let (levels, tail) = try!(one_or_more::<UnresolvedQualifierLevel>(subs,
                                                                              tail));
            let tail = try!(consume(b"E", tail));
            let (name, tail) = try!(BaseUnresolvedName::parse(subs, tail));
            return Ok((UnresolvedName::GlobalNested2(levels, name), tail));
        }

        if let Ok((name, tail)) = BaseUnresolvedName::parse(subs, input) {
            return Ok((UnresolvedName::Name(name), tail));
        }

        let tail = try!(consume(b"sr", input));

        if tail.peek() == Some(b'N') {
            let (ty, tail) = try!(UnresolvedTypeHandle::parse(subs, input));
            let (levels, tail) = try!(one_or_more::<UnresolvedQualifierLevel>(subs,
                                                                              tail));
            let tail = try!(consume(b"E", tail));
            let (name, tail) = try!(BaseUnresolvedName::parse(subs, tail));
            return Ok((UnresolvedName::Nested1(ty, levels, name), tail));
        }

        let (ty, tail) = try!(UnresolvedTypeHandle::parse(subs, tail));
        let (name, tail) = try!(BaseUnresolvedName::parse(subs, tail));
        Ok((UnresolvedName::Nested1(ty, vec![], name), tail))
    }
}

/// The `<unresolved-type>` production.
///
/// ```text
/// <unresolved-type> ::= <template-param> [ <template-args> ]  # T:: or T<X,Y>::
///                   ::= <decltype>                            # decltype(p)::
///                   ::= <substitution>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnresolvedType {
    /// An unresolved template type.
    Template(TemplateParam, Option<TemplateArgs>),

    /// An unresolved `decltype`.
    Decltype(Decltype),
}

define_handle! {
    /// A reference to a parsed `<unresolved-type>` production.
    pub enum UnresolvedTypeHandle
}

impl Parse for UnresolvedTypeHandle {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(UnresolvedTypeHandle, IndexStr<'b>)> {
        log_parse!("UnresolvedTypeHandle", input);

        if let Ok((param, tail)) = TemplateParam::parse(subs, input) {
            let (args, tail) = if let Ok((args, tail)) = TemplateArgs::parse(subs,
                                                                             tail) {
                (Some(args), tail)
            } else {
                (None, tail)
            };
            let ty = UnresolvedType::Template(param, args);
            let ty = Substitutable::UnresolvedType(ty);
            let idx = subs.insert(ty);
            let handle = UnresolvedTypeHandle::BackReference(idx);
            return Ok((handle, tail));
        }

        if let Ok((decltype, tail)) = Decltype::parse(subs, input) {
            let ty = UnresolvedType::Decltype(decltype);
            let ty = Substitutable::UnresolvedType(ty);
            let idx = subs.insert(ty);
            let handle = UnresolvedTypeHandle::BackReference(idx);
            return Ok((handle, tail));
        }

        let (sub, tail) = try!(Substitution::parse(subs, input));
        match sub {
            Substitution::WellKnown(component) => {
                Ok((UnresolvedTypeHandle::WellKnown(component), tail))
            }
            Substitution::BackReference(idx) => {
                // TODO: should this check that the back reference actually
                // points to an `<unresolved-type>`?
                Ok((UnresolvedTypeHandle::BackReference(idx), tail))
            }
        }
    }
}

/// The `<unresolved-qualifier-level>` production.
///
/// ```text
/// <unresolved-qualifier-level> ::= <simple-id>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct UnresolvedQualifierLevel(SimpleId);

impl Parse for UnresolvedQualifierLevel {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(UnresolvedQualifierLevel, IndexStr<'b>)> {
        log_parse!("UnresolvedQualifierLevel", input);

        let (id, tail) = try!(SimpleId::parse(subs, input));
        Ok((UnresolvedQualifierLevel(id), tail))
    }
}

/// The `<simple-id>` production.
///
/// ```text
/// <simple-id> ::= <source-name> [ <template-args> ]
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SimpleId(SourceName, Option<TemplateArgs>);

impl Parse for SimpleId {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(SimpleId, IndexStr<'b>)> {
        log_parse!("SimpleId", input);

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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
        log_parse!("BaseUnresolvedName", input);

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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum DestructorName {
    /// A destructor for an unresolved type.
    Unresolved(UnresolvedTypeHandle),

    /// A destructor for a resolved type name.
    Name(SimpleId),
}

impl Parse for DestructorName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(DestructorName, IndexStr<'b>)> {
        log_parse!("DestructorName", input);

        if let Ok((ty, tail)) = UnresolvedTypeHandle::parse(subs, input) {
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
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ExprPrimary {
    /// A type literal.
    Literal(TypeHandle, usize, usize),

    /// An external name.
    External(MangledName),
}

impl Parse for ExprPrimary {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(ExprPrimary, IndexStr<'b>)> {
        log_parse!("ExprPrimary", input);

        let tail = try!(consume(b"L", input));

        if let Ok((ty, tail)) = TypeHandle::parse(subs, tail) {
            let start = tail.index();
            let num_bytes_in_literal = tail.as_ref()
                .iter()
                .take_while(|&&c| c != b'E')
                .count();
            let tail = tail.range_from(num_bytes_in_literal..);
            let end = tail.index();
            let tail = try!(consume(b"E", tail));
            let expr = ExprPrimary::Literal(ty, start, end);
            return Ok((expr, tail));
        }

        // TODO: apparently g++ omitted the '_' in the <mangled-name> here until
        // -fabi-version=3, so we should detect and work around that...

        let (name, tail) = try!(MangledName::parse(subs, tail));
        let expr = ExprPrimary::External(name);
        Ok((expr, tail))
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
        log_parse!("Initializer", input);

        let tail = try!(consume(b"pi", input));
        let (exprs, tail) = try!(zero_or_more::<Expression>(subs, tail));
        let tail = try!(consume(b"E", tail));
        Ok((Initializer(exprs), tail))
    }
}

/// The `<local-name>` production.
///
/// ```text
/// <local-name> := Z <function encoding> E <entity name> [<discriminator>]
///              := Z <function encoding> E s [<discriminator>]
///              := Z <function encoding> Ed [ <parameter number> ] _ <entity name>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
        log_parse!("LocalName", input);

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
        log_parse!("Discriminator", input);

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
        log_parse!("ClosureTypeName", input);

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
pub struct LambdaSig(Vec<TypeHandle>);

impl Parse for LambdaSig {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(LambdaSig, IndexStr<'b>)> {
        log_parse!("LambdaSig", input);

        let (types, tail) = if let Ok(tail) = consume(b"v", input) {
            (vec![], tail)
        } else {
            try!(one_or_more::<TypeHandle>(subs, input))
        };
        Ok((LambdaSig(types), tail))
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
        log_parse!("DataMemberPrefix", input);

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
        log_parse!("Substitution", input);

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
        log!("Found a reference to @ {}", idx);
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
///
/// The `<special-name>` production is spread in pieces through out the ABI
/// spec.
///
/// ### 5.1.4.1 Virtual Tables and RTTI
///
/// ```text
/// <special-name> ::= TV <type>    # virtual table
///                ::= TT <type>    # VTT structure (construction vtable index)
///                ::= TI <type>    # typeinfo structure
///                ::= TS <type>    # typeinfo name (null-terminated byte string)
/// ```
///
/// ### 5.1.4.2 Virtual Override Thunks
///
/// ```text
/// <special-name> ::= T <call-offset> <base encoding>
///     # base is the nominal target function of thunk
///
/// <special-name> ::= Tc <call-offset> <call-offset> <base encoding>
///     # base is the nominal target function of thunk
///     # first call-offset is 'this' adjustment
///     # second call-offset is result adjustment
/// ```
///
/// ### 5.1.4.4 Guard Variables
///
/// ```text
/// <special-name> ::= GV <object name> # Guard variable for one-time initialization
///     # No <type>
/// ```
///
/// ### 5.1.4.5 Lifetime-Extended Temporaries
///
/// ```text
/// <special-name> ::= GR <object name> _             # First temporary
/// <special-name> ::= GR <object name> <seq-id> _    # Subsequent temporaries
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SpecialName {
    /// A virtual table.
    VirtualTable(TypeHandle),

    /// A VTT structure (construction vtable index).
    Vtt(TypeHandle),

    /// A typeinfo structure.
    Typeinfo(TypeHandle),

    /// A typeinfo name (null-terminated byte string).
    TypeinfoName(TypeHandle),

    /// A virtual override thunk.
    VirtualOverrideThunk(CallOffset, Box<Encoding>),

    /// A virtual override thunk with a covariant return type.
    VirtualOverrideThunkCovariant(CallOffset, CallOffset, Box<Encoding>),

    /// An initialization guard for some static storage.
    Guard(Name),

    /// A temporary used in the initialization of a static storage and promoted
    /// to a static lifetime.
    GuardTemporary(Name, usize),
}

impl Parse for SpecialName {
    fn parse<'a, 'b>(subs: &'a mut SubstitutionTable,
                     input: IndexStr<'b>)
                     -> Result<(SpecialName, IndexStr<'b>)> {
        log_parse!("SpecialName", input);

        let (head, tail) = match input.try_split_at(2) {
            None => return Err(ErrorKind::UnexpectedEnd.into()),
            Some((head, tail)) => (head, tail),
        };

        match head.as_ref() {
            b"TV" => {
                let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                Ok((SpecialName::VirtualTable(ty), tail))
            }
            b"TT" => {
                let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                Ok((SpecialName::Vtt(ty), tail))
            }
            b"TI" => {
                let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                Ok((SpecialName::Typeinfo(ty), tail))
            }
            b"TS" => {
                let (ty, tail) = try!(TypeHandle::parse(subs, tail));
                Ok((SpecialName::TypeinfoName(ty), tail))
            }
            b"Tc" => {
                let (first, tail) = try!(CallOffset::parse(subs, tail));
                let (second, tail) = try!(CallOffset::parse(subs, tail));
                let (base, tail) = try!(Encoding::parse(subs, tail));
                Ok((SpecialName::VirtualOverrideThunkCovariant(first,
                                                               second,
                                                               Box::new(base)),
                    tail))
            }
            b"GV" => {
                let (name, tail) = try!(Name::parse(subs, tail));
                Ok((SpecialName::Guard(name), tail))
            }
            b"GR" => {
                let (name, tail) = try!(Name::parse(subs, tail));
                let (idx, tail) = if let Ok(tail) = consume(b"_", tail) {
                    (0, tail)
                } else {
                    let (idx, tail) = try!(SeqId::parse(subs, tail));
                    (idx.0 + 1, tail)
                };
                Ok((SpecialName::GuardTemporary(name, idx), tail))
            }
            _ => {
                if let Ok(tail) = consume(b"T", input) {
                    let (offset, tail) = try!(CallOffset::parse(subs, tail));
                    let (base, tail) = try!(Encoding::parse(subs, tail));
                    Ok((SpecialName::VirtualOverrideThunk(offset, Box::new(base)), tail))
                } else {
                    Err(ErrorKind::UnexpectedText.into())
                }
            }
        }
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

fn one_or_more<'a, 'b, P>(subs: &'a mut SubstitutionTable,
                          input: IndexStr<'b>)
                          -> Result<(Vec<P>, IndexStr<'b>)>
    where P: Parse
{
    let (first, mut tail) = try!(P::parse(subs, input));
    let mut results = vec![first];
    loop {
        if let Ok((parsed, tail_tail)) = P::parse(subs, tail) {
            results.push(parsed);
            tail = tail_tail;
        } else {
            return Ok((results, tail));
        }
    }
}

fn zero_or_more<'a, 'b, P>(subs: &'a mut SubstitutionTable,
                           input: IndexStr<'b>)
                           -> Result<(Vec<P>, IndexStr<'b>)>
    where P: Parse
{
    let mut tail = input;
    let mut results = vec![];
    loop {
        if let Ok((parsed, tail_tail)) = P::parse(subs, tail) {
            results.push(parsed);
            tail = tail_tail;
        } else {
            return Ok((results, tail));
        }
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
    use index_str::IndexStr;
    use std::fmt::Debug;
    use std::iter::FromIterator;
    use subs::{Substitutable, SubstitutionTable};
    use super::{ArrayType, BuiltinType, CallOffset, ClosureTypeName, CtorDtorName,
                CvQualifiers, DataMemberPrefix, Decltype, Discriminator, ExprPrimary,
                Expression, FunctionParam, Identifier, Initializer, LambdaSig, Number,
                NvOffset, OperatorName, Parse, PointerToMemberType, RefQualifier, SeqId,
                SourceName, StandardBuiltinType, Substitution, TemplateArg,
                TemplateParam, TemplateTemplateParam, TemplateTemplateParamHandle, Type,
                TypeHandle, UnnamedTypeName, UnqualifiedName, UnscopedName, VOffset,
                WellKnownComponent};

    fn assert_parse_ok<P, S1, S2, I1, I2>(production: &'static str,
                                          subs: S1,
                                          input: I1,
                                          expected: P,
                                          expected_tail: I2,
                                          expected_new_subs: S2)
        where P: Debug + Parse + PartialEq,
              S1: AsRef<[Substitutable]>,
              S2: AsRef<[Substitutable]>,
              I1: AsRef<[u8]>,
              I2: AsRef<[u8]>
    {
        let input = input.as_ref();
        let expected_tail = expected_tail.as_ref();

        let expected_subs = SubstitutionTable::from_iter(subs.as_ref()
            .iter()
            .cloned()
            .chain(expected_new_subs.as_ref().iter().cloned()));
        let mut subs = SubstitutionTable::from_iter(subs.as_ref().iter().cloned());

        match P::parse(&mut subs, IndexStr::from(input)) {
            Err(error) => {
                panic!("Parsing {:?} as {} failed: {}",
                       String::from_utf8_lossy(input),
                       production,
                       error)
            }
            Ok((value, tail)) => {
                if value != expected {
                    panic!("Parsing {:?} as {} produced {:?}, expected {:?}",
                           String::from_utf8_lossy(input),
                           production,
                           value,
                           expected);
                }
                if tail != expected_tail {
                    panic!("Parsing {:?} as {} left a tail of {:?}, expected {:?}",
                           String::from_utf8_lossy(input),
                           production,
                           tail,
                           String::from_utf8_lossy(expected_tail));
                }
                if subs != expected_subs {
                    panic!("Parsing {:?} as {} produced a substitutions table of\n\n\
                            {:#?}\n\n\
                            but we expected\n\n\
                            {:#?}",
                           String::from_utf8_lossy(input),
                           production,
                           subs,
                           expected_subs);
                }
            }
        }
    }

    fn simple_assert_parse_ok<P, I1, I2>(production: &'static str,
                                         input: I1,
                                         expected: P,
                                         expected_tail: I2)
        where P: Debug + Parse + PartialEq,
              I1: AsRef<[u8]>,
              I2: AsRef<[u8]>
    {
        assert_parse_ok::<P, _, _, _, _>(production,
                                         [],
                                         input,
                                         expected,
                                         expected_tail,
                                         []);
    }

    fn assert_parse_err<P, S, I>(production: &'static str,
                                 subs: S,
                                 input: I,
                                 expected_error_kind: ErrorKind)
        where P: Debug + Parse + PartialEq,
              S: AsRef<[Substitutable]>,
              I: AsRef<[u8]>
    {
        let input = input.as_ref();
        let mut subs = SubstitutionTable::from_iter(subs.as_ref().iter().cloned());

        match P::parse(&mut subs, IndexStr::from(input)) {
            Err(ref error) if *error.kind() == expected_error_kind => {
                return;
            }
            Err(ref error) => {
                panic!("Parsing {:?} as {} produced an error of kind {:?}, but we expected kind {:?}",
                       String::from_utf8_lossy(input),
                       production,
                       error.kind(),
                       expected_error_kind);
            }
            Ok((value, tail)) => {
                panic!("Parsing {:?} as {} produced value {:?} and tail {:?}, but we expected error kind {:?}",
                       String::from_utf8_lossy(input),
                       production,
                       value,
                       tail,
                       expected_error_kind);
            }
        }
    }

    fn simple_assert_parse_err<P, I>(production: &'static str,
                                     input: I,
                                     expected_error_kind: ErrorKind)
        where P: Debug + Parse + PartialEq,
              I: AsRef<[u8]>
    {
        assert_parse_err::<P, _, _>(production, [], input, expected_error_kind);
    }

    macro_rules! assert_parse {
        ( $production:ident {
            $( with subs $subs:expr => {
                Ok => {
                    $( $input:expr => {
                        $expected:expr ,
                        $expected_tail:expr ,
                        $expected_new_subs:expr
                    } )*
                }
                Err => {
                    $( $error_input:expr => $error_kind:expr , )*
                }
            } )*
        } ) => {
            $( $(
                assert_parse_ok::<$production, _, _, _, _>(stringify!($production),
                                                           $subs,
                                                           $input,
                                                           $expected,
                                                           $expected_tail,
                                                           $expected_new_subs);
            )* )*

            $( $(
                assert_parse_err::<$production, _, _>(stringify!($production),
                                                      $subs,
                                                      $error_input,
                                                      $error_kind);
            )* )*
        };

        ( $production:ident {
            Ok => {
                $( $input:expr => {
                    $expected:expr ,
                    $expected_tail:expr
                } )*
            }
            Err => {
                $( $error_input:expr => $error_kind:expr , )*
            }
        } ) => {
            $(
                simple_assert_parse_ok::<$production, _, _>(stringify!($production),
                                                            $input,
                                                            $expected,
                                                            $expected_tail);
            )*


            $(
                simple_assert_parse_err::<$production, _>(stringify!($production),
                                                          $error_input,
                                                          $error_kind);
            )*
        };
    }

    #[test]
    #[should_panic]
    fn parse_mangled_name() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_encoding() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_name() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_unscoped_template_name_handle() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_nested_name() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_prefix_handle() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_template_prefix_handle() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_type_handle() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_function_type() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_bare_function_type() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_decltype() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_class_enum_type() {
        unimplemented!()
    }

    #[test]
    fn parse_array_type() {
        assert_parse!(ArrayType {
            with subs [
                Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
            ] => {
                Ok => {
                    b"A10_S_..." => {
                        ArrayType::DimensionNumber(10, TypeHandle::BackReference(0)),
                        b"...",
                        []
                    }
                    b"A10_Sb..." => {
                        ArrayType::DimensionNumber(10,
                                                   TypeHandle::WellKnown(
                                                       WellKnownComponent::StdString1)),
                        b"...",
                        []
                    }
                    b"Atr_S_..." => {
                        ArrayType::DimensionExpression(Expression::Rethrow,
                                                       TypeHandle::BackReference(0)),
                        b"...",
                        []
                    }
                    b"A_S_..." => {
                        ArrayType::NoDimension(TypeHandle::BackReference(0)),
                        b"...",
                        []
                    }
                }
                Err => {
                    b"A10_" => ErrorKind::UnexpectedEnd,
                    b"A10" => ErrorKind::UnexpectedEnd,
                    b"A" => ErrorKind::UnexpectedEnd,
                    b"" => ErrorKind::UnexpectedEnd,
                    b"A10_..." => ErrorKind::UnexpectedText,
                    b"A10..." => ErrorKind::UnexpectedText,
                    b"A..." => ErrorKind::UnexpectedText,
                    b"..." => ErrorKind::UnexpectedText,
                }
            }
        });
    }

    #[test]
    fn parse_pointer_to_member_type() {
        assert_parse!(PointerToMemberType {
            with subs [
                Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
            ] => {
                Ok => {
                    b"MS_S_..." => {
                        PointerToMemberType(TypeHandle::BackReference(0),
                                            TypeHandle::BackReference(0)),
                        b"...",
                        []
                    }
                }
                Err => {
                    b"MS_S" => ErrorKind::UnexpectedEnd,
                    b"MS_" => ErrorKind::UnexpectedEnd,
                    b"MS" => ErrorKind::UnexpectedEnd,
                    b"M" => ErrorKind::UnexpectedEnd,
                    b"" => ErrorKind::UnexpectedEnd,
                    b"MS_..." => ErrorKind::UnexpectedText,
                    b"M..." => ErrorKind::UnexpectedText,
                    b"..." => ErrorKind::UnexpectedText,
                }
            }
        });
    }

    #[test]
    fn parse_template_template_param_handle() {
        assert_parse!(TemplateTemplateParamHandle {
            with subs [
                Substitutable::TemplateTemplateParam(TemplateTemplateParam(TemplateParam(0)))
            ] => {
                Ok => {
                    b"S_..." => {
                        TemplateTemplateParamHandle::BackReference(0),
                        b"...",
                        []
                    }
                    b"T1_..." => {
                        TemplateTemplateParamHandle::BackReference(1),
                        b"...",
                        [
                            Substitutable::TemplateTemplateParam(TemplateTemplateParam(TemplateParam(2)))
                        ]
                    }
                }
                Err => {
                    b"S" => ErrorKind::UnexpectedText,
                    b"T" => ErrorKind::UnexpectedEnd,
                    b"" => ErrorKind::UnexpectedEnd,
                    b"S..." => ErrorKind::UnexpectedText,
                    b"T..." => ErrorKind::UnexpectedText,
                    b"..." => ErrorKind::UnexpectedText,
                }
            }
        });
    }

    #[test]
    #[should_panic]
    fn parse_template_args() {
        unimplemented!()
    }

    #[test]
    fn parse_template_arg() {
        assert_parse!(TemplateArg {
            with subs [
                Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
            ] => {
                Ok => {
                    b"S_..." => {
                        TemplateArg::Type(TypeHandle::BackReference(0)),
                        b"...",
                        []
                    }
                    b"XtrE..." => {
                        TemplateArg::Expression(Expression::Rethrow),
                        b"...",
                        []
                    }
                    b"LS_E..." => {
                        TemplateArg::SimpleExpression(
                            ExprPrimary::Literal(TypeHandle::BackReference(0), 3, 3)),
                        b"...",
                        []
                    }
                    b"JE..." => {
                        TemplateArg::ArgPack(vec![]),
                        b"...",
                        []
                    }
                    b"JS_XtrELS_EJEE..." => {
                        TemplateArg::ArgPack(vec![
                            TemplateArg::Type(TypeHandle::BackReference(0)),
                            TemplateArg::Expression(Expression::Rethrow),
                            TemplateArg::SimpleExpression(
                                ExprPrimary::Literal(TypeHandle::BackReference(0), 10, 10)),
                            TemplateArg::ArgPack(vec![]),
                        ]),
                        b"...",
                        []
                    }
                }
                Err => {
                    b"..." => ErrorKind::UnexpectedText,
                    b"X..." => ErrorKind::UnexpectedText,
                    b"J..." => ErrorKind::UnexpectedText,
                    b"JS_..." => ErrorKind::UnexpectedText,
                    b"JS_" => ErrorKind::UnexpectedEnd,
                    b"X" => ErrorKind::UnexpectedEnd,
                    b"J" => ErrorKind::UnexpectedEnd,
                    b"" => ErrorKind::UnexpectedEnd,
                }
            }
        });
    }

    #[test]
    #[should_panic]
    fn parse_expression() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_unresolved_name() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_unresolved_type_handle() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_unresolved_qualifier_level() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_simple_id() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_base_unresolved_name() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_destructor_name() {
        unimplemented!()
    }

    #[test]
    #[should_panic]
    fn parse_expr_primary() {
        unimplemented!()
    }

    #[test]
    fn parse_initializer() {
        assert_parse!(Initializer {
            Ok => {
                b"piE..." => {
                    Initializer(vec![]),
                    b"..."
                }
                b"pitrtrtrE..." => {
                    Initializer(vec![
                        Expression::Rethrow,
                        Expression::Rethrow,
                        Expression::Rethrow,
                    ]),
                    b"..."
                }
            }
            Err => {
                b"pirtrtrt..." => ErrorKind::UnexpectedText,
                b"pi..." => ErrorKind::UnexpectedText,
                b"..." => ErrorKind::UnexpectedText,
                b"pirt" => ErrorKind::UnexpectedText,
                b"pi" => ErrorKind::UnexpectedEnd,
                b"p" => ErrorKind::UnexpectedEnd,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    #[should_panic]
    fn parse_local_name() {
        unimplemented!()
    }

    #[test]
    fn parse_closure_type_name() {
        assert_parse!(ClosureTypeName {
            with subs [] => {
                Ok => {
                    b"UlvE_..." => {
                        ClosureTypeName(LambdaSig(vec![]), None),
                        b"...",
                        []
                    }
                    b"UlvE36_..." => {
                        ClosureTypeName(LambdaSig(vec![]), Some(36)),
                        b"...",
                        []
                    }
                }
                Err => {
                    b"UlvE36zzz" => ErrorKind::UnexpectedText,
                    b"UlvEzzz" => ErrorKind::UnexpectedText,
                    b"Ulvzzz" => ErrorKind::UnexpectedText,
                    b"zzz" => ErrorKind::UnexpectedText,
                    b"UlvE10" => ErrorKind::UnexpectedEnd,
                    b"UlvE" => ErrorKind::UnexpectedEnd,
                    b"Ulv" => ErrorKind::UnexpectedEnd,
                    b"Ul" => ErrorKind::UnexpectedEnd,
                    b"U" => ErrorKind::UnexpectedEnd,
                    b"" => ErrorKind::UnexpectedEnd,
                }
            }
        });
    }

    #[test]
    fn parse_lambda_sig() {
        assert_parse!(LambdaSig {
            with subs [
                Substitutable::Type(Type::Builtin(BuiltinType::Standard(StandardBuiltinType::Bool)))
            ] => {
                Ok => {
                    b"v..." => {
                        LambdaSig(vec![]),
                        b"...",
                        []
                    }
                    b"S_S_S_..." => {
                        LambdaSig(vec![
                            TypeHandle::BackReference(0),
                            TypeHandle::BackReference(0),
                            TypeHandle::BackReference(0),
                        ]),
                        b"...",
                        []
                    }
                }
                Err => {
                    b"..." => ErrorKind::UnexpectedText,
                    b"S" => ErrorKind::UnexpectedEnd,
                    b"" => ErrorKind::UnexpectedEnd,
                }
            }
        });
    }

    #[test]
    fn parse_substitution() {
        assert_parse!(Substitution {
            with subs [
                Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow))),
                Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow))),
                Substitutable::Type(Type::Decltype(Decltype::Expression(Expression::Rethrow)))
            ] => {
                Ok => {
                    b"S_..." => {
                        Substitution::BackReference(0),
                        b"...",
                        []
                    }
                    b"S1_..." => {
                        Substitution::BackReference(2),
                        b"...",
                        []
                    }
                    b"St..." => {
                        Substitution::WellKnown(WellKnownComponent::Std),
                        b"...",
                        []
                    }
                    b"Sa..." => {
                        Substitution::WellKnown(WellKnownComponent::StdAllocator),
                        b"...",
                        []
                    }
                    b"Sb..." => {
                        Substitution::WellKnown(WellKnownComponent::StdString1),
                        b"...",
                        []
                    }
                    b"Ss..." => {
                        Substitution::WellKnown(WellKnownComponent::StdString2),
                        b"...",
                        []
                    }
                    b"Si..." => {
                        Substitution::WellKnown(WellKnownComponent::StdIstream),
                        b"...",
                        []
                    }
                    b"So..." => {
                        Substitution::WellKnown(WellKnownComponent::StdOstream),
                        b"...",
                        []
                    }
                    b"Sd..." => {
                        Substitution::WellKnown(WellKnownComponent::StdIostream),
                        b"...",
                        []
                    }
                }
                Err => {
                    b"S999_" => ErrorKind::BadBackReference,
                    b"Sz" => ErrorKind::UnexpectedText,
                    b"zzz" => ErrorKind::UnexpectedText,
                    b"S1" => ErrorKind::UnexpectedEnd,
                    b"S" => ErrorKind::UnexpectedEnd,
                    b"" => ErrorKind::UnexpectedEnd,
                }
            }
        });
    }

    #[test]
    #[should_panic]
    fn parse_special_name() {
        unimplemented!()
    }

    #[test]
    fn parse_function_param() {
        assert_parse!(FunctionParam {
            Ok => {
                b"fpK_..." => {
                    FunctionParam(0,
                                  CvQualifiers {
                                      restrict: false,
                                      volatile: false,
                                      const_: true,
                                  },
                                  None),
                    b"..."
                }
                b"fL1pK_..." => {
                    FunctionParam(1,
                                  CvQualifiers {
                                      restrict: false,
                                      volatile: false,
                                      const_: true,
                                  },
                                  None),
                    b"..."
                }
                b"fpK3_..." => {
                    FunctionParam(0,
                                  CvQualifiers {
                                      restrict: false,
                                      volatile: false,
                                      const_: true,
                                  },
                                  Some(3)),
                    b"..."
                }
                b"fL1pK4_..." => {
                    FunctionParam(1,
                                  CvQualifiers {
                                      restrict: false,
                                      volatile: false,
                                      const_: true,
                                  },
                                  Some(4)),
                    b"..."
                }
            }
            Err => {
                b"fz" => ErrorKind::UnexpectedText,
                b"fLp_" => ErrorKind::UnexpectedText,
                b"fpL_" => ErrorKind::UnexpectedText,
                b"fL1pK4z" => ErrorKind::UnexpectedText,
                b"fL1pK4" => ErrorKind::UnexpectedEnd,
                b"fL1p" => ErrorKind::UnexpectedEnd,
                b"fL1" => ErrorKind::UnexpectedEnd,
                b"fL" => ErrorKind::UnexpectedEnd,
                b"f" => ErrorKind::UnexpectedEnd,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_discriminator() {
        assert_parse!(Discriminator {
            Ok => {
                b"_0..." => {
                    Discriminator(0),
                    b"..."
                }
                b"_9..." => {
                    Discriminator(9),
                    b"..."
                }
                b"__99_..." => {
                    Discriminator(99),
                    b"..."
                }
            }
            Err => {
                b"_n1" => ErrorKind::UnexpectedText,
                b"__99..." => ErrorKind::UnexpectedText,
                b"__99" => ErrorKind::UnexpectedEnd,
                b"..." => ErrorKind::UnexpectedText,
            }
        });
    }

    #[test]
    fn parse_data_member_prefix() {
        assert_parse!(DataMemberPrefix {
            Ok => {
                b"3fooM..." => {
                    DataMemberPrefix(SourceName(Identifier {
                        start: 1,
                        end: 4,
                    })),
                    b"..."
                }
            }
            Err => {
                b"zzz" => ErrorKind::UnexpectedText,
                b"1" => ErrorKind::UnexpectedEnd,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_ref_qualifier() {
        assert_parse!(RefQualifier {
            Ok => {
                b"R..." => {
                    RefQualifier::LValueRef,
                    b"..."
                }
                b"O..." => {
                    RefQualifier::RValueRef,
                    b"..."
                }
            }
            Err => {
                b"..." => ErrorKind::UnexpectedText,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_cv_qualifiers() {
        assert_parse!(CvQualifiers {
            Ok => {
                b"" => {
                    CvQualifiers { restrict: false, volatile: false, const_: false },
                    b""
                }
                b"..." => {
                    CvQualifiers { restrict: false, volatile: false, const_: false },
                    b"..."
                }
                b"r..." => {
                    CvQualifiers { restrict: true, volatile: false, const_: false },
                    b"..."
                }
                b"rV..." => {
                    CvQualifiers { restrict: true, volatile: true, const_: false },
                    b"..."
                }
                b"rVK..." => {
                    CvQualifiers { restrict: true, volatile: true, const_: true },
                    b"..."
                }
                b"V" => {
                    CvQualifiers { restrict: false, volatile: true, const_: false },
                    b""
                }
                b"VK" => {
                    CvQualifiers { restrict: false, volatile: true, const_: true },
                    b""
                }
                b"K..." => {
                    CvQualifiers { restrict: false, volatile: false, const_: true },
                    b"..."
                }
            }
            Err => {
                // None.
            }
        });
    }

    #[test]
    fn parse_builtin_type() {
        assert_parse!(BuiltinType {
            Ok => {
                b"c..." => {
                    BuiltinType::Standard(StandardBuiltinType::Char),
                    b"..."
                }
                b"c" => {
                    BuiltinType::Standard(StandardBuiltinType::Char),
                    b""
                }
                b"u3abc..." => {
                    BuiltinType::Extension(SourceName(Identifier {
                        start: 2,
                        end: 5,
                    })),
                    b"..."
                }
            }
            Err => {
                b"." => ErrorKind::UnexpectedText,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_template_param() {
        assert_parse!(TemplateParam {
            Ok => {
                b"T_..." => {
                    TemplateParam(0),
                    b"..."
                }
                b"T3_..." => {
                    TemplateParam(4),
                    b"..."
                }
            }
            Err => {
                b"wtf" => ErrorKind::UnexpectedText,
                b"Twtf" => ErrorKind::UnexpectedText,
                b"T3wtf" => ErrorKind::UnexpectedText,
                b"T" => ErrorKind::UnexpectedEnd,
                b"T3" => ErrorKind::UnexpectedEnd,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_unscoped_name() {
        assert_parse!(UnscopedName {
            Ok => {
                b"St5hello..." => {
                    UnscopedName::Std(UnqualifiedName::Source(SourceName(Identifier {
                        start: 3,
                        end: 8,
                    }))),
                    b"..."
                }
                b"5hello..." => {
                    UnscopedName::Unqualified(UnqualifiedName::Source(SourceName(Identifier {
                        start: 1,
                        end: 6,
                    }))),
                    b"..."
                }
            }
            Err => {
                b"St..." => ErrorKind::UnexpectedText,
                b"..." => ErrorKind::UnexpectedText,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_unqualified_name() {
        assert_parse!(UnqualifiedName {
            Ok => {
                b"qu.." => {
                    UnqualifiedName::Operator(OperatorName::Question),
                    b".."
                }
                b"C1.." => {
                    UnqualifiedName::CtorDtor(CtorDtorName::CompleteConstructor),
                    b".."
                }
                b"10abcdefghij..." => {
                    UnqualifiedName::Source(SourceName(Identifier {
                        start: 2,
                        end: 12,
                    })),
                    b"..."
                }
                b"Ut5_..." => {
                    UnqualifiedName::UnnamedType(UnnamedTypeName(Some(5))),
                    b"..."
                }
            }
            Err => {
                b"zzz" => ErrorKind::UnexpectedText,
                b"C" => ErrorKind::UnexpectedEnd,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_unnamed_type_name() {
        assert_parse!(UnnamedTypeName {
            Ok => {
                b"Ut_abc" => {
                    UnnamedTypeName(None),
                    b"abc"
                }
                b"Ut42_abc" => {
                    UnnamedTypeName(Some(42)),
                    b"abc"
                }
                b"Ut42_" => {
                    UnnamedTypeName(Some(42)),
                    b""
                }
            }
            Err => {
                b"ut_" => ErrorKind::UnexpectedText,
                b"u" => ErrorKind::UnexpectedEnd,
                b"Ut" => ErrorKind::UnexpectedEnd,
                b"Ut._" => ErrorKind::UnexpectedText,
                b"Ut42" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_identifier() {
        assert_parse!(Identifier {
            Ok => {
                b"1abc" => {
                    Identifier { start: 0, end: 4 },
                    b""
                }
                b"_Az1..." => {
                    Identifier { start: 0, end: 4 },
                    b"..."
                }
            }
            Err => {
                b"..." => ErrorKind::UnexpectedText,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_source_name() {
        assert_parse!(SourceName {
            Ok => {
                b"1abc" => {
                    SourceName(Identifier { start: 1, end: 2 }),
                    b"bc"
                }
                b"10abcdefghijklm" => {
                    SourceName(Identifier { start: 2, end: 12 }),
                    b"klm"
                }
            }
            Err => {
                b"0abc" => ErrorKind::UnexpectedText,
                b"n1abc" => ErrorKind::UnexpectedText,
                b"10abcdef" => ErrorKind::UnexpectedEnd,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_number() {
        assert_parse!(Number {
            Ok => {
                b"n2n3" => {
                    -2,
                    b"n3"
                }
                b"12345abcdef" => {
                    12345,
                    b"abcdef"
                }
                b"0abcdef" => {
                    0,
                    b"abcdef"
                }
                b"42" => {
                    42,
                    b""
                }
            }
            Err => {
                b"001" => ErrorKind::UnexpectedText,
                b"wutang" => ErrorKind::UnexpectedText,
                b"n" => ErrorKind::UnexpectedEnd,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_call_offset() {
        assert_parse!(CallOffset {
            Ok => {
                b"hn42_..." => {
                    CallOffset::NonVirtual(NvOffset(-42)),
                    b"..."
                }
                b"vn42_36_..." => {
                    CallOffset::Virtual(VOffset(-42, 36)),
                    b"..."
                }
            }
            Err => {
                b"h1..." => ErrorKind::UnexpectedText,
                b"v1_1..." => ErrorKind::UnexpectedText,
                b"hh" => ErrorKind::UnexpectedText,
                b"vv" => ErrorKind::UnexpectedText,
                b"z" => ErrorKind::UnexpectedText,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_v_offset() {
        assert_parse!(VOffset {
            Ok => {
                b"n2_n3abcdef" => {
                    VOffset(-2, -3),
                    b"abcdef"
                }
                b"12345_12345abcdef" => {
                    VOffset(12345, 12345),
                    b"abcdef"
                }
                b"0_0abcdef" => {
                    VOffset(0, 0),
                    b"abcdef"
                }
                b"42_n3" => {
                    VOffset(42, -3),
                    b""
                }
            }
            Err => {
                b"001" => ErrorKind::UnexpectedText,
                b"1_001" => ErrorKind::UnexpectedText,
                b"wutang" => ErrorKind::UnexpectedText,
                b"n_" => ErrorKind::UnexpectedText,
                b"1_n" => ErrorKind::UnexpectedEnd,
                b"1_" => ErrorKind::UnexpectedEnd,
                b"n" => ErrorKind::UnexpectedEnd,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_nv_offset() {
        assert_parse!(NvOffset {
            Ok => {
                b"n2n3" => {
                    NvOffset(-2),
                    b"n3"
                }
                b"12345abcdef" => {
                    NvOffset(12345),
                    b"abcdef"
                }
                b"0abcdef" => {
                    NvOffset(0),
                    b"abcdef"
                }
                b"42" => {
                    NvOffset(42),
                    b""
                }
            }
            Err => {
                b"001" => ErrorKind::UnexpectedText,
                b"wutang" => ErrorKind::UnexpectedText,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_seq_id() {
        assert_parse!(SeqId {
            Ok => {
                b"1_" => {
                    SeqId(1),
                    b"_"
                }
                b"42" => {
                    SeqId(146),
                    b""
                }
                b"ABCabc" => {
                    SeqId(13368),
                    b"abc"
                }
            }
            Err => {
                b"abc" => ErrorKind::UnexpectedText,
                b"001" => ErrorKind::UnexpectedText,
                b"wutang" => ErrorKind::UnexpectedText,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_ctor_dtor_name() {
        assert_parse!(CtorDtorName {
            Ok => {
                b"D0" => {
                    CtorDtorName::DeletingDestructor,
                    b""
                }
                b"C101" => {
                    CtorDtorName::CompleteConstructor,
                    b"01"
                }
            }
            Err => {
                b"gayagaya" => ErrorKind::UnexpectedText,
                b"C" => ErrorKind::UnexpectedEnd,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }

    #[test]
    fn parse_operator_name() {
        assert_parse!(OperatorName {
            Ok => {
                b"qu" => {
                    OperatorName::Question,
                    b""
                }
                b"quokka" => {
                    OperatorName::Question,
                    b"okka"
                }
            }
            Err => {
                b"bu-buuuu" => ErrorKind::UnexpectedText,
                b"q" => ErrorKind::UnexpectedEnd,
                b"" => ErrorKind::UnexpectedEnd,
            }
        });
    }
}
