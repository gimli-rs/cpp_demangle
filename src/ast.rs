//! Abstract syntax tree types for mangled symbols.

use error::{ErrorKind, Result};
use index_str::IndexStr;
use std::fmt;

/// A trait for anything that can be parsed from an `IndexStr` and return a
/// `Result` of the parsed `Self::Output` value and the rest of the `IndexStr`
/// input that has not been consumed in parsing the `Self::Output` value.
trait Parse {
    /// The result of parsing.
    type Output: Sized;

    /// Parse the `Self::Output` value from `input`.
    fn parse(input: IndexStr) -> Result<(Self::Output, IndexStr)>;
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
            type Output = Self;

            fn parse(input: IndexStr) -> Result<($typename, IndexStr)> {
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

                if input.len() == 0 || found_prefix {
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
pub struct MangledName(usize, Encoding);

impl Parse for MangledName {
    type Output = Self;

    fn parse(_input: IndexStr) -> Result<(MangledName, IndexStr)> {
        unimplemented!()
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

/// The `<name>` production.
///
/// ```text
/// <name> ::= <nested-name>
///        ::= <unscoped-name>
///        ::= <unscoped-template-name> <template-args>
///        ::= <local-name>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Name {
    /// A nested name
    Nested(NestedName),

    /// An unscoped name.
    Unscoped(UnscopedName),

    /// An unscoped template.
    UnscopedTemplate(UnscopedTemplateName, TemplateArgs),

    /// A local name.
    Local(LocalName),
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

/// The `<unscoped-template-name>` production.
///
/// ```text
/// <unscoped-template-name> ::= <unscoped-name>
///                          ::= <substitution>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnscopedTemplateName {
    /// An unscoped name.
    Unscoped(UnscopedName),

    /// A substitution.
    Substitution(Substitution),
}

/// The `<nested-name>` production.
///
/// ```text
/// <nested-name> ::= N [<CV-qualifiers>] [<ref-qualifier>] <prefix> <unqualified-name> E
///               ::= N [<CV-qualifiers>] [<ref-qualifier>] <template-prefix> <template-args> E
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum NestedName {
    /// An unquaified name.
    Unqualified(CvQualifiers, RefQualifier, Prefix, UnqualifiedName),

    /// A template name.
    Template(CvQualifiers, RefQualifier, TemplatePrefix, TemplateArgs),
}

/// The `<prefix>` production.
///
/// Note that it has been refactored and split into `Prefix` and `PrefixTail` to
/// remove the left-recursion.
///
/// ```text
/// <prefix> ::= <unqualified-name>                 # global class or namespace
///          ::= <prefix> <unqualified-name>        # nested class or namespace
///          ::= <template-prefix> <template-args>  # class template specialization
///          ::= <template-param>                   # template type parameter
///          ::= <decltype>                         # decltype qualifier
///          ::= <prefix> <data-member-prefix>      # initializer of a data member
///          ::= <substitution>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Prefix {
    /// An unqualified name and optionally more prefix.
    Unqualified(UnqualifiedName, Option<PrefixTail>),
    /// A template and optionally more prefix.
    Template(TemplatePrefix, TemplateArgs, Option<PrefixTail>),
    /// A template parameter and optionally more prefix.
    TemplateParam(TemplateParam, Option<PrefixTail>),
    /// A `decltype` and optionally more prefix.
    Decltype(Decltype, Option<PrefixTail>),
}

/// The second half of the <prefix> production with left-recursion factored out.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PrefixTail {
    /// An unqualified name and optionally more prefix.
    Unqualified(UnqualifiedName, Option<Box<PrefixTail>>),
    /// A data member and optionally more prefix.
    DataMember(DataMemberPrefix, Option<Box<PrefixTail>>),
}

/// The `<data-member-prefix>` production.
///
/// ```text
/// <data-member-prefix> := <member source-name> M
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct DataMemberPrefix(SourceName);

/// The `<decltype>` production.
///
/// ```text
/// <decltype> ::= Dt <expression> E  # decltype of an id-expression or class member access (C++0x)
///            ::= DT <expression> E  # decltype of an expression (C++0x)
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Decltype;

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
///
/// TODO FITZGEN: support the other substitution forms
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Substitution(Option<SeqId>);

/// The `<bare-function-type>` production.
///
/// ```text
/// <bare-function-type> ::= <signature type>+
///      # types are possible return type, then parameter types
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct BareFunctionType(Vec<Type>);

/// A <seq-id> production encoding a base-36 positive number.
///
/// ```text
/// <seq-id> ::= <0-9A-Z>+
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SeqId(usize);

impl Parse for SeqId {
    type Output = Self;

    fn parse(input: IndexStr) -> Result<(SeqId, IndexStr)> {
        parse_number(36, false, input).map(|(num, tail)| (SeqId(num as _), tail))
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
///        ::= <substitution>                           # See Compression below
///        ::= <CV-qualifiers> <type>
///        ::= P <type>                                 # pointer-to
///        ::= R <type>                                 # reference-to
///        ::= O <type>                                 # rvalue reference-to (C++0x)
///        ::= C <type>                                 # complex pair (C 2000)
///        ::= G <type>                                 # imaginary (C 2000)
///        ::= U <source-name> [<template-args>] <type> # vendor extended type qualifier
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Type;

/// The `<special-name>` production.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SpecialName;

/// The `<template-args>` production.
///
/// ```text
/// <template-args> ::= I <template-arg>+ E
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplateArgs(Vec<TemplateArg>);

/// A <template-arg> production.
///
/// ```text
/// <template-arg> ::= <type>                # type or template
///                ::= X <expression> E      # expression
///                ::= <expr-primary>        # simple expressions
///                ::= J <template-arg>* E   # argument pack
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplateArg;

/// The `<local-name>` production.
///
/// ```text
/// <local-name> := Z <function encoding> E <entity name> [<discriminator>]
///              := Z <function encoding> E s [<discriminator>]
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LocalName;

/// The `<discriminator>` production.
///
/// ```text
/// <discriminator> := _ <non-negative number>      # when number < 10
///                 := __ <non-negative number> _   # when number >= 10
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Discriminator(usize);

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
    type Output = Self;

    fn parse(input: IndexStr) -> Result<(UnqualifiedName, IndexStr)> {
        if let Ok((op, tail)) = OperatorName::parse(input) {
            return Ok((UnqualifiedName::Operator(op), tail));
        }

        if let Ok((ctor_dtor, tail)) = CtorDtorName::parse(input) {
            return Ok((UnqualifiedName::CtorDtor(ctor_dtor), tail));
        }

        if let Ok((source, tail)) = SourceName::parse(input) {
            return Ok((UnqualifiedName::Source(source), tail));
        }

        UnnamedTypeName::parse(input)
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
    type Output = Self;

    fn parse(input: IndexStr) -> Result<(SourceName, IndexStr)> {
        let (source_name_len, input) = try!(parse_number(10, false, input));
        debug_assert!(source_name_len >= 0);
        if source_name_len == 0 {
            return Err(ErrorKind::UnexpectedText.into());
        }

        let (head, tail) = match input.try_split_at(source_name_len as _) {
            Some((head, tail)) => (head, tail),
            None => return Err(ErrorKind::UnexpectedEnd.into()),
        };

        let (identifier, empty) = try!(Identifier::parse(head));
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
    type Output = Self;

    fn parse(input: IndexStr) -> Result<(Identifier, IndexStr)> {
        if input.len() == 0 {
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

/// Expect and consume the given byte str, and return the advanced `IndexStr` if
/// we saw the expectation. Otherwise return an error of kind
/// `ErrorKind::UnexpectedText` if the input doesn't match, or
/// `ErrorKind::UnexpectedEnd` if it isn't long enough.
fn consume<'a>(expected: &[u8], input: IndexStr<'a>) -> Result<IndexStr<'a>> {
    match input.try_split_at(expected.len()) {
        Some((head, tail)) if head == expected => Ok(tail),
        Some(_) => Err(ErrorKind::UnexpectedText.into()),
        None => Err(ErrorKind::UnexpectedEnd.into()),
    }
}

/// The `<unnamed-type-name>` production.
///
/// ```text
/// <unnamed-type-name> ::= Ut [ <nonnegative number> ] _
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct UnnamedTypeName(Option<usize>);

impl Parse for UnnamedTypeName {
    type Output = Self;

    fn parse(input: IndexStr) -> Result<(UnnamedTypeName, IndexStr)> {
        let input = try!(consume(b"Ut", input));
        let (number, input) = match parse_number(10, false, input) {
            Ok((number, input)) => (Some(number as _), input),
            Err(_) => (None, input),
        };
        let input = try!(consume(b"_", input));
        Ok((UnnamedTypeName(number), input))
    }
}

/// The `<CV-qualifiers>` production.
///
/// ```text
/// <CV-qualifiers> ::= [r] [V] [K]   # restrict (C99), volatile, const
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct CvQualifiers;

/// A <ref-qualifier> production.
///
/// ```text
/// <ref-qualifier> ::= R   # & ref-qualifier
///                 ::= O   # && ref-qualifier
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct RefQualifier;

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
    type Output = Self;

    fn parse(input: IndexStr) -> Result<(BuiltinType, IndexStr)> {
        if let Ok((ty, tail)) = StandardBuiltinType::parse(input) {
            return Ok((BuiltinType::Standard(ty), tail));
        }

        let tail = try!(consume(b"u", input));
        let (name, tail) = try!(SourceName::parse(tail));
        Ok((BuiltinType::Extension(name), tail))
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

/// The `<template-param>` production.
///
/// ```text
/// <template-param> ::= T_ # first template parameter
///                  ::= T <parameter-2 non-negative number> _
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplateParam(Option<usize>);

/// The `<template-template-param>` production.
///
/// ```text
/// <template-template-param> ::= <template-param>
///                           ::= <substitution>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TemplateTemplateParam;

impl Parse for TemplateParam {
    type Output = Self;

    fn parse(input: IndexStr) -> Result<(TemplateParam, IndexStr)> {
        let input = try!(consume(b"T", input));
        let (number, input) = match parse_number(10, false, input) {
            Ok((number, input)) => (Some(number as _), input),
            Err(_) => (None, input),
        };
        let input = try!(consume(b"_", input));
        Ok((TemplateParam(number), input))
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

/// The `<number>` production.
///
/// ```text
/// <number> ::= [n] <non-negative decimal integer>
/// ```
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum Number {}

impl Parse for Number {
    type Output = isize;

    fn parse(input: IndexStr) -> Result<(isize, IndexStr)> {
        parse_number(10, true, input)
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
    type Output = Self;

    fn parse(input: IndexStr) -> Result<(CallOffset, IndexStr)> {
        if input.len() == 0 {
            return Err(ErrorKind::UnexpectedEnd.into());
        }

        if let Ok(tail) = consume(b"h", input) {
            let (offset, tail) = try!(NvOffset::parse(tail));
            let tail = try!(consume(b"_", tail));
            return Ok((CallOffset::NonVirtual(offset), tail));
        }

        if let Ok(tail) = consume(b"v", input) {
            let (offset, tail) = try!(VOffset::parse(tail));
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
    type Output = Self;

    fn parse(input: IndexStr) -> Result<(NvOffset, IndexStr)> {
        Number::parse(input).map(|(num, tail)| (NvOffset(num), tail))
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
    type Output = Self;

    fn parse(input: IndexStr) -> Result<(VOffset, IndexStr)> {
        let (offset, tail) = try!(Number::parse(input));
        let tail = try!(consume(b"_", tail));
        let (virtual_offset, tail) = try!(Number::parse(tail));
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

#[cfg(test)]
mod tests {
    use error::ErrorKind;
    use super::{BuiltinType, CallOffset, CtorDtorName, Identifier, Number, NvOffset,
                OperatorName, Parse, SeqId, SourceName, StandardBuiltinType,
                TemplateParam, UnnamedTypeName, UnqualifiedName, VOffset};

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
            match <$nonterm>::parse(::index_str::IndexStr::from(input)) {
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
            match <$nonterm>::parse(::index_str::IndexStr::from(input)) {
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
