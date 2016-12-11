//! TODO FITZGEN

#![allow(dead_code, unused_variables)]
#![deny(missing_docs)]
#![deny(missing_debug_implementations)]
// #![deny(warnings)]

// `error_chain!` can recurse deeply
#![recursion_limit = "1024"]

use std::fmt;

#[macro_use]
extern crate error_chain;

pub mod error;
mod index_str;

use error::{ErrorKind, Result};
use index_str::IndexStr;

#[macro_use]
mod testing;

/// TODO FITZGEN
pub type OwnedSymbol = Symbol<Vec<u8>>;

/// TODO FITZGEN
pub type BorrowedSymbol<'a> = Symbol<&'a [u8]>;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Symbol<T> {
    raw: T,
    parsed: MangledName,
}

impl<T> Symbol<T>
    where T: AsRef<[u8]>
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

/// Define a "vocabulary" nonterminal, something like `OperatorName` or
/// `CtorDtorName` that's basically a big list of constant strings.
/// This declares:
///
/// - the enum itself
/// - a `parse` method
/// - a `std::fmt::Display` impl
///
/// See the definition of `CTorDtorName` for an example of its use.
macro_rules! define_vocabulary {
    ( $typename:ident { $($variant:ident ( $mangled:pat, $printable:expr )),* } ) => {

        #[derive(Clone, Debug, Hash, PartialEq, Eq)]
        enum $typename {
            $(
                #[doc=$printable]
                $variant
            ),*
        }

        impl $typename {
            fn parse(input: IndexStr) -> Result<($typename, IndexStr)> {
                let (head, tail) = match input.try_split_at(2) {
                    Some((head, tail)) => (head, tail),
                    None => {
                        return Err(ErrorKind::UnexpectedEnd.into());
                    }
                };
                let name = match head.as_ref() {
                    $(
                        $mangled => $typename::$variant,
                    )*
                    _ => {
                        return Err(ErrorKind::UnexpectedText.into());
                    }
                };
                Ok((name, tail))
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

define_vocabulary! {
    CtorDtorName {
        CompleteConstructor             (b"C1", "complete object constructor"),
        BaseConstructor                 (b"C2", "base object constructor"),
        CompleteAllocatingConstructor   (b"C3", "complete object allocating constructor"),
        DeletingDestructor              (b"D0", "deleting destructor"),
        CompleteDestructor              (b"D1", "complete object destructor"),
        BaseDestructor                  (b"D2", "base object destructor")
    }
}

define_vocabulary! {
    OperatorName {
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

#[cfg(test)]
mod tests {
    use super::{CtorDtorName, OperatorName};
    use error::ErrorKind;

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
        assert_parse!(OperatorName: b"b" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(OperatorName: b"" => Err(ErrorKind::UnexpectedEnd));
    }
}
