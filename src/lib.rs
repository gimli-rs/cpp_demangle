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


#[derive(Clone, Debug, Hash, PartialEq, Eq)]
enum OperatorName {
    /// `new`
    New,
    /// `new[]`
    NewArray,
    /// `delete`
    Delete,
    /// `delete[]`
    DeleteArray,
    /// `+` (unary)
    UnaryPlus,
    /// `-` (unary)
    Neg,
    /// `&` (unary)
    AddressOf,
    /// `*` (unary)
    Deref,
    /// `~`
    BitNot,
    /// `+`
    Add,
    /// `-`
    Sub,
    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Rem,
    /// `&`
    BitAnd,
    /// `|`
    BitOr,
    /// `^`
    BitXor,
    /// `=`
    Assign,
    /// `+=`
    AddAssign,
    /// `-=`
    SubAssign,
    /// `*=`
    MulAssign,
    /// `/=`
    DivAssign,
    /// `%=`
    RemAssign,
    /// `&=`
    BitAndAssign,
    /// `|=`
    BitOrAssign,
    /// `^=`
    BitXorAssign,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
    /// `<<=`
    ShlAssign,
    /// `>>=`
    ShrAssign,
    /// `==`
    Eq,
    /// `!=`
    Ne,
    /// `<`
    Less,
    /// `>`
    Greater,
    /// `<=`
    LessEq,
    /// `>=`
    GreaterEq,
    /// `!`
    Not,
    /// `&&`
    LogicalAnd,
    /// `||`
    LogicalOr,
    /// `++` (postfix in <expression> context)
    PostInc,
    /// `--` (postfix in <expression> context)
    PostDec,
    /// `,`
    Comma,
    /// `->*`
    DerefMemberPtr,
    /// `->`
    DerefMember,
    /// `()`
    Call,
    /// `[]`
    Index,
    /// `?:`
    Question
}

impl OperatorName {
    fn parse(input: IndexStr) -> Result<(OperatorName, IndexStr)> {
        let (head, tail) = match input.try_split_at(2) {
            Some((head, tail)) => (head, tail),
            None => {
                return Err(ErrorKind::UnexpectedEnd.into());
            }
        };
        let name = match head.as_ref() {
            b"nw" => OperatorName::New,
            b"na" => OperatorName::NewArray,
            b"dl" => OperatorName::Delete,
            b"da" => OperatorName::DeleteArray,
            b"ps" => OperatorName::UnaryPlus,
            b"ng" => OperatorName::Neg,
            b"ad" => OperatorName::AddressOf,
            b"de" => OperatorName::Deref,
            b"co" => OperatorName::BitNot,
            b"pl" => OperatorName::Add,
            b"mi" => OperatorName::Sub,
            b"ml" => OperatorName::Mul,
            b"dv" => OperatorName::Div,
            b"rm" => OperatorName::Rem,
            b"an" => OperatorName::BitAnd,
            b"or" => OperatorName::BitOr,
            b"eo" => OperatorName::BitXor,
            b"aS" => OperatorName::Assign,
            b"pL" => OperatorName::AddAssign,
            b"mI" => OperatorName::SubAssign,
            b"mL" => OperatorName::MulAssign,
            b"dV" => OperatorName::DivAssign,
            b"rM" => OperatorName::RemAssign,
            b"aN" => OperatorName::BitAndAssign,
            b"oR" => OperatorName::BitOrAssign,
            b"eO" => OperatorName::BitXorAssign,
            b"ls" => OperatorName::Shl,
            b"rs" => OperatorName::Shr,
            b"lS" => OperatorName::ShlAssign,
            b"rS" => OperatorName::ShrAssign,
            b"eq" => OperatorName::Eq,
            b"ne" => OperatorName::Ne,
            b"lt" => OperatorName::Less,
            b"gt" => OperatorName::Greater,
            b"le" => OperatorName::LessEq,
            b"ge" => OperatorName::GreaterEq,
            b"nt" => OperatorName::Not,
            b"aa" => OperatorName::LogicalAnd,
            b"oo" => OperatorName::LogicalOr,
            b"pp" => OperatorName::PostInc,
            b"mm" => OperatorName::PostDec,
            b"cm" => OperatorName::Comma,
            b"pm" => OperatorName::DerefMemberPtr,
            b"pt" => OperatorName::DerefMember,
            b"cl" => OperatorName::Call,
            b"ix" => OperatorName::Index,
            b"qu" => OperatorName::Question,
            _ => {
                return Err(ErrorKind::UnexpectedText.into());
            }
        };
        Ok((name, tail))
    }
}

#[cfg(test)]
mod tests {
    use super::OperatorName;
    use error::ErrorKind;

    #[test]
    fn parse_operator_name() {
        assert_parse!(OperatorName: b"quokka" => Ok(OperatorName::Question, b"okka"));
        assert_parse!(OperatorName: b"bu-buuu" => Err(ErrorKind::UnexpectedText));
        assert_parse!(OperatorName: b"b" => Err(ErrorKind::UnexpectedEnd));
        assert_parse!(OperatorName: b"" => Err(ErrorKind::UnexpectedEnd));
    }
}
