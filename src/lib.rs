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
pub type OwnedSymbol = Symbol<String>;

/// TODO FITZGEN
pub type BorrowedSymbol<'a> = Symbol<&'a str>;

/// TODO FITZGEN
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Symbol<T> {
    raw: T,
    parsed: MangledName,
}

impl<T> Symbol<T>
    where T: AsRef<str>
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
            "nw" => OperatorName::New,
	    "na" => OperatorName::NewArray,
	    "dl" => OperatorName::Delete,
	    "da" => OperatorName::DeleteArray,
	    "ps" => OperatorName::UnaryPlus,
	    "ng" => OperatorName::Neg,
	    "ad" => OperatorName::AddressOf,
	    "de" => OperatorName::Deref,
	    "co" => OperatorName::BitNot,
	    "pl" => OperatorName::Add,
	    "mi" => OperatorName::Sub,
	    "ml" => OperatorName::Mul,
	    "dv" => OperatorName::Div,
	    "rm" => OperatorName::Rem,
	    "an" => OperatorName::BitAnd,
	    "or" => OperatorName::BitOr,
	    "eo" => OperatorName::BitXor,
	    "aS" => OperatorName::Assign,
	    "pL" => OperatorName::AddAssign,
	    "mI" => OperatorName::SubAssign,
	    "mL" => OperatorName::MulAssign,
	    "dV" => OperatorName::DivAssign,
	    "rM" => OperatorName::RemAssign,
	    "aN" => OperatorName::BitAndAssign,
	    "oR" => OperatorName::BitOrAssign,
	    "eO" => OperatorName::BitXorAssign,
	    "ls" => OperatorName::Shl,
	    "rs" => OperatorName::Shr,
	    "lS" => OperatorName::ShlAssign,
	    "rS" => OperatorName::ShrAssign,
	    "eq" => OperatorName::Eq,
	    "ne" => OperatorName::Ne,
	    "lt" => OperatorName::Less,
	    "gt" => OperatorName::Greater,
	    "le" => OperatorName::LessEq,
	    "ge" => OperatorName::GreaterEq,
	    "nt" => OperatorName::Not,
	    "aa" => OperatorName::LogicalAnd,
	    "oo" => OperatorName::LogicalOr,
	    "pp" => OperatorName::PostInc,
	    "mm" => OperatorName::PostDec,
	    "cm" => OperatorName::Comma,
	    "pm" => OperatorName::DerefMemberPtr,
	    "pt" => OperatorName::DerefMember,
	    "cl" => OperatorName::Call,
	    "ix" => OperatorName::Index,
	    "qu" => OperatorName::Question,
            _ => {
                return Err(ErrorKind::UnexpectedEnd.into());
            }
        };
        Ok((name, tail))
    }
}

#[cfg(test)]
mod tests {
    use super::OperatorName;
    use index_str::IndexStr;

    #[test]
    fn parse_operator_name() {
        assert_parse!(OperatorName: "quokka" => Ok(OperatorName::Question, "okka"));
    }
}
