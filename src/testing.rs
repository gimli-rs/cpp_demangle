/// Try to parse something, and check the result. For example:
///
///     assert_parse!(OperatorName: "quokka" => Ok(OperatorName::Question, "okka"));
///
/// This asserts that calling `OperatorName::parse` on the text `"qu"`
/// succeeds, producing the value `OperatorName::Question`, and leaving the
/// unparsed text `"okka"`.
macro_rules! assert_parse {
    ($nonterm:ty : $input:expr => Ok($ex_value:expr, $ex_tail:expr)) => {
        match <$nonterm>::parse(IndexStr::from($input)) {
            Err(e) => panic!("Parsing {:?} as {} failed: {}",
                             $input, stringify!($nonterm), e),
            Ok((value, tail)) => {
                if value != $ex_value {
                    panic!("Parsing {:?} as {} produced {:?}, expected {:?}",
                           $input, stringify!($nonterm), value, $ex_value);
                }
                if tail != $ex_tail {
                    panic!("Parsing {:?} as {} left a tail of {:?}, expected {:?}",
                           $input, stringify!($nonterm), tail.as_ref(), $ex_tail);
                }
            }
        }
    }
}
