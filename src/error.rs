//! Custom `Error` and `Result` types for the `cpp_demangle` crate.

error_chain! {
    errors {
        /// The mangled symbol ends abruptly.
        UnexpectedEnd {
            display("mangled symbol ends abruptly")
        }

        /// The mangled symbol is not well-formed.
        UnexpectedText {
            display("mangled symbol is not well-formed")
        }

        /// Found a back reference that is out-of-bounds of the substitution
        /// table.
        BadBackReference {
            display("back reference that is out-of-bounds of the substitution table")
        }
    }
}
