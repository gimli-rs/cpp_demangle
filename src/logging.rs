macro_rules! log {
    ( $fmt:expr ) => {
        if cfg!(feature = "logging") {
            println!($fmt);
        }
    };
    ( $fmt:expr, $($x:tt)* ) => {
        if cfg!(feature = "logging") {
            println!($fmt, $($x)*);
        }
    }
}
