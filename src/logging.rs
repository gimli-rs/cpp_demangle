//! Debug logging infrastructure and macros.
//!
//! This module exports macros for debug logging, which are usually no-ops that
//! can be completely compiled away by LLVM, but are activated when the crate's
//! `logging` feature is enabled.
//!
//! All the logging macros format their output in an s-expression format for
//! easy navigation in editors that support parentheses-based cursor movement.

use ast;
use index_str::IndexStr;
use res;
use std::fmt;
use std::io;

#[cfg(feature = "logging")]
use std::cell::RefCell;

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

pub struct AutoLogParse;

thread_local! {
    #[cfg(feature = "logging")]
    static LOG_DEPTH: RefCell<usize> = RefCell::new(0);
}

impl AutoLogParse {
    #[cfg(feature = "logging")]
    pub fn new<'a>(production: &'static str, input: IndexStr<'a>) -> AutoLogParse {
        LOG_DEPTH.with(|depth| {
            if *depth.borrow() == 0 {
                println!("");
            }

            let indent: String = (0..*depth.borrow() * 4).map(|_| ' ').collect();
            log!("{}({} \"{}\"",
                 indent,
                 production,
                 String::from_utf8_lossy(input.as_ref()));
            *depth.borrow_mut() += 1;
        });
        AutoLogParse
    }

    #[cfg(not(feature = "logging"))]
    #[inline(always)]
    pub fn new(_: &'static str, _: IndexStr) -> AutoLogParse {
        AutoLogParse
    }
}

#[cfg(feature = "logging")]
impl Drop for AutoLogParse {
    fn drop(&mut self) {
        LOG_DEPTH.with(|depth| {
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
        let _log = $crate::logging::AutoLogParse::new($production, $input);
    }
}

pub struct AutoLogDemangle;

impl AutoLogDemangle {
    #[cfg(feature = "logging")]
    pub fn new<P, W>(production: &P, ctx: &ast::DemangleContext<W>) -> AutoLogDemangle
        where P: ?Sized + fmt::Debug,
              W: io::Write
    {
        LOG_DEPTH.with(|depth| {
            if *depth.borrow() == 0 {
                println!("");
            }

            let indent: String = (0..*depth.borrow() * 4).map(|_| ' ').collect();
            log!("{}(", indent);
            log!("{}  {:?}", indent, production);
            log!("{}  inner = {:?}", indent, ctx.inner());

            *depth.borrow_mut() += 1;
        });
        AutoLogDemangle
    }

    #[cfg(not(feature = "logging"))]
    #[inline(always)]
    pub fn new<P, W>(_: &P, _: &ast::DemangleContext<W>) -> AutoLogDemangle
        where P: ?Sized + fmt::Debug,
              W: io::Write
    {
        AutoLogDemangle
    }
}

#[cfg(feature = "logging")]
impl Drop for AutoLogDemangle {
    fn drop(&mut self) {
        LOG_DEPTH.with(|depth| {
            *depth.borrow_mut() -= 1;
            let indent: String = (0..*depth.borrow() * 4).map(|_| ' ').collect();
            log!("{})", indent);
        });
    }
}

/// Automatically log start and end demangling in an s-expression format, when
/// the `logging` feature is enabled.
macro_rules! log_demangle {
    ( $production:expr , $ctx:expr ) => {
        let _log = $crate::logging::AutoLogDemangle::new($production, $ctx);
    }
}

pub struct AutoLogResolve;

impl AutoLogResolve {
    #[cfg(feature = "logging")]
    pub fn new<P>(production: &P, scope: Option<res::ArgScopeStack>) -> AutoLogResolve
        where P: ?Sized + fmt::Debug
    {
        LOG_DEPTH.with(|depth| {
            if *depth.borrow() == 0 {
                println!("");
            }

            let indent: String = (0..*depth.borrow() * 4).map(|_| ' ').collect();
            log!("{}(", indent);
            log!("{}  {:?}", indent, production);
            log!("{}  scope = {:?}", indent, scope);

            *depth.borrow_mut() += 1;
        });
        AutoLogResolve
    }

    #[cfg(not(feature = "logging"))]
    #[inline(always)]
    pub fn new<P>(_: &P, _: Option<res::ArgScopeStack>) -> AutoLogResolve
        where P: ?Sized + fmt::Debug
    {
        AutoLogResolve
    }
}

#[cfg(feature = "logging")]
impl Drop for AutoLogResolve {
    fn drop(&mut self) {
        LOG_DEPTH.with(|depth| {
            *depth.borrow_mut() -= 1;
            let indent: String = (0..*depth.borrow() * 4).map(|_| ' ').collect();
            log!("{})", indent);
        });
    }
}

/// Automatically log start and end scope resolution in an s-expression format,
/// when the `logging` feature is enabled.
macro_rules! log_resolve {
    ( $production:expr , $scope:expr ) => {
        let _log = $crate::logging::AutoLogResolve::new($production, $scope);
    }
}
