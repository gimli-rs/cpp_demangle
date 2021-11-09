#![allow(non_snake_case)]

extern crate cpp_demangle;
extern crate diff;

use std::io::Write;

use cpp_demangle::DemangleOptions;

fn assert_demangles_as(mangled: &str, expected: &str, options: Option<DemangleOptions>) {
    let sym = cpp_demangle::BorrowedSymbol::new(mangled.as_bytes())
        .expect("should parse mangled symbol ok");

    let actual = if let Some(o) = options {
        sym.demangle(&o).expect("should demangle ok")
    } else {
        let mut actual = vec![];
        write!(&mut actual, "{}", sym).expect("should demangle symbol ok");
        String::from_utf8(actual).expect("should demangle to valid utf-8")
    };

    if expected != actual {
        println!();
        println!("Diff:");
        println!("--- expected");
        print!("+++ actual");

        let mut last = None;
        for cmp in diff::chars(expected, &actual) {
            match (last, cmp.clone()) {
                (Some(diff::Result::Left(_)), diff::Result::Left(_))
                | (Some(diff::Result::Both(..)), diff::Result::Both(..))
                | (Some(diff::Result::Right(_)), diff::Result::Right(_)) => {}

                (_, diff::Result::Left(_)) => print!("\n-"),
                (_, diff::Result::Both(..)) => print!("\n "),
                (_, diff::Result::Right(_)) => print!("\n+"),
            };
            match cmp.clone() {
                diff::Result::Left(c) | diff::Result::Both(c, _) | diff::Result::Right(c) => {
                    print!("{}", c)
                }
            }
            last = Some(cmp);
        }
        println!();
    }

    assert_eq!(expected, actual);
}

fn assert_does_not_parse(s: &str) {
    if let Ok(sym) = cpp_demangle::BorrowedSymbol::new(s.as_bytes()) {
        panic!("Unexpectedly parsed '{}' as '{}'", s, sym);
    }
}

fn assert_does_not_demangle(s: &str) {
    match cpp_demangle::BorrowedSymbol::new(s.as_bytes()) {
        Ok(sym) => {
            if let Ok(d) = sym.demangle(&DemangleOptions::default()) {
                panic!("Unexpectedly demangled '{}' as '{}'", s, d);
            }
        }
        Err(e) => {
            panic!("Failed to parse '{}': {}", s, e);
        }
    }
}

macro_rules! demangles {
    ( $mangled:ident , $demangled:expr ) => {
        demangles!($mangled, stringify!($mangled), $demangled);
    };
    ( $name:ident , $mangled:expr , $demangled:expr ) => {
        #[test]
        fn $name() {
            assert_demangles_as($mangled, $demangled, None);
        }
    };
}

macro_rules! demangles_no_param_and_no_return_type {
    ( $mangled:ident , $demangled:expr ) => {
        demangles_no_param_and_no_return_type!($mangled, stringify!($mangled), $demangled);
    };
    ( $name:ident , $mangled:expr , $demangled:expr ) => {
        #[test]
        fn $name() {
            let options = DemangleOptions::new().no_params().no_return_type();
            assert_demangles_as($mangled, $demangled, Some(options));
        }
    };
}

macro_rules! demangles_simplify_template_parameters {
    ( $mangled:ident , $demangled:expr ) => {
        demangles_simplify_template_parameters!($mangled, stringify!($mangled), $demangled);
    };
    ( $name:ident , $mangled:expr , $demangled:expr ) => {
        #[test]
        fn $name() {
            let options = DemangleOptions::new().hide_expression_literal_types();
            assert_demangles_as($mangled, $demangled, Some(options));
        }
    };
}

macro_rules! demangles_no_return_type {
    ( $mangled:ident , $demangled:expr ) => {
        demangles_no_return_type!($mangled, stringify!($mangled), $demangled);
    };
    ( $name:ident , $mangled:expr , $demangled:expr ) => {
        #[test]
        fn $name() {
            let options = DemangleOptions::new().no_return_type();
            assert_demangles_as($mangled, $demangled, Some(options));
        }
    };
}

macro_rules! demangles_no_param {
    ( $mangled:ident , $demangled:expr ) => {
        demangles_no_param!($mangled, stringify!($mangled), $demangled);
    };
    ( $name:ident , $mangled:expr , $demangled:expr ) => {
        #[test]
        fn $name() {
            let options = DemangleOptions::new().no_params();
            assert_demangles_as($mangled, $demangled, Some(options));
        }
    };
}

macro_rules! does_not_parse {
    ( $name:ident ) => {
        #[test]
        fn $name() {
            assert_does_not_parse(stringify!($name));
        }
    };
}

macro_rules! does_not_demangle {
    ( $name:ident ) => {
        #[test]
        fn $name() {
            assert_does_not_demangle(stringify!($name));
        }
    };
}

// This should definitely not parse and demangle as
// `operator()(unsigned __int128, short, long double)`.
does_not_parse!(close);

// Test some potential stack-overflows due to cyclic template parameter references
does_not_demangle!(_Z1fIT_EvT_);

// A stack overflow (https://github.com/gimli-rs/cpp_demangle/pull/186)
#[test]
fn test_stackoverflow_does_not_occur_issue_186() {
    assert_does_not_demangle("__ZNSt3__18__bind_rINS_4pairINS_12basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEEE8cc_errorEEZN5stlab2v15asyncIZNSB_14serial_queue_tclIZN12_GLOBAL__N_114future_adaptorIN10redacteLib12ValueOrErrorIS7_EEZNK10cc_element17rendition_requestEmbE4$_14EEDaNS_6futureIT_EEOT0_EUlSO_E_JNSN_ISJ_EEEEESM_OSO_DpOT0_EUlSU_E_SS_JST_EEENSB_6futureINS_9result_ofIFNS_5decayISQ_E4typeEDpNS11_IT1_E4typeEEE4typeEvEESO_SR_DpOS14_EUlRST_E_JST_EEC1IS1F_JST_EvEESU_SX_");
}

demangles!(
    _ZN7mozilla6detail12ListenerImplINS_14AbstractThreadEZNS_20MediaEventSourceImplILNS_14ListenerPolicyE0EJNS_13TimedMetadataEEE15ConnectInternalIS2_NS_12MediaDecoderEMS8_FvOS5_EEENS_8EnableIfIXsr8TakeArgsIT1_EE5valueENS_18MediaEventListenerEE4TypeEPT_PT0_SD_EUlS9_E_JS5_EE17ApplyWithArgsImplISL_EENSC_IXsr8TakeArgsISH_EE5valueEvE4TypeERKSH_S9_,
    // This does not match llvm-cxxfilt
    "mozilla::EnableIf<TakeArgs<mozilla::EnableIf<TakeArgs<void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&)>::value, mozilla::MediaEventListener>::Type mozilla::MediaEventSourceImpl<(mozilla::ListenerPolicy)0, mozilla::TimedMetadata>::ConnectInternal<mozilla::AbstractThread, mozilla::MediaDecoder, void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&)>(mozilla::AbstractThread*, mozilla::MediaDecoder*, void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&))::{lambda(mozilla::TimedMetadata&&)#1}>::value, void>::Type mozilla::detail::ListenerImpl<mozilla::AbstractThread, mozilla::EnableIf<TakeArgs<void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&)>::value, mozilla::MediaEventListener>::Type mozilla::MediaEventSourceImpl<(mozilla::ListenerPolicy)0, mozilla::TimedMetadata>::ConnectInternal<mozilla::AbstractThread, mozilla::MediaDecoder, void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&)>(mozilla::AbstractThread*, mozilla::MediaDecoder*, void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&))::{lambda(mozilla::TimedMetadata&&)#1}, mozilla::TimedMetadata>::ApplyWithArgsImpl<mozilla::EnableIf<TakeArgs<void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&)>::value, mozilla::MediaEventListener>::Type mozilla::MediaEventSourceImpl<(mozilla::ListenerPolicy)0, mozilla::TimedMetadata>::ConnectInternal<mozilla::AbstractThread, mozilla::MediaDecoder, void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&)>(mozilla::AbstractThread*, mozilla::MediaDecoder*, void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&))::{lambda(mozilla::TimedMetadata&&)#1}>(mozilla::EnableIf<TakeArgs<void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&)>::value, mozilla::MediaEventListener>::Type mozilla::MediaEventSourceImpl<(mozilla::ListenerPolicy)0, mozilla::TimedMetadata>::ConnectInternal<mozilla::AbstractThread, mozilla::MediaDecoder, void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&)>(mozilla::AbstractThread*, mozilla::MediaDecoder*, void (mozilla::MediaDecoder::*)(mozilla::TimedMetadata&&))::{lambda(mozilla::TimedMetadata&&)#1} const&, mozilla::TimedMetadata&&)"
);

demangles!(
    _Z20instantiate_with_intI3FooET_IiEv,
    "Foo<int> instantiate_with_int<Foo>()"
);
demangles!(_Z3fooISt6vectorIiEEvv, "void foo<std::vector<int> >()");
demangles!(__ZN3foo3barE3quxS0_, "foo::bar(qux, qux)");
demangles!(__ZN3foo3barE3quxS_, "foo::bar(qux, foo)");

demangles!(
    _ZN4funcI2TyEEN6ResultIT_EES3_,
    "Result<Ty> func<Ty>(Result<Ty>)"
);
demangles!(_ZN4funcI2TyEEN6ResultIT_EES2_, "Result<Ty> func<Ty>(Ty)");
demangles!(
    _ZN4funcI2TyEEN6ResultIT_EES1_,
    "Result<Ty> func<Ty>(Result)"
);
demangles!(_ZN4funcI2TyEEN6ResultIT_EES0_, "Result<Ty> func<Ty>(Ty)");
demangles!(_ZN4funcI2TyEEN6ResultIT_EES_, "Result<Ty> func<Ty>(func)");

demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), Ty)"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES0_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), Ty::method)"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES1_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), Ty)"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES2_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), char const)"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES3_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), char const*)"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES4_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), void (char const*))"
);
demangles!(
    _ZN2Ty6methodIS_EEvMT_FvPKcES5_,
    "void Ty::method<Ty>(void (Ty::*)(char const*), void (Ty::*)(char const*))"
);

demangles!(_ZNK1fB5cxx11Ev, "f[abi:cxx11]() const");

demangles!(
    _ZN4base8internal14CheckedSubImplIlEENSt9enable_ifIXsr3std14numeric_limitsIT_EE10is_integerEbE4typeES3_S3_PS3_,
    "std::enable_if<std::numeric_limits<long>::is_integer, bool>::type base::internal::CheckedSubImpl<long>(long, long, long*)"
);

demangles!(
    _ZZN7mozilla12EMEDecryptor5FlushEvENUlvE_D4Ev,
    "mozilla::EMEDecryptor::Flush()::{lambda()#1}::~{lambda()#1}()"
);

demangles!(
    _ZSt4copyIPKcPcET0_T_S4_S3_,
    "char* std::copy<char const*, char*>(char const*, char const*, char*)"
);

demangles!(
    _Z9_mm_or_psDv4_fS_,
    "_mm_or_ps(float __vector(4), float __vector(4))"
);

demangles!(
    _ZN5space20templated_trampolineIPFvvEEEvT_,
    "void space::templated_trampoline<void (*)()>(void (*)())"
);

demangles!(
    _Z18convertCase_helperIN14QUnicodeTables14CasefoldTraitsEtET0_S2_,
    "unsigned short convertCase_helper<QUnicodeTables::CasefoldTraits, unsigned short>(unsigned short)"
);

demangles!(
    _ZnwmRKSt9nothrow_t,
    "operator new(unsigned long, std::nothrow_t const&)"
);

demangles!(
    _ZGRL13MozLangGroups_,
    "reference temporary #0 for MozLangGroups"
);

demangles!(_ZZ3abcvEN3defD0Ev, "abc()::def::~def()");
demangles!(
    _ZZN13CrashReporter7OOPInitEvEN17ProxyToMainThreadD0Ev,
    "CrashReporter::OOPInit()::ProxyToMainThread::~ProxyToMainThread()"
);

demangles!(_ZUlvE_, "{lambda()#1}");
demangles!(_ZZ3aaavEUlvE_, "aaa()::{lambda()#1}");
demangles!(_ZZ3aaavENUlvE_3bbbE, "aaa()::{lambda()#1}::bbb");
demangles!(_ZN3aaaUlvE_D1Ev, "aaa::{lambda()#1}::~{lambda()#1}()");

demangles!(_ZZ3aaavEN3bbbD1Ev, "aaa()::bbb::~bbb()");
demangles!(
    _ZZ3aaavENUlvE_D1Ev,
    // libiberty says "aaa()::{lambda()#1}::~aaa()" but I am pretty sure that is
    // a bug, especially given the previous demangling, which is the same but
    // with an identifier instead of a lambda. Finally, both demangle.go and my
    // OSX system `__cxa_demangle` agree with this destructor-of-the-lambda
    // interpretation.
    "aaa()::{lambda()#1}::~{lambda()#1}()"
);

demangles!(
    multiple_nested_local_names_and_operator_call_and_a_lambda_and_a_destructor,
    "_ZZZN7mozilla12MediaManager12GetUserMediaEP18nsPIDOMWindowInnerRKNS_3dom22MediaStreamConstraintsEP33nsIDOMGetUserMediaSuccessCallbackP31nsIDOMGetUserMediaErrorCallbackNS3_10CallerTypeEEN4$_30clERP8nsTArrayI6RefPtrINS_11MediaDeviceEEEENUlRPKcE_D1Ev",
    "mozilla::MediaManager::GetUserMedia(nsPIDOMWindowInner*, mozilla::dom::MediaStreamConstraints const&, nsIDOMGetUserMediaSuccessCallback*, nsIDOMGetUserMediaErrorCallback*, mozilla::dom::CallerType)::$_30::operator()(nsTArray<RefPtr<mozilla::MediaDevice> >*&)::{lambda(char const*&)#1}::~{lambda(char const*&)#1}()"
);

demangles!(_ZN11InstrumentsL8gSessionE, "Instruments::gSession");
demangles!(
    _ZTWN2js10TlsContextE,
    "TLS wrapper function for js::TlsContext"
);

demangles!(_Z3fooILb0EEvi, "void foo<false>(int)");
demangles!(_Z3fooILb1EEvi, "void foo<true>(int)");
demangles!(_Z3fooILb2EEvi, "void foo<(bool)2>(int)");
demangles!(_Z3fooILb999999EEvi, "void foo<(bool)999999>(int)");
demangles!(_Z3fooILbaaaaaaEEvi, "void foo<(bool)aaaaaa>(int)");
demangles!(
    bool_literal_with_decimal,
    "_Z3fooILb999.999EEvi",
    "void foo<(bool)999.999>(int)"
);
demangles!(_Z3fooILbn1EEvi, "void foo<(bool)-1>(int)");
demangles!(_Z3fooILbn0EEvi, "void foo<(bool)-0>(int)");

demangles!(_Z3fooILc65EEvi, "void foo<(char)65>(int)");
demangles!(_Z3fooILc48EEvi, "void foo<(char)48>(int)");
demangles!(_Z3fooILc0EEvi, "void foo<(char)0>(int)");
demangles!(_Z3fooILc999999EEvi, "void foo<(char)999999>(int)");
demangles!(_Z3fooILcaaaaaaEEvi, "void foo<(char)aaaaaa>(int)");
demangles!(
    char_literal_with_decimal,
    "_Z3fooILc999.999EEvi",
    "void foo<(char)999.999>(int)"
);
demangles!(_Z3fooILcn65EEvi, "void foo<(char)-65>(int)");
demangles!(
    char_literal_with_negative_sign,
    "_Z3fooILc-65EEvi",
    "void foo<(char)-65>(int)"
);

demangles!(_Z3fooILd65EEvi, "void foo<(double)[65]>(int)");
demangles!(_Z3fooILd48EEvi, "void foo<(double)[48]>(int)");
demangles!(_Z3fooILd0EEvi, "void foo<(double)[0]>(int)");
demangles!(_Z3fooILd999999EEvi, "void foo<(double)[999999]>(int)");
demangles!(_Z3fooILdaaaaaaEEvi, "void foo<(double)[aaaaaa]>(int)");
demangles!(
    double_literal_with_decimal,
    "_Z3fooILd999.999EEvi",
    "void foo<(double)[999.999]>(int)"
);
demangles!(_Z3fooILdn65EEvi, "void foo<(double)-[65]>(int)");
demangles!(
    double_literal_with_negative_sign,
    "_Z3fooILd-65EEvi",
    "void foo<(double)[-65]>(int)"
);

demangles!(_Z3fooILf65EEvi, "void foo<(float)[65]>(int)");
demangles!(_Z3fooILf48EEvi, "void foo<(float)[48]>(int)");
demangles!(_Z3fooILf0EEvi, "void foo<(float)[0]>(int)");
demangles!(_Z3fooILf999999EEvi, "void foo<(float)[999999]>(int)");
demangles!(_Z3fooILfaaaaaaEEvi, "void foo<(float)[aaaaaa]>(int)");
demangles!(
    float_literal_with_decimal,
    "_Z3fooILf999.999EEvi",
    "void foo<(float)[999.999]>(int)"
);
demangles!(_Z3fooILfn65EEvi, "void foo<(float)-[65]>(int)");
demangles!(
    float_literal_with_negative_sign,
    "_Z3fooILf-65EEvi",
    "void foo<(float)[-65]>(int)"
);

demangles!(_Z3fooILin1EEvv, "void foo<-1>()");
demangles!(_Z3fooILi0EEvv, "void foo<0>()");
demangles!(_Z3fooILin0EEvv, "void foo<-0>()");
demangles!(_Z3fooILi999999EEvv, "void foo<999999>()");
demangles!(_Z3fooILiaaaaaaEEvv, "void foo<aaaaaa>()");
demangles!(
    int_literal_with_decimal,
    "_Z3fooILi999.999EEvv",
    "void foo<999.999>()"
);

demangles!(_Z3abcrA_l, "abc(long restrict [])");
demangles!(_Z3abcFrA_lvE, "abc(long restrict (()) [])");
demangles!(_Z3abcFrPA_lvE, "abc(long (* restrict()) [])");
demangles!(
    _Z3abcM3defFPVPFrPivEvE,
    "abc(int* restrict (* volatile* (def::*)())())"
);
demangles!(
    _Z3abcM3defFPVPFrPA_lvEvE,
    "abc(long (* restrict (* volatile* (def::*)())()) [])"
);
demangles!(_Z3abcKFA_ivE, "abc(int (() const) [])");
demangles!(_Z3abcFFivElE, "abc(int (long)())");
demangles!(_Z3abcFPFrPivElE, "abc(int* restrict (*(long))())");
demangles!(_Z3abcKFvRSt7ostreamE, "abc(void (std::ostream&) const)");

demangles!(
    _ZL29SupportsTextureSampleCountMTLPU19objcproto9MTLDevice11objc_objectm,
    "SupportsTextureSampleCountMTL(objc_object objcproto9MTLDevice*, unsigned long)"
);

demangles!(
    _ZN3WTF8FunctionIFvvEE15CallableWrapperIZN7WebCore12CacheStorage5matchEONS_7VariantIJNS_6RefPtrINS4_12FetchRequestEEENS_6StringEEEEONS4_17CacheQueryOptionsEONS_3RefINS4_15DeferredPromiseEEEEUlvE_E4callEv,
    "WTF::Function<void ()>::CallableWrapper<WebCore::CacheStorage::match(WTF::Variant<WTF::RefPtr<WebCore::FetchRequest>, WTF::String>&&, WebCore::CacheQueryOptions&&, WTF::Ref<WebCore::DeferredPromise>&&)::{lambda()#1}>::call()"
);
demangles!(
    _ZN6WebKit25WebCacheStorageConnection17didReceiveMessageERN3IPC10ConnectionERNS1_7DecoderE,
    "WebKit::WebCacheStorageConnection::didReceiveMessage(IPC::Connection&, IPC::Decoder&)"
);
demangles!(
    _ZN3IPC10Connection15dispatchMessageESt10unique_ptrINS_7DecoderESt14default_deleteIS2_EE,
    "IPC::Connection::dispatchMessage(std::unique_ptr<IPC::Decoder, std::default_delete<IPC::Decoder> >)"
);
demangles!(
    _ZN3IPC10Connection18dispatchOneMessageEv,
    "IPC::Connection::dispatchOneMessage()"
);
demangles!(
    _ZN3WTF7RunLoop11performWorkEv,
    "WTF::RunLoop::performWork()"
);

demangles!(
    _Z4funcINS_6ObjectEENS0_IT_EEi,
    "func::Object<func::Object> func<func::Object>(int)"
);

demangles!(
    _ZN4funcINS_6ObjectEEENS0_IT_EEi,
    "func::Object<func::Object> func<func::Object>(int)"
);

demangles!(
    _ZNK7mozilla6layers19CapturedBufferState20ForEachTextureClientIZNS0_21CompositorBridgeChild21NotifyBeginAsyncPaintI6RefPtrIS1_EEEvRT_EUlS7_E_EEvS7_,
    "void mozilla::layers::CapturedBufferState::ForEachTextureClient<void mozilla::layers::CompositorBridgeChild::NotifyBeginAsyncPaint<RefPtr<mozilla::layers::CapturedBufferState> >(RefPtr<mozilla::layers::CapturedBufferState>&)::{lambda(auto:1)#1}>(void mozilla::layers::CompositorBridgeChild::NotifyBeginAsyncPaint<RefPtr<mozilla::layers::CapturedBufferState> >(RefPtr<mozilla::layers::CapturedBufferState>&)::{lambda(auto:1)#1}) const"
);

demangles!(
    _ZNSt3__116forward_as_tupleIJRKZN11tconcurrent6detail6sharedIFvvEEC1IZNS1_7yielder13await_suspendINS1_12task_promiseIvEEEEvNSt12experimental13coroutines_v116coroutine_handleIT_EEEUlvE_EEbNS_10shared_ptrINS1_17cancelation_tokenEEEOSE_PvEUlRSI_DpOT_E_EEENS_5tupleIJSP_EEESP_,
    "std::__1::tuple<tconcurrent::detail::shared<void ()>::shared<void tconcurrent::yielder::await_suspend<tconcurrent::task_promise<void> >(std::experimental::coroutines_v1::coroutine_handle<tconcurrent::task_promise<void> >)::{lambda()#1}>(std::__1::shared_ptr<tconcurrent::cancelation_token>, void tconcurrent::yielder::await_suspend<tconcurrent::task_promise<void> >(std::experimental::coroutines_v1::coroutine_handle<tconcurrent::task_promise<void> >)::{lambda()#1}&&, void*)::{lambda(tconcurrent::cancelation_token&, auto:1&&)#1} const&> std::__1::forward_as_tuple<tconcurrent::detail::shared<void ()>::shared<void tconcurrent::yielder::await_suspend<tconcurrent::task_promise<void> >(std::experimental::coroutines_v1::coroutine_handle<tconcurrent::task_promise<void> >)::{lambda()#1}>(std::__1::shared_ptr<tconcurrent::cancelation_token>, void tconcurrent::yielder::await_suspend<tconcurrent::task_promise<void> >(std::experimental::coroutines_v1::coroutine_handle<tconcurrent::task_promise<void> >)::{lambda()#1}&&, void*)::{lambda(tconcurrent::cancelation_token&, auto:1&&)#1} const&>(tconcurrent::detail::shared<void ()>::shared<void tconcurrent::yielder::await_suspend<tconcurrent::task_promise<void> >(std::experimental::coroutines_v1::coroutine_handle<tconcurrent::task_promise<void> >)::{lambda()#1}>(std::__1::shared_ptr<tconcurrent::cancelation_token>, void tconcurrent::yielder::await_suspend<tconcurrent::task_promise<void> >(std::experimental::coroutines_v1::coroutine_handle<tconcurrent::task_promise<void> >)::{lambda()#1}&&, void*)::{lambda(tconcurrent::cancelation_token&, auto:1&&)#1} const&)"
);

demangles!(
    _Z1jI1AEDTcldtfp_cvPT_EES1_,
    // TODO: libiberty formats this as
    //
    //   decltype (({parm#1}.(operator A*))()) j<A>(A)
    "decltype (({parm#1}.operator A*)()) j<A>(A)"
);

demangles!(
    _Z3MinIiiEDTqultfp_fp0_cl7forwardIT_Efp_Ecl7forwardIT0_Efp0_EEOS0_OS1_,
    "decltype (({parm#1}<{parm#2})?((forward<int>)({parm#1})) : ((forward<int>)({parm#2}))) Min<int, int>(int&&, int&&)"
);

demangles!(
    _ZN16already_AddRefedIN7mozilla6detail16RunnableFunctionIZNS0_3ipc21AsyncMinidumpAnalyzer3RunEvEUlvE_EEEC4Ev,
    "already_AddRefed<mozilla::detail::RunnableFunction<mozilla::ipc::AsyncMinidumpAnalyzer::Run()::{lambda()#1}> >::already_AddRefed()"
);

demangles!(
    _Z6IsNullIiEN1EIXsr1FIT_EE1nEE4typeES1_,
    "E<F<int>::n>::type IsNull<int>(int)"
);
demangles!(
    _Z6IsNullIiEN1EIXgssr1FIT_EE1nEE4typeEv,
    "E<::F<int>::n>::type IsNull<int>()"
);

// Test cases found via differential testing against `c++filt` with `cargo-fuzz`
// and `libFuzzer`.

demangles!(
    _Z5ccc_Z5cccmmmml,
    "ccc_Z(cccmm, unsigned long, unsigned long, long)"
);
demangles!(
    __Z3S_Z3SGffffjjjjjjjjjjzjjjjjjojjjjjjjj,
    "S_Z(SGf, float, float, float, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, ..., unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned __int128, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int, unsigned int)"
);
demangles!(
    __Z3SGfDdedddd,
    "SGf(decimal64, long double, double, double, double, double)"
);
demangles!(
    __ZN6ISiS_Z3b_dE1ES0_7__dIFFFdhl,
    "ISiS_Z::b_d(E, E, __dIFFF, double, unsigned char, long)"
);
demangles!(
    _ZN9__gnu_cxxmiIPKtPtNSt7__cxx1112basic_stringItN4base18string16_internals20string16_char_traitsESaItEEEEEDTmicldtfp_4baseEcldtfp0_4baseEERKNS_17__normal_iteratorIT_T1_EERKNSC_IT0_SE_EE,
    "decltype ((({parm#1}.base)())-(({parm#2}.base)())) __gnu_cxx::operator-<unsigned short const*, unsigned short*, std::__cxx11::basic_string<unsigned short, base::string16_internals::string16_char_traits, std::allocator<unsigned short> > >(__gnu_cxx::__normal_iterator<unsigned short const*, std::__cxx11::basic_string<unsigned short, base::string16_internals::string16_char_traits, std::allocator<unsigned short> > > const&, __gnu_cxx::__normal_iterator<unsigned short*, std::__cxx11::basic_string<unsigned short, base::string16_internals::string16_char_traits, std::allocator<unsigned short> > > const&)"
);
demangles!(
    _Z3addIidEDTplL_Z1gEfp0_ET_T0_,
    "decltype (g+{parm#2}) add<int, double>(int, double)"
);
demangles!(
    _ZNK7mozilla15NativeEventDatacvPKT_I12_GdkEventKeyEEv,
    "mozilla::NativeEventData::operator _GdkEventKey const*<_GdkEventKey>() const"
);
demangles!(
    _ZN5boost9unordered18unordered_multimapItN3xxx6xxxxxx6xxxxxx14xxxxxxxxxxxxxxENS_4hashItEESt8equal_toItESaISt4pairIKtS5_EEE7emplaceIISC_EEENS0_15iterator_detail8iteratorINS0_6detail16grouped_ptr_nodeISC_EEEEDpOT_,
    "boost::unordered::iterator_detail::iterator<boost::unordered::detail::grouped_ptr_node<std::pair<unsigned short const, xxx::xxxxxx::xxxxxx::xxxxxxxxxxxxxx> > > boost::unordered::unordered_multimap<unsigned short, xxx::xxxxxx::xxxxxx::xxxxxxxxxxxxxx, boost::hash<unsigned short>, std::equal_to<unsigned short>, std::allocator<std::pair<unsigned short const, xxx::xxxxxx::xxxxxx::xxxxxxxxxxxxxx> > >::emplace<std::pair<unsigned short const, xxx::xxxxxx::xxxxxx::xxxxxxxxxxxxxx> >(std::pair<unsigned short const, xxx::xxxxxx::xxxxxx::xxxxxxxxxxxxxx>&&)"
);
demangles!(
    _ZNSt6vectorIN3xxx6xxxxxx15xxxxxxxxxxxxxxxESaIS2_EE12emplace_backIIS2_EEEvDpOT_,
    "void std::vector<xxx::xxxxxx::xxxxxxxxxxxxxxx, std::allocator<xxx::xxxxxx::xxxxxxxxxxxxxxx> >::emplace_back<xxx::xxxxxx::xxxxxxxxxxxxxxx>(xxx::xxxxxx::xxxxxxxxxxxxxxx&&)"
);
demangles_no_param_and_no_return_type!(
    _ZN2js9LifoAlloc21newArrayUninitializedI17OffsetAndDefIndexEEPT_m,
    "js::LifoAlloc::newArrayUninitialized<OffsetAndDefIndex>"
);
demangles_no_param_and_no_return_type!(_Z4callIXadL_Z5helloiEEEvi, "call<&hello(int)>");
demangles_no_param_and_no_return_type!(_ZNK5Hello6methodEv, "Hello::method");

demangles_no_return_type!(
    _ZL15draw_quad_spansIjEviPN4glsl11vec2_scalarEtPDv16_fR7TextureiS6_RK8ClipRect,
    "draw_quad_spans<unsigned int>(int, glsl::vec2_scalar*, unsigned short, float __vector(16)*, Texture&, int, Texture&, ClipRect const&)"
);

demangles_no_return_type!(
    _ZL13draw_elementsItEviiR6BuffermR11VertexArrayR7TextureiS5_,
    "draw_elements<unsigned short>(int, int, Buffer&, unsigned long, VertexArray&, Texture&, int, Texture&)"
);

demangles_no_return_type!(
    _ZL12check_depth8ILi515ELb0EEitPtRDv8_s,
    "check_depth8<515, false>(unsigned short, unsigned short*, short __vector(8)&)"
);

demangles_no_return_type!(
    _ZN7mozilla6detail23RunnableMethodArgumentsIJNS_2wr10WrWindowIdEbEE5applyINS2_12RenderThreadEMS6_FvS3_bEEEDTcl9applyImplfp_fp0_dtdefpT10mArgumentstlNSt3__116integer_sequenceImJLm0ELm1EEEEEEEPT_T0_,
    "mozilla::detail::RunnableMethodArguments<mozilla::wr::WrWindowId, bool>::apply<mozilla::wr::RenderThread, void (mozilla::wr::RenderThread::*)(mozilla::wr::WrWindowId, bool)>(mozilla::wr::RenderThread*, void (mozilla::wr::RenderThread::*)(mozilla::wr::WrWindowId, bool))"
);

demangles_no_param!(
    _ZN7mozilla6detail23RunnableMethodArgumentsIJNS_2wr10WrWindowIdEbEE5applyINS2_12RonderThroudEMS6_FvS3_bEEEDTcl9applyImplfp_fp0_dtdefpT10mArgumentstlNSt3__116integer_sequenceImJLm0ELm1EEEEEEEPT_T0_,
    "decltype ((applyImpl)({parm#1}, {parm#2}, (*this).mArguments, std::__1::integer_sequence<unsigned long, (unsigned long)0, (unsigned long)1>{})) mozilla::detail::RunnableMethodArguments<mozilla::wr::WrWindowId, bool>::apply<mozilla::wr::RonderThroud, void (mozilla::wr::RonderThroud::*)(mozilla::wr::WrWindowId, bool)>"
);

demangles!(
    _ZZN17TestLargestRegion18TestNonRectangularEvENUt_D2Ev,
    "TestLargestRegion::TestNonRectangular()::{unnamed type#1}::~TestNonRectangular()"
);
demangles!(
    clone_suffix,
    "_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE12_M_constructIPcEEvT_S7_St20forward_iterator_tag.isra.90",
    "void std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >::_M_construct<char*>(char*, char*, std::forward_iterator_tag) [clone .isra.90]"
);
demangles!(
    multiple_clone_suffixes,
    "_ZN15google_breakpad17ProcCpuInfoReader14GetValueAndLenEPm.isra.20.part.21",
    "google_breakpad::ProcCpuInfoReader::GetValueAndLen(unsigned long*) [clone .isra.20] [clone .part.21]"
);
// Taken from https://gcc.gnu.org/bugzilla/show_bug.cgi?id=40831
demangles!(
    multiple_clone_numbers,
    "_Z3fooi.part.9.165493.constprop.775.31805",
    "foo(int) [clone .part.9.165493] [clone .constprop.775.31805]"
);
demangles!(_Z1fDpDv1_c, "f(char __vector(1)...)");
demangles!(
    _Z1gIJidEEDTclL_Z1fEspplfp_Li1EEEDpT_,
    "decltype (f(({parm#1}+(1))...)) g<int, double>(int, double)"
);
demangles!(
    _ZN7mozilla5xpcom16GetServiceHelperCI2NS0_18StaticModuleHelperEENS0_8ModuleIDEP8nsresult,
    "mozilla::xpcom::GetServiceHelper::StaticModuleHelper(mozilla::xpcom::ModuleID, nsresult*)"
);
demangles!(_ZNK1QssERKS_, "Q::operator<=>(Q const&) const");
// Taken from https://git.llvm.org/klaus/libcxxabi/commit/5dd173b3792e868a7ebfa699d156f24075eafc01.diff
demangles!(
    ___ZN19URLConnectionClient33_clientInterface_cancelConnectionEP16dispatch_queue_sU13block_pointerFvvE_block_invoke14,
    "invocation function for block in URLConnectionClient::_clientInterface_cancelConnection(dispatch_queue_s*, void () block_pointer)"
);
demangles!(
    _ZNK8SkRecord6Record5visitIRN9SkRecords4DrawEEEDTclfp_cvNS2_4NoOpE_EEEOT_,
    "decltype ({parm#1}(SkRecords::NoOp())) SkRecord::Record::visit<SkRecords::Draw&>(SkRecords::Draw&) const"
);
demangles!(
    _ZGTtNKSt11logic_error4whatEv,
    "transaction clone for std::logic_error::what() const"
);
// Tests the case where the character after 'GT' can be any char but 'n'.
demangles!(
    _ZGTmNKSt11logic_error4whatEv,
    "transaction clone for std::logic_error::what() const"
);
demangles!(
    _ZGTnNKSt11logic_error4whatEv,
    "non-transaction clone for std::logic_error::what() const"
);
demangles!(
    block_invoke_dot_suffix,
    "___ZN6WebKit23ApplicationStateTrackerC2EP6UIViewP13objc_selectorS4_S4__block_invoke.19",
    "invocation function for block in WebKit::ApplicationStateTracker::ApplicationStateTracker(UIView*, objc_selector*, objc_selector*, objc_selector*)"
);
demangles!(
    _ZNKSt6__ndk112basic_stringIDuNS_11char_traitsIDuEENS_9allocatorIDuEEE5c_strEv,
    "std::__ndk1::basic_string<char8_t, std::__ndk1::char_traits<char8_t>, std::__ndk1::allocator<char8_t> >::c_str() const"
);

demangles_simplify_template_parameters!(
    _ZN11SmiTagging2ILs4EE13kSmiShiftSizeE,
    "SmiTagging2<4>::kSmiShiftSize"
);
