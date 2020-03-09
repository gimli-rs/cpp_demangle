use cpp_demangle;
use libc;
use std::ffi;

#[no_mangle]
pub unsafe extern "C" fn demangle(
    buffer: *const libc::c_char,
    options: cpp_demangle::DemangleOptions,
) -> *mut libc::c_char {
    if buffer.is_null() {
        return buffer as *mut libc::c_char
    }

    let buffer = ffi::CStr::from_ptr(buffer);

    if let Ok((symbol, _)) = cpp_demangle::BorrowedSymbol::with_tail(&buffer.to_bytes_with_nul()) {
        return ffi::CString::new(symbol.demangle(&options).unwrap()).unwrap().into_raw();
    }

    buffer.to_owned().into_raw()
}

#[no_mangle]
pub unsafe extern "C" fn free_demangled_name(buffer: *mut libc::c_char) {
    if buffer.is_null() {
        return;
    }
    ffi::CString::from_raw(buffer);
}
