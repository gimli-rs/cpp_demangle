#!/bin/sh

set -e

function test_cxxfilt() {
    flags=$1
    mangled_name=$2
    demangled_name=$3
    lineno=$4

    if ./cxxfilt $flags $mangled_name | grep -q "$demangled_name"; then
        echo $mangled_name passed
    else
        echo $mangled_name failed at line $lineno
        exit 1
    fi
}

test_cxxfilt "--no-params" "_Z1fCf" "f" $LINENO
test_cxxfilt "" "_Z13function_tempIiEv1AIXszcvT_Li999EEE" "void function_temp<int>(A<sizeof ((int)(999))>)" $LINENO
test_cxxfilt "" "_ZN1KILi1EXadL_ZN1S1mEEEE1fEv" "K<1, &S::m>::f()" $LINENO
