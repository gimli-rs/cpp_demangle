LDIR=../target/debug
IDIR=../include

CARGO?=$(shell which cargo)

CFLAGS=-I$(IDIR)
LFLAGS=-L. -l:../target/debug/libcpp_demangle_c_api.so

all: cxxfilt

debug: CFLAGS += -g -O0 -fsanitize=address -fsanitize=undefined
debug: cxxfilt

cxxfilt: cxxfilt.c
	# Lets assure that the library is actually built
	$(CARGO) build --manifest-path ../Cargo.toml
	$(CC) $(CFLAGS) -o $@ $< $(LFLAGS)

clean:
	rm cxxfilt
