#ifdef __cplusplus

extern "C" {
#endif

// Flags currently in `cpp_demangle`, but options that control demangling
struct DemangleOptions {
  // Do not display function arguments.
  bool no_params;
};

extern char *demangle(const char *buffer, struct DemangleOptions options);
extern void free_demangled_name(char *buffer);

#ifdef __cplusplus
}
#endif
