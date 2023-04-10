#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stdint.h>

// options that control parsing
struct ParseOptions {
  // The parsing recursion limit. 0 uses the default value.
  uint32_t recursion_limit;
  // The substitution table limit. 0 uses the default value.
  uint32_t substitutions_limit;
};

// Flags currently in `cpp_demangle`, but options that control demangling
struct DemangleOptions {
  // Do not display function arguments.
  bool no_params;
  // Do not display the function return type.
  bool no_return_type;
  // The demangling recursion limit. 0 uses the default value.
  uint32_t recursion_limit;
};

extern char *demangle(const char *buffer, struct ParseOptions parse_options, struct DemangleOptions demangle_options);
extern void free_demangled_name(char *buffer);

#ifdef __cplusplus
}
#endif
