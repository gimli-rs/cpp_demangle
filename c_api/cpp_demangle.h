#ifdef __cplusplus
extern "C" {
#endif

// Flags currently in `cpp_demangle`
typedef enum {
  DMGL_NO_OPTS = 0,
  DMGL_PARAMS  = (1 << 0)
} DemangleOptions;

extern char *demangle(const char *buffer, DemangleOptions options);
extern void free_demangled_name(char *buffer);

#ifdef __cplusplus
}
#endif
