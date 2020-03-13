#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>

#include "cpp_demangle.h"

// TODO(MGS): As more options are added to the `cpp_demangle` crate, add them
// here too
static const struct option long_options[] = {
    {"help", no_argument, NULL, 'h'},
    {"no-params", no_argument, NULL, 'p'},
    {NULL, no_argument, NULL, 0}};

static void usage() {
  fprintf(stdout, "\
    Usage: %s [options] [mangled names]\n",
          "cxxfilt");

  fprintf(stdout, "\
    [-p|--no-params]\t Do not display function arguments\n");

  fprintf(stdout, "]\n");

  fprintf(stdout, "\
    [-h|--help]\t Display help information\n");

  exit(1);
}

void demangle_name(char *mangled_name, bool no_params) {
  struct DemangleOptions d = {no_params};
  unsigned skip = 0;

  if (mangled_name[0] == '.' || mangled_name[0] == '$')
    skip++;

  char *result = demangle(mangled_name, d);
  printf("%s\n", result);
  free_demangled_name(result);
}

int main(int argc, char **argv) {
  int c = 0;
  bool no_params = false;
  while ((c = getopt_long(argc, argv, "_hp", long_options, (int *)0)) != EOF) {
    switch (c) {
      case '?':
      case 'h':
        usage();
        break;
      case 'p':
        no_params = true;
        break;
    }
  }

  if (optind < argc)
    for (; optind < argc; optind++)
      demangle_name(argv[optind], no_params);

  return 0;
}
