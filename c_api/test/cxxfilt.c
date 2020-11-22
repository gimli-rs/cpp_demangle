#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>

#include "cpp_demangle.h"

// As more options are added to the `cpp_demangle` crate, add them here too.
static const struct option long_options[] = {
    {"help", no_argument, NULL, 'h'},
    {"no-params", no_argument, NULL, 'p'},
    {"no-return-type", no_argument, NULL, 'r'},
    {NULL, no_argument, NULL, 0}};

// Prints the help message of the program.
static void usage() {
  fprintf(stdout, "\
    Usage: %s [options] [mangled names]\n",
          "cxxfilt");

  fprintf(stdout, "\
    [-p|--no-params]\t Do not display function arguments\n");

  fprintf(stdout, "\
    [-r|--no-return-type]\t Do not display function return types\n");

  fprintf(stdout, "]\n");

  fprintf(stdout, "\
    [-h|--help]\t Display help information\n");

  exit(1);
}

// Where the main demangling takes place.
void demangle_name(char *mangled_name, bool no_params) {
  struct ParseOptions p = {};
  struct DemangleOptions d = {no_params};
  unsigned skip = 0;

  if (mangled_name[0] == '.' || mangled_name[0] == '$')
    skip++;

  // Every `demangle` call needs to have a matching `free_demangled_name`, which
  // is how the memory is free'd when using this api.
  char *result = demangle(mangled_name, p, d);
  printf("%s\n", result);
  free_demangled_name(result);
}

int main(int argc, char **argv) {
  int c = 0;
  bool no_params = false;
  bool no_return_type = false;

  // This is where all the options/flags for this example should be handled.
  while ((c = getopt_long(argc, argv, "_hp", long_options, (int *)0)) != EOF) {
    switch (c) {
      case '?':
      case 'h':
        usage();
        break;
      case 'p':
        no_params = true;
        break;
      case 'r':
        no_return_type = true;
        break;
    }
  }

  if (optind < argc)
    for (; optind < argc; optind++)
      demangle_name(argv[optind], no_params);

  return 0;
}
