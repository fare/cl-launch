#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>

#ifndef CL_LAUNCH_SCRIPT
#define CL_LAUNCH_SCRIPT "/usr/bin/cl-launch"
#endif

char me[] = "cl-shim";
char var[] = "CL_ARGV0";
char shell[] = "/bin/sh";

int main (int argc, char**argv) {
  int res;
  size_t l;
  char**args;

  res = setenv(var, argv[0], 1);
  if (res == -1) { perror(me); exit(42); }

  l = (1+argc)*sizeof(char**);
  args = malloc(l);
  if (!args) { exit(42); }

  args[0] = argv[0];
  args[1] = CL_LAUNCH_SCRIPT;
  for(;argc>0;argc--) {
    args[argc+1] = argv[argc];
  }
  res = execv(shell, args);
  perror(me);
  exit(42);
}
