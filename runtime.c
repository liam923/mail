// stdio gives nullability-completeness warnings
#pragma GCC diagnostic ignored "-Wnullability-completeness"
#include <stdio.h>

void printNum(long n) {
  printf("%ld\n", n);
}
