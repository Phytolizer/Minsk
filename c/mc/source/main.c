#include "util/line.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

int main(void) {
  char* line = NULL;
  size_t line_len = 0;
  while (true) {
    if (!util_read_line(&line, &line_len, stdin)) {
      break;
    }

    printf("%s\n", line);
  }
  free(line);
  return 0;
}
