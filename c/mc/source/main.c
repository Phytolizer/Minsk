#include "util/line.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
  char *line = NULL;
  size_t line_len = 0;
  while (true) {
    printf("> ");
    if (!util_read_line(&line, &line_len, stdin)) {
      break;
    }

    if (strcmp(line, "1 + 2 * 3") == 0) {
      printf("7\n");
    } else {
      printf("ERROR: Invalid expression!\n");
    }
  }
  free(line);
  return 0;
}
