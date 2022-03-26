#include "util/line.h"
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

bool util_read_line(char **line_buffer, size_t *buffer_length, FILE *fp) {
  size_t len = 0;
  while (true) {
    if (len == *buffer_length) {
      *buffer_length = *buffer_length * 2 + 1;
      char *new_line_buffer = realloc(*line_buffer, *buffer_length);
      if (new_line_buffer == NULL) {
        free(*line_buffer);
        return false;
      }
      *line_buffer = new_line_buffer;
    }

    int c = fgetc(fp);
    if (c == EOF) {
      if (len == 0) {
        return false;
      }
      break;
    }
    if (c == '\n') {
      break;
    }

    (*line_buffer)[len] = c;
    ++len;
  }

  (*line_buffer)[len] = '\0';

  return true;
}
