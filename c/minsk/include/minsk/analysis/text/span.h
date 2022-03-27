#pragma once

#include <stddef.h>

typedef struct {
  size_t start;
  size_t length;
} text_span_t;

text_span_t text_span_from_bounds(size_t start, size_t end);
size_t text_span_end(text_span_t span);
