#pragma once

#include <stddef.h>

typedef struct {
  size_t start;
  size_t length;
} text_span_t;

size_t text_span_end(text_span_t span);
