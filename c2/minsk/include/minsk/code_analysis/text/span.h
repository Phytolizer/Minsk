#pragma once

#include <stddef.h>

typedef struct
{
  size_t start;
  size_t length;
} minsk_text_span_t;

static inline minsk_text_span_t
minsk_text_span_from_bounds(size_t start, size_t end)
{
  return (minsk_text_span_t){
    .start = start,
    .length = end - start,
  };
}

static inline size_t
minsk_text_span_end(minsk_text_span_t span)
{
  return span.start + span.length;
}
