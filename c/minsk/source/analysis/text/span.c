#include "minsk/analysis/text/span.h"

text_span_t text_span_from_bounds(size_t start, size_t end) {
  text_span_t span = {
      .start = start,
      .length = end - start,
  };
  return span;
}

size_t text_span_end(text_span_t span) { return span.start + span.length; }
