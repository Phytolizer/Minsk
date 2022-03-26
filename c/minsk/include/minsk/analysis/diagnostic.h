#pragma once

#include "minsk/analysis/text/span.h"
#include "sds.h"

typedef struct {
  text_span_t span;
  sds message;
} diagnostic_t;

diagnostic_t diagnostic_new(text_span_t span, sds message);
void diagnostic_free(diagnostic_t *diagnostic);
