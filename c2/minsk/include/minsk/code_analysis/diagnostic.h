#pragma once

#include <minsk-string/string.h>

#include "minsk/code_analysis/text/span.h"

typedef struct
{
  minsk_text_span_t span;
  string_t message;
} minsk_diagnostic_t;
