#pragma once

#include <minsk-string/string.h>
#include <stdint.h>

#include "minsk/code_analysis/syntax/kind.h"
#include "minsk/code_analysis/text/span.h"
#include "minsk/runtime/object.h"

typedef struct
{
  minsk_syntax_kind_t kind;
  int64_t position;
  string_t text;
  minsk_object_t value;
} minsk_syntax_token_t;

static inline minsk_text_span_t
minsk_syntax_token_span(minsk_syntax_token_t tok)
{
  return (minsk_text_span_t){tok.position, tok.text.length};
}
