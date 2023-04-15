#pragma once

#include <arena.h>
#include <unicode/umachine.h>

#include "minsk/code_analysis/diagnostic.h"
#include "minsk/code_analysis/syntax/kind.h"
#include "minsk/data_structures/buf.h"
#include "minsk/runtime/object.h"

typedef BUF_T(minsk_diagnostic_t) minsk_diagnostic_bag_buf_t;

typedef struct
{
  Arena * _arena;
  minsk_diagnostic_bag_buf_t diagnostics;
} minsk_diagnostic_bag_t;

extern minsk_diagnostic_bag_t
minsk_diagnostic_bag_new(Arena * arena);

extern void
minsk_diagnostic_bag_report_invalid_number(
  minsk_diagnostic_bag_t * bag,
  minsk_text_span_t span,
  string_t text,
  string_t type_name
);

extern void
minsk_diagnostic_bag_report_bad_character(
  minsk_diagnostic_bag_t * bag,
  int64_t position,
  UChar32 character
);

extern void
minsk_diagnostic_bag_report_unexpected_token(
  minsk_diagnostic_bag_t * bag,
  minsk_text_span_t span,
  minsk_syntax_kind_t actual_kind,
  minsk_syntax_kind_t expected_kind
);

extern void
minsk_diagnostic_bag_report_undefined_binary_operator(
  minsk_diagnostic_bag_t * bag,
  minsk_text_span_t span,
  string_t op_text,
  minsk_object_type_t left_type,
  minsk_object_type_t right_type
);

extern void
minsk_diagnostic_bag_report_undefined_unary_operator(
  minsk_diagnostic_bag_t * bag,
  minsk_text_span_t span,
  string_t op_text,
  minsk_object_type_t operand_type
);

extern void
minsk_diagnostic_bag_report_undefined_name(
  minsk_diagnostic_bag_t * bag,
  minsk_text_span_t span,
  string_t name
);
