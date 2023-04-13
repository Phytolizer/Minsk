#include "minsk/code_analysis/diagnostic_bag.h"

#include <unicode/utf.h>

extern minsk_diagnostic_bag_t
minsk_diagnostic_bag_new(Arena * arena)
{
  return (minsk_diagnostic_bag_t){
    ._arena = arena,
  };
}

static void
report(minsk_diagnostic_bag_t * bag, minsk_text_span_t span, string_t message)
{
  minsk_diagnostic_t diagnostic = {span, message};
  BUF_PUSH_ARENA(bag->_arena, &bag->diagnostics, diagnostic);
}

extern void
minsk_diagnostic_bag_report_invalid_number(
  minsk_diagnostic_bag_t * bag,
  minsk_text_span_t span,
  string_t text,
  string_t type_name
)
{
  report(
    bag,
    span,
    string_printf_arena(
      bag->_arena,
      "The number '" STRING_FMT "' isn't a valid " STRING_FMT ".",
      STRING_ARG(text),
      STRING_ARG(type_name)
    )
  );
}

extern void
minsk_diagnostic_bag_report_bad_character(
  minsk_diagnostic_bag_t * bag,
  int64_t position,
  UChar32 character
)
{
  uint8_t utf8buf[UTF8_MAX_CHAR_LENGTH];
  int64_t nbytes = 0;
  U8_APPEND_UNSAFE(utf8buf, nbytes, character);
  minsk_text_span_t span = {position, 1};
  report(
    bag,
    span,
    string_printf_arena(
      bag->_arena,
      "Bad character in input: '" STRING_FMT "'.",
      STRING_ARG(STRING_REF_DATA(utf8buf, nbytes))
    )
  );
}

extern void
minsk_diagnostic_bag_report_unexpected_token(
  minsk_diagnostic_bag_t * bag,
  minsk_text_span_t span,
  minsk_syntax_kind_t actual_kind,
  minsk_syntax_kind_t expected_kind
)
{
  report(
    bag,
    span,
    string_printf_arena(
      bag->_arena,
      "Unexpected token <" STRING_FMT ">, expected <" STRING_FMT ">.",
      STRING_ARG(minsk_syntax_kind_display_name(bag->_arena, actual_kind)),
      STRING_ARG(minsk_syntax_kind_display_name(bag->_arena, expected_kind))
    )
  );
}

extern void
minsk_diagnostic_bag_report_undefined_binary_operator(
  minsk_diagnostic_bag_t * bag,
  minsk_text_span_t span,
  string_t op_text,
  minsk_object_type_t left_type,
  minsk_object_type_t right_type
)
{
  report(
    bag,
    span,
    string_printf_arena(
      bag->_arena,
      "The binary operator '" STRING_FMT
      "' isn't defined for the types " STRING_FMT " and " STRING_FMT ".",
      STRING_ARG(op_text),
      STRING_ARG(minsk_object_type_display_name(bag->_arena, left_type)),
      STRING_ARG(minsk_object_type_display_name(bag->_arena, right_type))
    )
  );
}

extern void
minsk_diagnostic_bag_report_undefined_unary_operator(
  minsk_diagnostic_bag_t * bag,
  minsk_text_span_t span,
  string_t op_text,
  minsk_object_type_t operand_type
)
{
  report(
    bag,
    span,
    string_printf_arena(
      bag->_arena,
      "The unary operator '" STRING_FMT
      "' isn't defined for the type " STRING_FMT ".",
      STRING_ARG(op_text),
      STRING_ARG(minsk_object_type_display_name(bag->_arena, operand_type))
    )
  );
}
