#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/diagnostic.h"
#include "minsk/analysis/text/span.h"
#include "sds.h"
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

static void report(diagnostic_bag_t* bag, text_span_t span, sds message) {
  diagnostic_t diagnostic = diagnostic_new(span, message);
  if (bag->length == bag->capacity) {
    bag->capacity = bag->capacity * 2 + 1;
    diagnostic_t* new_data =
        realloc(bag->data, sizeof(diagnostic_t) * bag->capacity);
    if (new_data == NULL) {
      free(bag->data);
      assert(false && "memory allocation failure");
    }
    bag->data = new_data;
  }
  bag->data[bag->length] = diagnostic;
  bag->length += 1;
}

void diagnostic_bag_init(diagnostic_bag_t* bag) {
  bag->data = NULL;
  bag->length = 0;
  bag->capacity = 0;
}

void diagnostic_bag_free(diagnostic_bag_t* bag) {
  for (size_t i = 0; i < bag->length; i++) {
    diagnostic_free(&bag->data[i]);
  }
  free(bag->data);
  bag->data = NULL;
  bag->length = 0;
  bag->capacity = 0;
}

void diagnostic_bag_copy_diagnostic(
    diagnostic_bag_t* bag,
    diagnostic_t diagnostic
) {
  report(bag, diagnostic.span, sdsdup(diagnostic.message));
}

void diagnostic_bag_append_range(
    diagnostic_bag_t* bag,
    diagnostic_bag_t other
) {
  // Concatenate the two bags and store in `bag`.
  for (size_t i = 0; i < other.length; i++) {
    report(bag, other.data[i].span, sdsdup(other.data[i].message));
  }
}

void diagnostic_bag_report_invalid_int(
    diagnostic_bag_t* bag,
    text_span_t span,
    sds text
) {
  sds message =
      sdscatfmt(sdsempty(), "Number '%s' doesn't fit in an int", text);
  report(bag, span, message);
  sdsfree(text);
}

void diagnostic_bag_report_bad_character(
    diagnostic_bag_t* bag,
    size_t position,
    char character
) {
  sds message =
      sdscatprintf(sdsempty(), "Bad character in input: '%c'", character);
  report(bag, (text_span_t){.start = position, .length = 1}, message);
}

void diagnostic_bag_report_unexpected_token(
    diagnostic_bag_t* bag,
    text_span_t span,
    syntax_kind_t expected_kind,
    syntax_kind_t actual_kind
) {
  sds message = sdscatfmt(
      sdsempty(),
      "Unexpected token <%s>, expected <%s>",
      syntax_kind_to_string(expected_kind),
      syntax_kind_to_string(actual_kind)
  );
  report(bag, span, message);
}

void diagnostic_bag_report_undefined_binary_operator(
    diagnostic_bag_t* bag,
    text_span_t span,
    const char* operator_text,
    object_kind_t left_type,
    object_kind_t right_type
) {
  sds message = sdscatfmt(
      sdsempty(),
      "Undefined binary operator '%s' for types <%s> and "
      "<%s>",
      operator_text,
      object_kind_to_string(left_type),
      object_kind_to_string(right_type)
  );
  report(bag, span, message);
}

void diagnostic_bag_report_undefined_unary_operator(
    diagnostic_bag_t* bag,
    text_span_t span,
    const char* operator_text,
    object_kind_t operand_type
) {
  sds message = sdscatfmt(
      sdsempty(),
      "Undefined unary operator '%s' for type <%s>",
      operator_text,
      object_kind_to_string(operand_type)
  );
  report(bag, span, message);
}

void diagnostic_bag_report_undefined_variable(
    diagnostic_bag_t* bag,
    text_span_t span,
    const char* name
) {
  sds message = sdscatfmt(sdsempty(), "Undefined variable '%s'", name);
  report(bag, span, message);
}

void diagnostic_bag_report_cannot_convert(
    diagnostic_bag_t* bag,
    text_span_t span,
    object_kind_t from_type,
    object_kind_t to_type
) {
  sds message = sdscatfmt(
      sdsempty(),
      "Cannot convert from type <%s> to type <%s>",
      object_kind_to_string(from_type),
      object_kind_to_string(to_type)
  );
  report(bag, span, message);
}
