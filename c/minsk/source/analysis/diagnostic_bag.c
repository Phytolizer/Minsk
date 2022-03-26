#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/diagnostic.h"
#include "minsk/analysis/text/span.h"
#include "sds.h"
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

static void report(diagnostic_bag_t *bag, text_span_t span, sds message) {
  diagnostic_t diagnostic = diagnostic_new(span, message);
  if (bag->length == bag->capacity) {
    bag->capacity = bag->capacity * 2 + 1;
    diagnostic_t *new_data =
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

void diagnostic_bag_init(diagnostic_bag_t *bag) {
  bag->data = NULL;
  bag->length = 0;
  bag->capacity = 0;
}

void diagnostic_bag_free(diagnostic_bag_t *bag) {
  for (size_t i = 0; i < bag->length; i++) {
    diagnostic_free(&bag->data[i]);
  }
  free(bag->data);
  bag->data = NULL;
  bag->length = 0;
  bag->capacity = 0;
}

void diagnostic_bag_copy_diagnostic(diagnostic_bag_t *bag,
                                    diagnostic_t diagnostic) {
  report(bag, diagnostic.span, sdsdup(diagnostic.message));
}

void diagnostic_bag_report_invalid_int(diagnostic_bag_t *bag, text_span_t span,
                                       sds text) {
  sds message =
      sdscatfmt(sdsempty(), "Number '%S' doesn't fit in an int", text);
  report(bag, span, message);
  sdsfree(text);
}

void diagnostic_bag_report_bad_character(diagnostic_bag_t *bag, size_t position,
                                         char character) {
  sds message =
      sdscatprintf(sdsempty(), "Bad character in input: '%c'", character);
  report(bag, (text_span_t){.start = position, .length = 1}, message);
}

void diagnostic_bag_report_unexpected_token(diagnostic_bag_t *bag,
                                            text_span_t span,
                                            syntax_kind_t expected_kind,
                                            syntax_kind_t actual_kind) {
  sds message = sdscatfmt(sdsempty(), "Unexpected token <%s>, expected <%s>",
                          syntax_kind_to_string(expected_kind),
                          syntax_kind_to_string(actual_kind));
  report(bag, span, message);
}
