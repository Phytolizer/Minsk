#pragma once

#include "minsk/analysis/diagnostic.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/text/span.h"
#include <stddef.h>

typedef struct {
  diagnostic_t *data;
  size_t length;
  size_t capacity;
} diagnostic_bag_t;

void diagnostic_bag_init(diagnostic_bag_t *bag);
void diagnostic_bag_free(diagnostic_bag_t *bag);
void diagnostic_bag_copy_diagnostic(diagnostic_bag_t *bag,
                                    diagnostic_t diagnostic);
void diagnostic_bag_report_invalid_int(diagnostic_bag_t *bag, text_span_t span,
                                       sds text);
void diagnostic_bag_report_bad_character(diagnostic_bag_t *bag, size_t position,
                                         char character);
void diagnostic_bag_report_unexpected_token(diagnostic_bag_t *bag,
                                            text_span_t span,
                                            syntax_kind_t expected_kind,
                                            syntax_kind_t actual_kind);
