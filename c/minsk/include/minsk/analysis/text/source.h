#pragma once

#include "minsk/analysis/text/span.h"
#include "sds.h"
#include <stddef.h>

typedef struct {
  size_t start;
  size_t length;
  size_t length_including_line_break;
} text_line_t;

text_line_t text_line_new(
    size_t start, size_t length, size_t length_including_line_break);
size_t text_line_end(text_line_t line);
size_t text_line_end_including_line_break(text_line_t line);
text_span_t text_line_span(text_line_t line);
text_span_t text_line_span_including_line_break(text_line_t line);

typedef struct {
  text_line_t* data;
  size_t length;
  size_t capacity;
} text_line_vector_t;

void text_line_vector_init(text_line_vector_t* vector);
void text_line_vector_push(text_line_vector_t* vector, text_line_t line);
void text_line_vector_free(text_line_vector_t* vector);

typedef struct {
  sds text;
  text_line_vector_t lines;
} source_text_t;

source_text_t source_text_from(sds text);
size_t source_text_get_line_index(source_text_t* text, size_t offset);
void source_text_free(source_text_t* text);
