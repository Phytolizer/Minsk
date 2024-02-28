#pragma once

#include <arena.h>

#include "minsk-string/string.h"
#include "minsk/code_analysis/text/span.h"
#include "minsk/data_structures/buf.h"

typedef struct
{
  size_t start;
  size_t length;
  size_t length_including_line_break;
} minsk_text_line_t;

static inline size_t
minsk_text_line_end(minsk_text_line_t line)
{
  return line.start + line.length;
}

static inline size_t
minsk_text_line_end_including_line_break(minsk_text_line_t line)
{
  return line.start + line.length_including_line_break;
}

static inline minsk_text_span_t
minsk_text_line_span(minsk_text_line_t line)
{
  return (minsk_text_span_t){
    .start = line.start,
    .length = line.length,
  };
}

static inline minsk_text_span_t
minsk_text_line_span_including_line_break(minsk_text_line_t line)
{
  return (minsk_text_span_t){
    .start = line.start,
    .length = line.length_including_line_break,
  };
}

typedef BUF_T(minsk_text_line_t) minsk_text_line_buf_t;

typedef struct
{
  Arena * _arena;
  minsk_text_line_buf_t _lines;
  string_t text;
} minsk_text_source_text_t;

extern minsk_text_source_text_t
minsk_text_source_text_from(string_t text, Arena * arena);

extern size_t
minsk_text_source_text_get_line_index(
  minsk_text_source_text_t text,
  size_t position
);

static inline string_t
minsk_text_source_text_substring_indexed(
  minsk_text_source_text_t text,
  size_t start,
  size_t length
)
{
  return STRING_REF_DATA(text.text.data + start, length);
}

static inline string_t
minsk_text_source_text_substring_bounds(
  minsk_text_source_text_t text,
  size_t start,
  size_t end
)
{
  return minsk_text_source_text_substring_indexed(text, start, end - start);
}

static inline string_t
minsk_text_source_text_substring_span(
  minsk_text_source_text_t text,
  minsk_text_span_t span
)
{
  return minsk_text_source_text_substring_indexed(
    text,
    span.start,
    span.length
  );
}
