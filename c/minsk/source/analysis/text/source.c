#include "minsk/analysis/text/source.h"
#include "sds.h"
#include <assert.h>
#include <stddef.h>
#include <stdlib.h>

text_line_t text_line_new(size_t start, size_t length,
                          size_t length_including_line_break) {
  text_line_t line = {
      .start = start,
      .length = length,
      .length_including_line_break = length_including_line_break,
  };
  return line;
}

size_t text_line_end(text_line_t line) { return line.start + line.length; }

size_t text_line_end_including_line_break(text_line_t line) {
  return line.start + line.length_including_line_break;
}

text_span_t text_line_span(text_line_t line) {
  return (text_span_t){.start = line.start, .length = line.length};
}

text_span_t text_line_span_including_line_break(text_line_t line) {
  return (text_span_t){
      .start = line.start,
      .length = line.length_including_line_break,
  };
}

void text_line_vector_init(text_line_vector_t *vector) {
  vector->data = malloc(sizeof(text_line_t) * 8);
  vector->length = 0;
  vector->capacity = 8;
}

void text_line_vector_push(text_line_vector_t *vector, text_line_t line) {
  if (vector->length == vector->capacity) {
    vector->capacity *= 2;
    text_line_t *new_data =
        realloc(vector->data, sizeof(text_line_t) * vector->capacity);
    assert(new_data != NULL);
    vector->data = new_data;
  }
  vector->data[vector->length] = line;
  vector->length += 1;
}

void text_line_vector_free(text_line_vector_t *vector) { free(vector->data); }

static size_t get_line_break_width(sds text, size_t position) {
  char character = text[position];
  char lookahead =
      (char)(position + 1 == sdslen(text) ? '\0' : text[position + 1]);
  if (character == '\r' && lookahead == '\n') {
    return 2;
  }
  if (character == '\r' || character == '\n') {
    return 1;
  }
  return 0;
}

static void source_text_parse_lines(source_text_t *source_text) {
  size_t position = 0;
  size_t line_start = 0;

  while (position < sdslen(source_text->text)) {
    size_t line_break_width = get_line_break_width(source_text->text, position);

    if (line_break_width == 0) {
      position += 1;
    } else {
      text_line_vector_push(
          &source_text->lines,
          text_line_new(line_start, position - line_start,
                        position - line_start + line_break_width));
      line_start = position + line_break_width;
      position = line_start;
    }
  }

  text_line_vector_push(
      &source_text->lines,
      text_line_new(line_start, position - line_start, position - line_start));
}

source_text_t source_text_from(sds text) {
  source_text_t source_text = {.text = text};
  text_line_vector_init(&source_text.lines);
  source_text_parse_lines(&source_text);
  return source_text;
}

size_t source_text_get_line_index(source_text_t *text, size_t offset) {
  // binary search the lines
  size_t start = 0;
  size_t end = text->lines.length;

  while (start < end) {
    size_t mid = (start + end) / 2;
    text_line_t line = text->lines.data[mid];
    if (offset < line.start) {
      end = mid;
    } else if (offset >= text_line_end(line)) {
      start = mid + 1;
    } else {
      return mid;
    }
  }
  return start;
}

void source_text_free(source_text_t *text) {
  sdsfree(text->text);
  text_line_vector_free(&text->lines);
}
