#include "minsk/code_analysis/text/source_text.h"

#include <unicode/uchar.h>
#include <unicode/urename.h>

typedef UChar32 codepoint_t;

static bool
is_like_lf(ULineBreak prop)
{
  return prop == U_LB_MANDATORY_BREAK || prop == U_LB_LINE_FEED ||
         prop == U_LB_NEXT_LINE;
}

static size_t
get_line_break_width(string_t text, size_t position)
{
  codepoint_t current;
  codepoint_t next;
  U8_NEXT(text.data, position, text.length, current);
  U8_NEXT(text.data, position, text.length, next);

  if (current == '\r' && next == '\n')
  {
    return 2;
  }

  ULineBreak prop = u_getIntPropertyValue(current, UCHAR_LINE_BREAK);

  if (!is_like_lf(prop))
  {
    return 0;
  }

  return U8_LENGTH(current);
}

static void
add_line(
  minsk_text_line_buf_t * lines,
  size_t start,
  size_t position,
  size_t line_break_width,
  Arena * arena
)
{
  minsk_text_line_t line = {
    .start = start,
    .length = position - start,
    .length_including_line_break = position - start + line_break_width,
  };
  BUF_PUSH_ARENA(arena, lines, line);
}

static minsk_text_line_buf_t
parse_lines(string_t text, Arena * arena)
{
  minsk_text_line_buf_t lines = BUF_INIT;
  size_t position = 0;
  size_t line_start = 0;

  while (position < text.length)
  {
    size_t line_break_width = get_line_break_width(text, position);
    if (line_break_width == 0)
    {
      position++;
    }
    else
    {
      add_line(&lines, line_start, position, line_break_width, arena);
      position += line_break_width;
      line_start = position;
    }
  }

  if (position >= line_start)
  {
    add_line(&lines, line_start, position, 0, arena);
  }

  return lines;
}

extern minsk_text_source_text_t
minsk_text_source_text_from(string_t text, Arena * arena)
{
  return (minsk_text_source_text_t){
    ._arena = arena,
    .lines = parse_lines(text, arena),
    .text = text,
  };
}

extern size_t
minsk_text_source_text_get_line_index(
  minsk_text_source_text_t text,
  size_t position
)
{
  size_t lower = 0;
  size_t upper = text.lines.len - 1;

  while (lower <= upper)
  {
    size_t index = lower + (upper - lower) / 2;
    minsk_text_line_t line = text.lines.ptr[index];
    if (line.start == position)
    {
      return index;
    }
    if (line.start > position)
    {
      upper = index - 1;
    }
    else
    {
      lower = index + 1;
    }
  }
  return lower - 1;
}
