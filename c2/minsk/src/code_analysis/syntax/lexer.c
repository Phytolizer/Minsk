#include "minsk/code_analysis/syntax/lexer.h"

#include <minsk-platform/debugger.h>
#include <minsk-string/string.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <utf8proc.h>

#include "minsk/code_analysis/syntax/kind.h"
#include "minsk/code_analysis/syntax/token.h"
#include "minsk/runtime/object.h"

typedef utf8proc_int32_t codepoint_t;

static bool category_is_space(utf8proc_category_t cat)
{
  return cat == UTF8PROC_CATEGORY_ZS || cat == UTF8PROC_CATEGORY_ZL ||
         cat == UTF8PROC_CATEGORY_ZP;
}

static bool is_digit(codepoint_t cp)
{
  return cp >= '0' && cp <= '9';
}

typedef struct
{
  uint64_t value;
  bool did_overflow;
} overflow_t;

enum
{
  UINT63_MAX = UINT64_MAX >> 1U,
};

static overflow_t add_overflow(uint64_t a, uint64_t b)
{
  uint64_t result = (a + b) & UINT63_MAX;
  return (overflow_t){
    .value = result,
    .did_overflow = result < a,
  };
}

static overflow_t mul_overflow(uint64_t a, uint64_t b)
{
  return (overflow_t){
    .value = a * b,
    .did_overflow = b != 0 && a > UINT63_MAX / b,
  };
}

static overflow_t parse_number(string_t str)
{
  uint64_t result = 0;
  for (size_t i = 0; i < str.length; i++)
  {
    // not parsing UTF8 here, only valid chars are ASCII anyway
    char c = str.data[i];

    DEBUGGER_ASSERT(is_digit(c), "bad digit %d", c);
    overflow_t ov = mul_overflow(result, 10);
    if (ov.did_overflow)
    {
      return ov;
    }
    ov = add_overflow(ov.value, c - '0');
    if (ov.did_overflow)
    {
      return ov;
    }
    result = ov.value;
  }
  return (overflow_t){.value = result};
}

static codepoint_t peek(minsk_syntax_lexer_t* lexer, utf8proc_ssize_t offset)
{
  DEBUGGER_ASSERT(offset < MINSK_SYNTAX_LEXER_MAX_PEEK, "offset too large");
  utf8proc_ssize_t position = lexer->_position;
  while (lexer->_peek_count < offset + 1)
  {
    if (position >= lexer->_text_len)
    {
      return '\0';
    }

    utf8proc_ssize_t byte_count = utf8proc_iterate(
      lexer->_text + lexer->_position,
      lexer->_text_len,
      &lexer->_peek_buf[lexer->_peek_count].cp
    );
    if (byte_count > 0)
    {
      lexer->_peek_buf[lexer->_peek_count].position = position;
      lexer->_peek_buf[lexer->_peek_count].size = byte_count;
      position += byte_count;
    }
    else
    {
      DEBUGGER_FATAL("invalid UTF-8");
    }

    ++lexer->_peek_count;
  }

  return lexer->_peek_buf[offset].cp;
}

static inline utf8proc_int32_t current(minsk_syntax_lexer_t* lexer)
{
  return peek(lexer, 0);
}

/// Set lexer->_position to reflect a call to `next`.
static void move(minsk_syntax_lexer_t* lexer, int amount)
{
  minsk_syntax_lexer_peek_char_t const* last = &lexer->_peek_buf[amount - 1];
  lexer->_position = last->position + last->size;
}

static void next(minsk_syntax_lexer_t* lexer, int amount)
{
  if (amount == 0)
  {
    return;
  }

  if (lexer->_peek_count < amount)
  {
    // parse UTF8
    (void)peek(lexer, amount);
  }
  move(lexer, amount);
  if (lexer->_peek_count > amount)
  {
    memmove(
      lexer->_peek_buf,
      lexer->_peek_buf + amount,
      (lexer->_peek_count - amount) * sizeof(minsk_syntax_lexer_peek_char_t)
    );
  }
  lexer->_peek_count -= amount;
}

static void scan_whitespace(minsk_syntax_lexer_t* lexer)
{
  while (category_is_space(utf8proc_category(current(lexer))))
  {
    next(lexer, 1);
  }
}

static void scan_digits(minsk_syntax_lexer_t* lexer)
{
  while (is_digit(current(lexer)))
  {
    next(lexer, 1);
  }
}

static string_t
ref_current_text(minsk_syntax_lexer_t const* lexer, utf8proc_ssize_t start)
{
  return STRING_REF_DATA(lexer->_text + start, lexer->_position - start);
}

extern minsk_syntax_lexer_t minsk_syntax_lexer_new(Arena* arena, string_t text)
{
  return (minsk_syntax_lexer_t){
    ._arena = arena,
    ._text = (utf8proc_uint8_t const*)text.data,
    ._text_len = text.length,
    ._position = 0,
    ._peek_count = 0,
  };
}

extern minsk_syntax_token_t minsk_syntax_lexer_lex(minsk_syntax_lexer_t* lexer)
{
  minsk_syntax_kind_t kind = MINSK_SYNTAX_KIND_BAD_TOKEN;
  utf8proc_ssize_t start = lexer->_position;
  string_t text = EMPTY_STRING;
  minsk_object_t value = MINSK_OBJECT_NIL;

#define TOK(n, k)   \
  next(lexer, (n)); \
  kind = (k);       \
  break

  codepoint_t cp = current(lexer);
  switch (cp)
  {
    case 0: TOK(0, MINSK_SYNTAX_KIND_END_OF_FILE_TOKEN);
    case '+': TOK(1, MINSK_SYNTAX_KIND_PLUS_TOKEN);
    case '-': TOK(1, MINSK_SYNTAX_KIND_MINUS_TOKEN);
    case '*': TOK(1, MINSK_SYNTAX_KIND_STAR_TOKEN);
    case '/': TOK(1, MINSK_SYNTAX_KIND_SLASH_TOKEN);
    case '(': TOK(1, MINSK_SYNTAX_KIND_OPEN_PARENTHESIS_TOKEN);
    case ')': TOK(1, MINSK_SYNTAX_KIND_CLOSE_PARENTHESIS_TOKEN);
    default:
    {
      utf8proc_category_t cat = utf8proc_category(cp);
      if (category_is_space(cat))
      {
        scan_whitespace(lexer);
        kind = MINSK_SYNTAX_KIND_WHITESPACE_TOKEN;
      }
      else if (is_digit(cp))
      {
        scan_digits(lexer);
        kind = MINSK_SYNTAX_KIND_NUMBER_TOKEN;
        text = ref_current_text(lexer, start);
        overflow_t parse_result = parse_number(text);
        if (parse_result.did_overflow)
        {
          BUF_PUSH_ARENA(
            lexer->_arena,
            &lexer->diagnostics,
            string_printf_arena(
              lexer->_arena,
              "The number literal '" STRING_FMT "' is too large.",
              STRING_ARG(text)
            )
          );
        }
        else
        {
          DEBUGGER_ASSERT(
            parse_result.value <= UINT63_MAX,
            "parse_number overflowed silently!"
          );
          value = MINSK_OBJECT_INTEGER(parse_result.value);
        }
      }
    }
  }

  if (kind == MINSK_SYNTAX_KIND_BAD_TOKEN)
  {
    utf8proc_uint8_t charbuf[4];
    utf8proc_ssize_t nbytes = utf8proc_encode_char(current(lexer), charbuf);
    BUF_PUSH_ARENA(
      lexer->_arena,
      &lexer->diagnostics,
      string_printf_arena(
        lexer->_arena,
        "Bad character in input: '" STRING_FMT "'",
        STRING_ARG(STRING_REF_DATA(charbuf, nbytes))
      )
    );
    next(lexer, 1);
  }

  if (text.length == 0)
  {
    text = ref_current_text(lexer, start);
  }
  return (minsk_syntax_token_t){
    .kind = kind,
    .position = start,
    .text = text,
    .value = value,
  };
}
