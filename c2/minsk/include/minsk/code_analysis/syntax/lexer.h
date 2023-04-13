#pragma once

#include <arena.h>
#include <minsk-string/string.h>
#include <stdint.h>
#include <unicode/umachine.h>

#include "minsk/code_analysis/diagnostic_buf.h"
#include "minsk/code_analysis/syntax/token.h"

enum
{
  MINSK_SYNTAX_LEXER_MAX_PEEK = 4
};

typedef struct
{
  UChar32 cp;
  int64_t position;
  int size;
} minsk_syntax_lexer_peek_char_t;

typedef struct
{
  Arena * _arena;
  uint8_t const * _text;
  int64_t _text_len;
  int64_t _position;
  minsk_syntax_lexer_peek_char_t _peek_buf[MINSK_SYNTAX_LEXER_MAX_PEEK];
  int64_t _peek_count;
  minsk_diagnostic_buf_t diagnostics;
} minsk_syntax_lexer_t;

extern minsk_syntax_lexer_t
minsk_syntax_lexer_new(Arena * arena, string_t text);
extern minsk_syntax_token_t
minsk_syntax_lexer_lex(minsk_syntax_lexer_t * lexer);
