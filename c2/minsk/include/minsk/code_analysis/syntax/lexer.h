#pragma once

#include <arena.h>
#include <minsk-string/string.h>
#include <utf8proc.h>

#include "./token.h"
#include "minsk/code_analysis/diagnostic_buf.h"

enum
{
  MINSK_SYNTAX_LEXER_MAX_PEEK = 4,
};

typedef struct
{
  utf8proc_int32_t cp;
  utf8proc_ssize_t position;
  int size;
} minsk_syntax_lexer_peek_char_t;

typedef struct
{
  Arena* _arena;
  utf8proc_uint8_t const* _text;
  utf8proc_ssize_t _text_len;
  utf8proc_ssize_t _position;
  minsk_syntax_lexer_peek_char_t _peek_buf[MINSK_SYNTAX_LEXER_MAX_PEEK];
  utf8proc_ssize_t _peek_count;
  minsk_diagnostic_buf_t diagnostics;
} minsk_syntax_lexer_t;

extern minsk_syntax_lexer_t minsk_syntax_lexer_new(Arena* arena, string_t text);
extern minsk_syntax_token_t minsk_syntax_lexer_lex(minsk_syntax_lexer_t* lexer);
