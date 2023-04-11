#pragma once

#include <bstrlib.h>

#include "./token.h"

typedef struct
{
  const_bstring _text;
  int _position;
} minsk_syntax_lexer_t;

extern minsk_syntax_lexer_t minsk_syntax_lexer_new(const_bstring text);
extern minsk_syntax_token_t minsk_syntax_lexer_lex(minsk_syntax_lexer_t* lexer);
