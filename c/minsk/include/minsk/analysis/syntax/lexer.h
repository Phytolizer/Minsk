#pragma once

#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/text/source.h"
#include "sds.h"
#include "token.h"
#include <stddef.h>

typedef struct {
  source_text_t text;
  size_t position;
  diagnostic_bag_t diagnostics;
} lexer_t;

void lexer_init(lexer_t *lexer, source_text_t text);
syntax_token_t lexer_next_token(lexer_t *lexer);
void lexer_free(lexer_t *lexer);
