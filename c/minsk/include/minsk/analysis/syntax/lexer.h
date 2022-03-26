#pragma once

#include "minsk/analysis/diagnostic_bag.h"
#include "sds.h"
#include "token.h"
#include <stddef.h>

typedef struct {
  sds text;
  size_t position;
  diagnostic_bag_t diagnostics;
} lexer_t;

void lexer_init(lexer_t *lexer, sds text);
syntax_token_t lexer_next_token(lexer_t *lexer);
void lexer_free(lexer_t *lexer);
