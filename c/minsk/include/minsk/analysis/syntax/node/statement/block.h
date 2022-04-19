#pragma once

#include "minsk/analysis/syntax/node/statement.h"
#include "minsk/analysis/syntax/token.h"

typedef struct {
  statement_syntax_t base;
  syntax_token_t open_brace_token;
  statement_syntax_vector_t statements;
  syntax_token_t close_brace_token;
} block_statement_syntax_t;

void block_statement_syntax_init(block_statement_syntax_t* stmt,
    syntax_token_t open_brace_token, statement_syntax_vector_t statements,
    syntax_token_t close_brace_token);
void block_statement_syntax_free(block_statement_syntax_t* stmt);
