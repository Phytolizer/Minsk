#include "minsk/analysis/syntax/node/statement/block.h"
#include "minsk/analysis/syntax/node/statement.h"
#include "minsk/analysis/syntax/token.h"
#include <stdlib.h>

void block_statement_syntax_init(
    block_statement_syntax_t* stmt,
    syntax_token_t open_brace_token,
    statement_syntax_vector_t statements,
    syntax_token_t close_brace_token
) {
  stmt->base.base.kind = syntax_kind_block_statement;
  stmt->base.base.is_token = false;
  stmt->open_brace_token = open_brace_token;
  stmt->statements = statements;
  stmt->close_brace_token = close_brace_token;
}

void block_statement_syntax_free(block_statement_syntax_t* stmt) {
  token_free(&stmt->open_brace_token);
  statement_syntax_vector_free(&stmt->statements);
  token_free(&stmt->close_brace_token);
  free(stmt);
}
