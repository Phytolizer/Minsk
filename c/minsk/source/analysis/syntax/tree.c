#include "minsk/analysis/syntax/tree.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/syntax/lexer.h"
#include "minsk/analysis/syntax/node.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/node/unit.h"
#include "minsk/analysis/syntax/parser.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/analysis/syntax/tokens.h"
#include "minsk/analysis/text/source.h"

void syntax_tree_free(syntax_tree_t* tree) {
  compilation_unit_syntax_free(&tree->root);
  diagnostic_bag_free(&tree->diagnostics);
  source_text_free(&tree->source_text);
}

syntax_tree_t syntax_tree_parse(const char* text) {
  parser_t parser;
  parser_init(&parser, source_text_from(sdsnew(text)));
  compilation_unit_syntax_t root = parser_parse_compilation_unit(&parser);
  diagnostic_bag_t diagnostics = parser.diagnostics;
  parser.diagnostics.length = 0;
  parser.diagnostics.data = NULL;
  parser_free(&parser);
  return (syntax_tree_t){
      .diagnostics = diagnostics,
      .root = root,
      .source_text = parser.source_text,
  };
}

syntax_token_vector_t syntax_tree_parse_tokens(const char* text) {
  lexer_t lexer;
  lexer_init(&lexer, source_text_from(sdsnew(text)));
  syntax_token_vector_t tokens;
  syntax_token_vector_init(&tokens);
  while (true) {
    syntax_token_t token = lexer_next_token(&lexer);
    if (token.base.kind == syntax_kind_end_of_file_token) {
      token_free(&token);
      break;
    }
    syntax_token_vector_push(&tokens, token);
  }
  source_text_free(&lexer.text);
  lexer_free(&lexer);

  return tokens;
}
