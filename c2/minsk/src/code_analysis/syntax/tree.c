#include "minsk/code_analysis/syntax/tree.h"

#include <arena.h>
#include <minsk-string/string.h>
#include <stdbool.h>

#include "minsk/code_analysis/syntax/kind.h"
#include "minsk/code_analysis/syntax/lexer.h"
#include "minsk/code_analysis/syntax/parser.h"

extern minsk_syntax_tree_t
minsk_syntax_tree_parse(Arena * arena, string_t text)
{
  minsk_text_source_text_t source_text =
    minsk_text_source_text_from(text, arena);
  minsk_syntax_parser_t parser = minsk_syntax_parser_new(arena, source_text);
  minsk_syntax_compilation_unit_t compilation_unit =
    minsk_syntax_parser_parse_compilation_unit(&parser);
  return (minsk_syntax_tree_t){
    .text = source_text,
    .diagnostics = parser.diagnostics,
    .root = compilation_unit,
  };
}

extern minsk_syntax_token_buf_t
minsk_syntax_tree_parse_tokens(Arena * arena, string_t text)
{
  minsk_text_source_text_t source_text =
    minsk_text_source_text_from(text, arena);
  minsk_syntax_lexer_t lexer = minsk_syntax_lexer_new(arena, source_text);
  minsk_syntax_token_buf_t buf = BUF_INIT;
  while (true)
  {
    minsk_syntax_token_t token = minsk_syntax_lexer_lex(&lexer);
    if (token.kind == MINSK_SYNTAX_KIND_END_OF_FILE_TOKEN)
    {
      break;
    }
    BUF_PUSH_ARENA(arena, &buf, token);
  }
  return buf;
}
