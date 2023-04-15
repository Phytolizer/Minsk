#include "minsk/code_analysis/syntax/tree.h"

#include <arena.h>
#include <minsk-string/string.h>

#include "minsk/code_analysis/syntax/lexer.h"
#include "minsk/code_analysis/syntax/parser.h"

extern minsk_syntax_tree_t
minsk_syntax_tree_parse(Arena * arena, string_t text)
{
  minsk_syntax_parser_t parser = minsk_syntax_parser_new(arena, text);
  return minsk_syntax_parser_parse(&parser);
}

extern minsk_syntax_token_buf_t
minsk_syntax_tree_parse_tokens(Arena * arena, string_t text)
{
  minsk_syntax_lexer_t lexer = minsk_syntax_lexer_new(arena, text);
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
