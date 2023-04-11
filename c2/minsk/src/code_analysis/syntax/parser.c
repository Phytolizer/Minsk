#include "minsk/code_analysis/syntax/parser.h"

#include <arena.h>
#include <minsk-string/string.h>
#include <minsk/code_analysis/syntax/kind.h>
#include <minsk/code_analysis/syntax/lexer.h>
#include <minsk/code_analysis/syntax/token.h>
#include <stdbool.h>

extern minsk_syntax_parser_t
minsk_syntax_parser_new(Arena* arena, string_t text)
{
  minsk_syntax_lexer_t lexer = minsk_syntax_lexer_new(text);
  minsk_syntax_parser_token_buf_t tokens = {0};
  while (true)
  {
    minsk_syntax_token_t tok = minsk_syntax_lexer_lex(&lexer);
    if (tok.kind == MINSK_SYNTAX_KIND_WHITESPACE_TOKEN || tok.kind == MINSK_SYNTAX_KIND_BAD_TOKEN)
    {
      string_free(tok.text);
      continue;
    }
    BUF_PUSH_ARENA(arena, &tokens, tok);
    if (tok.kind == MINSK_SYNTAX_KIND_END_OF_FILE_TOKEN)
    {
      break;
    }
  }
  return (minsk_syntax_parser_t){
    ._tokens = tokens,
    ._position = 0,
  };
}
