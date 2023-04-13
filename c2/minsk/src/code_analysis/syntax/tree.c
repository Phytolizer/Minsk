#include "minsk/code_analysis/syntax/tree.h"

#include <arena.h>
#include <minsk-string/string.h>

#include "minsk/code_analysis/syntax/parser.h"

extern minsk_syntax_tree_t
minsk_syntax_tree_parse(Arena * arena, string_t text)
{
  minsk_syntax_parser_t parser = minsk_syntax_parser_new(arena, text);
  return minsk_syntax_parser_parse(&parser);
}
