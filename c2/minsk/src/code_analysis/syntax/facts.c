#include "minsk/code_analysis/syntax/facts.h"

extern minsk_syntax_facts_precedence_t
minsk_syntax_facts_binary_operator_precedence(minsk_syntax_kind_t kind)
{
  switch (kind)
  {
    case MINSK_SYNTAX_KIND_STAR_TOKEN:
    case MINSK_SYNTAX_KIND_SLASH_TOKEN: return 2;
    case MINSK_SYNTAX_KIND_PLUS_TOKEN:
    case MINSK_SYNTAX_KIND_MINUS_TOKEN: return 1;
    default: return 0;
  }
}

extern minsk_syntax_facts_precedence_t
minsk_syntax_facts_unary_operator_precedence(minsk_syntax_kind_t kind)
{
  switch (kind)
  {
    case MINSK_SYNTAX_KIND_PLUS_TOKEN:
    case MINSK_SYNTAX_KIND_MINUS_TOKEN: return 3;
    default: return 0;
  }
}
