#include "minsk/code_analysis/syntax/facts.h"

#include <uthash.h>

extern minsk_syntax_facts_precedence_t
minsk_syntax_facts_binary_operator_precedence(minsk_syntax_kind_t kind)
{
  switch (kind)
  {
    case MINSK_SYNTAX_KIND_STAR_TOKEN:
    case MINSK_SYNTAX_KIND_SLASH_TOKEN: return 5;
    case MINSK_SYNTAX_KIND_PLUS_TOKEN:
    case MINSK_SYNTAX_KIND_MINUS_TOKEN: return 4;
    case MINSK_SYNTAX_KIND_EQUALS_EQUALS_TOKEN:
    case MINSK_SYNTAX_KIND_BANG_EQUALS_TOKEN: return 3;
    case MINSK_SYNTAX_KIND_AMPERSAND_AMPERSAND_TOKEN: return 2;
    case MINSK_SYNTAX_KIND_PIPE_PIPE_TOKEN: return 1;
    default: return 0;
  }
}

extern minsk_syntax_facts_precedence_t
minsk_syntax_facts_unary_operator_precedence(minsk_syntax_kind_t kind)
{
  switch (kind)
  {
    case MINSK_SYNTAX_KIND_PLUS_TOKEN:
    case MINSK_SYNTAX_KIND_MINUS_TOKEN:
    case MINSK_SYNTAX_KIND_BANG_TOKEN: return 6;
    default: return 0;
  }
}
typedef struct
{
  string_t text;
  minsk_syntax_kind_t kind;
  UT_hash_handle hh;
} keyword_t;

static keyword_t * g_keywords;

static void add_keyword(string_t text, minsk_syntax_kind_t kind)
{
  keyword_t * kw = malloc(sizeof(*kw));
  kw->text = text;
  kw->kind = kind;
  HASH_ADD_KEYPTR(hh, g_keywords, kw->text.data, kw->text.length, kw);
}

static void init_keywords(void)
{
  add_keyword(STRING_REF("true"), MINSK_SYNTAX_KIND_TRUE_KEYWORD);
  add_keyword(STRING_REF("false"), MINSK_SYNTAX_KIND_FALSE_KEYWORD);
}

extern void minsk_syntax_facts_free_keyword_table(void)
{
  keyword_t * kw;
  keyword_t * tmp;
  HASH_ITER(hh, g_keywords, kw, tmp)
  {
    HASH_DEL(g_keywords, kw);
    free(kw);
  }
}

extern minsk_syntax_kind_t minsk_syntax_facts_keyword_kind(string_t text)
{
  if (g_keywords == NULL)
  {
    init_keywords();
  }

  keyword_t * kw = NULL;
  HASH_FIND(hh, g_keywords, text.data, text.length, kw);
  if (kw != NULL)
  {
    return kw->kind;
  }
  return MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN;
}
