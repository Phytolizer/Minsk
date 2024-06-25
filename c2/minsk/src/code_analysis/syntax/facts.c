#include "minsk/code_analysis/syntax/facts.h"

#include <minsk-string/string.h>

#include "minsk/code_analysis/syntax/kind.h"
#include "minsk/hash/fnv.h"

static uint64_t
keyword_hash(string_t key)
{
  return minsk_hash_fnv64a(key.data, key.length);
}

static bool
keyword_cmpr(string_t a, string_t b)
{
  return STRING_EQUAL(a, b);
}

#define NAME keyword_map
#define KEY_TY string_t
#define VAL_TY minsk_syntax_kind_t
#define HASH_FN keyword_hash
#define CMPR_FN keyword_cmpr
#include <verstable.h>

typedef keyword_map keyword_map_t;

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

static keyword_map_t g_keywords;

static void
add_keyword(string_t text, minsk_syntax_kind_t kind)
{
  keyword_map_insert(&g_keywords, text, kind);
}

static void
init_keywords(void)
{
  keyword_map_init(&g_keywords);
  add_keyword(STRING_REF("true"), MINSK_SYNTAX_KIND_TRUE_KEYWORD);
  add_keyword(STRING_REF("false"), MINSK_SYNTAX_KIND_FALSE_KEYWORD);
}

extern void
minsk_syntax_facts_free_keyword_table(void)
{
  keyword_map_cleanup(&g_keywords);
}

extern string_t
minsk_syntax_facts_get_text(minsk_syntax_kind_t kind)
{
  switch (kind)
  {
    case MINSK_SYNTAX_KIND_PLUS_TOKEN: return STRING_REF("+");
    case MINSK_SYNTAX_KIND_MINUS_TOKEN: return STRING_REF("-");
    case MINSK_SYNTAX_KIND_STAR_TOKEN: return STRING_REF("*");
    case MINSK_SYNTAX_KIND_SLASH_TOKEN: return STRING_REF("/");
    case MINSK_SYNTAX_KIND_BANG_TOKEN: return STRING_REF("!");
    case MINSK_SYNTAX_KIND_EQUALS_TOKEN: return STRING_REF("=");
    case MINSK_SYNTAX_KIND_AMPERSAND_AMPERSAND_TOKEN: return STRING_REF("&&");
    case MINSK_SYNTAX_KIND_PIPE_PIPE_TOKEN: return STRING_REF("||");
    case MINSK_SYNTAX_KIND_EQUALS_EQUALS_TOKEN: return STRING_REF("==");
    case MINSK_SYNTAX_KIND_BANG_EQUALS_TOKEN: return STRING_REF("!=");
    case MINSK_SYNTAX_KIND_OPEN_PARENTHESIS_TOKEN: return STRING_REF("(");
    case MINSK_SYNTAX_KIND_CLOSE_PARENTHESIS_TOKEN: return STRING_REF(")");
    case MINSK_SYNTAX_KIND_FALSE_KEYWORD: return STRING_REF("false");
    case MINSK_SYNTAX_KIND_TRUE_KEYWORD: return STRING_REF("true");
    default: return EMPTY_STRING;
  }
}

static bool
binary_operator_kind_next(minsk_syntax_kind_iterator_t * it)
{
  while (it->curr < MINSK_SYNTAX_KIND_COUNT)
  {
    it->curr++;
    minsk_syntax_facts_precedence_t prec =
      minsk_syntax_facts_binary_operator_precedence(it->curr);
    if (prec > 0)
    {
      return true;
    }
  }
  return false;
}

extern minsk_syntax_kind_iterator_t
minsk_syntax_facts_get_binary_operator_kinds(void)
{
  minsk_syntax_kind_iterator_t it = {.next = binary_operator_kind_next};
  return it;
}

static bool
unary_operator_kind_next(minsk_syntax_kind_iterator_t * it)
{
  while (it->curr < MINSK_SYNTAX_KIND_COUNT)
  {
    it->curr++;
    minsk_syntax_facts_precedence_t prec =
      minsk_syntax_facts_unary_operator_precedence(it->curr);
    if (prec > 0)
    {
      return true;
    }
  }
  return false;
}

extern minsk_syntax_kind_iterator_t
minsk_syntax_facts_get_unary_operator_kinds(void)
{
  minsk_syntax_kind_iterator_t it = {.next = unary_operator_kind_next};
  return it;
}

extern minsk_syntax_kind_t
minsk_syntax_facts_keyword_kind(string_t text)
{
  if (g_keywords.buckets_mask == 0)
  {
    init_keywords();
  }

  keyword_map_itr kw = keyword_map_get(&g_keywords, text);
  if (!keyword_map_is_end(kw))
  {
    return kw.data->val;
  }
  return MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN;
}
