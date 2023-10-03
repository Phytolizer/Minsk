#include <arena.h>
#include <minsk-string/string.h>
#include <minsk/code_analysis/syntax/facts.h>
#include <minsk/code_analysis/syntax/kind.h>
#include <minsk/code_analysis/syntax/token.h>
#include <minsk/code_analysis/syntax/tree.h>
#include <minsk/data_structures/buf.h>
#include <minsk/meta/concat.h>
#include <stdbool.h>
#include <stddef.h>
#include <tau/tau.h>

#include "minsk-test/tau-ext.h"

static string_t
format_escapes(Arena * arena, string_t text)
{
  string_t result = EMPTY_STRING;
  for (size_t i = 0; i < text.length; i++)
  {
    char byte = text.data[i];
    switch (byte)
    {
      // this table assumes ASCII and/or UTF-8
      case '\n': string_append_arena(arena, &result, STRING_REF("\\n")); break;
      case '\r': string_append_arena(arena, &result, STRING_REF("\\r")); break;
      case '\t': string_append_arena(arena, &result, STRING_REF("\\t")); break;
      case '\\': string_append_arena(arena, &result, STRING_REF("\\\\")); break;
      case '\'': string_append_arena(arena, &result, STRING_REF("\\'")); break;
      case ' ':
      case '!':
      case '#' ... '&':
      case '(' ... '[':
      case ']' ... '~': string_push_arena(arena, &result, byte); break;
      default: string_append_printf_arena(arena, &result, "\\x%02x", byte);
    }
  }
  return result;
}

typedef struct
{
  minsk_syntax_kind_t kind;
  string_t text;
} simple_token_t;

#define ST(kind, text_lit)     \
  {                            \
    kind, STRING_REF(text_lit) \
  }

typedef BUF_T(simple_token_t) simple_token_buf_t;

static simple_token_buf_t
get_fixed_tokens(Arena * arena)
{
  simple_token_buf_t buf = BUF_INIT;
  for (minsk_syntax_kind_t kind = 0; kind < MINSK_SYNTAX_KIND_COUNT; ++kind)
  {
    string_t text = minsk_syntax_facts_get_text(kind);
    if (text.length > 0)
    {
      simple_token_t tok = {kind, text};
      BUF_PUSH_ARENA(arena, &buf, tok);
    }
  }
  return buf;
}

static simple_token_t const DYNAMIC_TOKENS[] = {
  ST(MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN, "a"),
  ST(MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN, "abc"),
  ST(MINSK_SYNTAX_KIND_NUMBER_TOKEN, "1"),
  ST(MINSK_SYNTAX_KIND_NUMBER_TOKEN, "123"),
};

static simple_token_t const SEPARATORS[] = {
  ST(MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, " "),
  ST(MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "  "),
  ST(MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "\r"),
  ST(MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "\n"),
  ST(MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "\r\n"),
};

static simple_token_buf_t
get_tokens(Arena * arena)
{
  simple_token_buf_t buf = BUF_INIT;
  BUF_APPEND_ARENA(arena, &buf, get_fixed_tokens(arena));
  BUF_APPEND_ARENA(arena, &buf, BUF_ARRAY(simple_token_buf_t, DYNAMIC_TOKENS));
  return buf;
}

static simple_token_buf_t
get_all_tokens(Arena * arena)
{
  simple_token_buf_t buf = BUF_INIT;
  BUF_APPEND_ARENA(arena, &buf, get_tokens(arena));
  BUF_APPEND_ARENA(arena, &buf, BUF_ARRAY(simple_token_buf_t, SEPARATORS));
  return buf;
}

static void
test_lexes_token(Arena * arena, string_t name, simple_token_t t)
{
  minsk_syntax_token_buf_t tokens =
    minsk_syntax_tree_parse_tokens(arena, t.text);
  if (tokens.len != 1)
  {
    EXTREQUIRE(
      false,
      STRING_FMT ": %zu tokens != 1",
      STRING_ARG(name),
      tokens.len
    );
  }

  minsk_syntax_token_t token = tokens.ptr[0];
  EXTCHECK(
    token.kind == t.kind,
    STRING_FMT ": wrong kind " STRING_FMT ", expected " STRING_FMT,
    STRING_ARG(name),
    STRING_ARG(minsk_syntax_kind_display_name(arena, token.kind)),
    STRING_ARG(minsk_syntax_kind_display_name(arena, t.kind))
  );
  EXTCHECK(
    STRING_EQUAL(token.text, t.text),
    STRING_FMT ": wrong text '" STRING_FMT "', expected '" STRING_FMT "'",
    STRING_ARG(name),
    STRING_ARG(format_escapes(arena, token.text)),
    STRING_ARG(format_escapes(arena, t.text))
  );
}

static string_t
simple_token_stringify(Arena * arena, simple_token_t t)
{
  return string_printf_arena(
    arena,
    "(" STRING_FMT " '" STRING_FMT "')",
    STRING_ARG(minsk_syntax_kind_display_name(arena, t.kind)),
    STRING_ARG(format_escapes(arena, t.text))
  );
}

TEST(lexer, lexes_token)
{
  Arena test_arena = {0};
  simple_token_buf_t tokens = get_all_tokens(&test_arena);
  for (size_t i = 0; i < tokens.len; i++)
  {
    simple_token_t t = tokens.ptr[i];
    string_t name = simple_token_stringify(&test_arena, t);
    test_lexes_token(&test_arena, name, t);
  }

  arena_free(&test_arena);
}

static const char * const SYNTAX_KIND_RAW_NAMES[] = {
#define X(x) [MINSK_CONCAT(MINSK_PREFIX_SYNTAX_KIND, x)] = #x,
#include "minsk/code_analysis/syntax/private/kinds.xmacro"
#undef X
};

static bool
requires_separator(minsk_syntax_kind_t k1, minsk_syntax_kind_t k2)
{
  bool kw1 = string_endswith(
    STRING_REF_FROM_C(SYNTAX_KIND_RAW_NAMES[k1]),
    STRING_REF("_KEYWORD")
  );
  bool kw2 = string_endswith(
    STRING_REF_FROM_C(SYNTAX_KIND_RAW_NAMES[k2]),
    STRING_REF("_KEYWORD")
  );

  bool idstart = kw1 || k1 == MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN;
  bool idcontinue = kw2 || k2 == MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN ||
                    k2 == MINSK_SYNTAX_KIND_NUMBER_TOKEN;

  if (idstart && idcontinue)
  {
    return true;
  }

  if (k1 == MINSK_SYNTAX_KIND_NUMBER_TOKEN && k2 == MINSK_SYNTAX_KIND_NUMBER_TOKEN)
  {
    return true;
  }

  bool equals_sensitive =
    k1 == MINSK_SYNTAX_KIND_EQUALS_TOKEN || k1 == MINSK_SYNTAX_KIND_BANG_TOKEN;
  bool is_equals = k2 == MINSK_SYNTAX_KIND_EQUALS_TOKEN ||
                   k2 == MINSK_SYNTAX_KIND_EQUALS_EQUALS_TOKEN;
  if (equals_sensitive && is_equals)
  {
    return true;
  }

  return false;
}

static void
test_lexes_token_pair(
  Arena * arena,
  string_t name,
  simple_token_t t1,
  simple_token_t t2
)
{
  minsk_syntax_token_buf_t tokens = minsk_syntax_tree_parse_tokens(
    arena,
    string_printf_arena(
      arena,
      STRING_FMT STRING_FMT,
      STRING_ARG(t1.text),
      STRING_ARG(t2.text)
    )
  );
  if (tokens.len != 2)
  {
    EXTREQUIRE(
      false,
      STRING_FMT ": %zu tokens != 2",
      STRING_ARG(name),
      tokens.len
    );
  }

  minsk_syntax_token_t tok1 = tokens.ptr[0];
  minsk_syntax_token_t tok2 = tokens.ptr[1];
  EXTCHECK(
    tok1.kind == t1.kind,
    STRING_FMT ": wrong kind " STRING_FMT ", expected " STRING_FMT,
    STRING_ARG(name),
    STRING_ARG(minsk_syntax_kind_display_name(arena, tok1.kind)),
    STRING_ARG(minsk_syntax_kind_display_name(arena, t1.kind))
  );
  EXTCHECK(
    STRING_EQUAL(tok1.text, t1.text),
    STRING_FMT ": wrong text '" STRING_FMT "', expected '" STRING_FMT "'",
    STRING_ARG(name),
    STRING_ARG(format_escapes(arena, tok1.text)),
    STRING_ARG(format_escapes(arena, t1.text))
  );
  EXTCHECK(
    tok2.kind == t2.kind,
    STRING_FMT ": wrong kind " STRING_FMT ", expected " STRING_FMT,
    STRING_ARG(name),
    STRING_ARG(minsk_syntax_kind_display_name(arena, tok2.kind)),
    STRING_ARG(minsk_syntax_kind_display_name(arena, t2.kind))
  );
  EXTCHECK(
    STRING_EQUAL(tok2.text, t2.text),
    STRING_FMT ": wrong text '" STRING_FMT "', expected '" STRING_FMT "'",
    STRING_ARG(name),
    STRING_ARG(format_escapes(arena, tok2.text)),
    STRING_ARG(format_escapes(arena, t2.text))
  );
}

TEST(lexer, lexes_token_pair)
{
  Arena test_arena = {0};
  simple_token_buf_t tokens = get_tokens(&test_arena);
  for (size_t i = 0; i < tokens.len; i++)
  {
    simple_token_t t1 = tokens.ptr[i];
    string_t t1_name = simple_token_stringify(&test_arena, t1);
    for (size_t j = 0; j < tokens.len; j++)
    {
      simple_token_t t2 = tokens.ptr[j];
      if (!requires_separator(t1.kind, t2.kind))
      {
        string_t t2_name = simple_token_stringify(&test_arena, t2);
        string_t name = string_printf_arena(
          &test_arena,
          STRING_FMT ", " STRING_FMT,
          STRING_ARG(t1_name),
          STRING_ARG(t2_name)
        );
        test_lexes_token_pair(&test_arena, name, t1, t2);
      }
    }
  }
  arena_free(&test_arena);
}

static void
test_lexes_token_pair_with_separator(
  Arena * arena,
  string_t name,
  simple_token_t t1,
  simple_token_t sep,
  simple_token_t t2
)
{
  minsk_syntax_token_buf_t tokens = minsk_syntax_tree_parse_tokens(
    arena,
    string_printf_arena(
      arena,
      STRING_FMT STRING_FMT STRING_FMT,
      STRING_ARG(t1.text),
      STRING_ARG(sep.text),
      STRING_ARG(t2.text)
    )
  );
  if (tokens.len != 3)
  {
    EXTREQUIRE(
      false,
      STRING_FMT ": %zu tokens != 3",
      STRING_ARG(name),
      tokens.len
    );
  }

  minsk_syntax_token_t tok1 = tokens.ptr[0];
  minsk_syntax_token_t tsep = tokens.ptr[1];
  minsk_syntax_token_t tok2 = tokens.ptr[2];
  EXTCHECK(
    tok1.kind == t1.kind,
    STRING_FMT ": wrong kind " STRING_FMT ", expected " STRING_FMT,
    STRING_ARG(name),
    STRING_ARG(minsk_syntax_kind_display_name(arena, tok1.kind)),
    STRING_ARG(minsk_syntax_kind_display_name(arena, t1.kind))
  );
  EXTCHECK(
    STRING_EQUAL(tok1.text, t1.text),
    STRING_FMT ": wrong text '" STRING_FMT "', expected '" STRING_FMT "'",
    STRING_ARG(name),
    STRING_ARG(format_escapes(arena, tok1.text)),
    STRING_ARG(format_escapes(arena, t1.text))
  );
  EXTCHECK(
    tsep.kind == sep.kind,
    STRING_FMT ": wrong kind " STRING_FMT ", expected " STRING_FMT,
    STRING_ARG(name),
    STRING_ARG(minsk_syntax_kind_display_name(arena, tsep.kind)),
    STRING_ARG(minsk_syntax_kind_display_name(arena, sep.kind))
  );
  EXTCHECK(
    STRING_EQUAL(tsep.text, sep.text),
    STRING_FMT ": wrong text '" STRING_FMT "', expected '" STRING_FMT "'",
    STRING_ARG(name),
    STRING_ARG(format_escapes(arena, tsep.text)),
    STRING_ARG(format_escapes(arena, sep.text))
  );
  EXTCHECK(
    tok2.kind == t2.kind,
    STRING_FMT ": wrong kind " STRING_FMT ", expected " STRING_FMT,
    STRING_ARG(name),
    STRING_ARG(minsk_syntax_kind_display_name(arena, tok2.kind)),
    STRING_ARG(minsk_syntax_kind_display_name(arena, t2.kind))
  );
  EXTCHECK(
    STRING_EQUAL(tok2.text, t2.text),
    STRING_FMT ": wrong text '" STRING_FMT "', expected '" STRING_FMT "'",
    STRING_ARG(name),
    STRING_ARG(format_escapes(arena, tok2.text)),
    STRING_ARG(format_escapes(arena, t2.text))
  );
}

TEST(lexer, lexes_token_pair_with_separator)
{
  Arena test_arena = {0};
  simple_token_buf_t tokens = get_tokens(&test_arena);
  simple_token_buf_t separators = BUF_ARRAY(simple_token_buf_t, SEPARATORS);
  for (size_t i = 0; i < tokens.len; i++)
  {
    simple_token_t t1 = tokens.ptr[i];
    string_t t1_name = simple_token_stringify(&test_arena, t1);
    for (size_t j = 0; j < tokens.len; j++)
    {
      simple_token_t t2 = tokens.ptr[j];
      if (requires_separator(t1.kind, t2.kind))
      {
        string_t t2_name = simple_token_stringify(&test_arena, t2);
        for (size_t k = 0; k < separators.len; k++)
        {
          simple_token_t sep = separators.ptr[k];
          string_t sep_name = simple_token_stringify(&test_arena, sep);
          string_t name = string_printf_arena(
            &test_arena,
            STRING_FMT ", " STRING_FMT ", " STRING_FMT,
            STRING_ARG(t1_name),
            STRING_ARG(sep_name),
            STRING_ARG(t2_name)
          );
          test_lexes_token_pair_with_separator(&test_arena, name, t1, sep, t2);
        }
      }
    }
  }
  arena_free(&test_arena);
}
