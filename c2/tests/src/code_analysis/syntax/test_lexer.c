#include <minsk-string/string.h>
#include <minsk-test/tau-ext.h>
#include <minsk/code_analysis/syntax/kind.h>
#include <minsk/code_analysis/syntax/tree.h>
#include <minsk/data_structures/buf.h>
#include <tau/tau.h>

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

#define ST(kind, text_lit)   \
 {                           \
  kind, STRING_REF(text_lit) \
 }

typedef BUF_T(simple_token_t) simple_token_buf_t;

static simple_token_t TOKENS[] = {
  ST(MINSK_SYNTAX_KIND_PLUS_TOKEN, "+"),
  ST(MINSK_SYNTAX_KIND_MINUS_TOKEN, "-"),
  ST(MINSK_SYNTAX_KIND_STAR_TOKEN, "*"),
  ST(MINSK_SYNTAX_KIND_SLASH_TOKEN, "/"),
  ST(MINSK_SYNTAX_KIND_OPEN_PARENTHESIS_TOKEN, "("),
  ST(MINSK_SYNTAX_KIND_CLOSE_PARENTHESIS_TOKEN, ")"),
  ST(MINSK_SYNTAX_KIND_BANG_TOKEN, "!"),
  ST(MINSK_SYNTAX_KIND_AMPERSAND_AMPERSAND_TOKEN, "&&"),
  ST(MINSK_SYNTAX_KIND_PIPE_PIPE_TOKEN, "||"),
  ST(MINSK_SYNTAX_KIND_BANG_EQUALS_TOKEN, "!="),
  ST(MINSK_SYNTAX_KIND_EQUALS_EQUALS_TOKEN, "=="),
  ST(MINSK_SYNTAX_KIND_EQUALS_TOKEN, "="),
  ST(MINSK_SYNTAX_KIND_FALSE_KEYWORD, "false"),
  ST(MINSK_SYNTAX_KIND_TRUE_KEYWORD, "true"),

  ST(MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN, "a"),
  ST(MINSK_SYNTAX_KIND_IDENTIFIER_TOKEN, "abc"),
  ST(MINSK_SYNTAX_KIND_NUMBER_TOKEN, "1"),
  ST(MINSK_SYNTAX_KIND_NUMBER_TOKEN, "123"),
};

static simple_token_t SEPARATORS[] = {
  ST(MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, " "),
  ST(MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "  "),
  ST(MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "\r"),
  ST(MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "\n"),
  ST(MINSK_SYNTAX_KIND_WHITESPACE_TOKEN, "\r\n"),
};

static simple_token_buf_t
get_all_tokens(Arena * arena)
{
  simple_token_buf_t buf = BUF_INIT;
  BUF_APPEND_ARENA(arena, &buf, BUF_ARRAY(simple_token_buf_t, TOKENS));
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

TEST(lexer, lexes_token)
{
  Arena test_arena = {0};
  simple_token_buf_t tokens = get_all_tokens(&test_arena);
  for (size_t i = 0; i < tokens.len; i++)
  {
    simple_token_t t = tokens.ptr[i];
    string_t name = string_printf_arena(
      &test_arena,
      "(" STRING_FMT " '" STRING_FMT "')",
      STRING_ARG(minsk_syntax_kind_display_name(&test_arena, t.kind)),
      STRING_ARG(format_escapes(&test_arena, t.text))
    );
    test_lexes_token(&test_arena, name, t);
  }

  arena_free(&test_arena);
}
