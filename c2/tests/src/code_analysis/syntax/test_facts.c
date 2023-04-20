#include <arena.h>
#include <minsk/code_analysis/syntax/facts.h>
#include <minsk/code_analysis/syntax/kind.h>
#include <minsk/code_analysis/syntax/tree.h>
#include <tau/tau.h>

#include "minsk-test/tau-ext.h"

static void
test_get_text_round_trips(
  Arena * arena,
  string_t name,
  minsk_syntax_kind_t kind
)
{
  string_t text = minsk_syntax_facts_get_text(kind);
  minsk_syntax_token_buf_t tokens = minsk_syntax_tree_parse_tokens(arena, text);
  EXTREQUIRE(
    tokens.len == 1,
    STRING_FMT ": %zu tokens != 1",
    STRING_ARG(name),
    tokens.len
  );
  minsk_syntax_token_t token = tokens.ptr[0];
  EXTCHECK(
    token.kind == kind,
    STRING_FMT ": wrong kind " STRING_FMT ", expected " STRING_FMT,
    STRING_ARG(name),
    STRING_ARG(minsk_syntax_kind_display_name(arena, token.kind)),
    STRING_ARG(minsk_syntax_kind_display_name(arena, kind))
  );
  EXTCHECK(
    STRING_EQUAL(token.text, text),
    STRING_FMT ": wrong text '" STRING_FMT "', expected '" STRING_FMT "'",
    STRING_ARG(name),
    STRING_ARG(token.text),
    STRING_ARG(text)
  );
}

TEST(facts, get_text_round_trips)
{
  Arena test_arena = {0};
  for (minsk_syntax_kind_t kind = 0; kind < MINSK_SYNTAX_KIND_COUNT; kind++)
  {
    if (minsk_syntax_facts_get_text(kind).length > 0)
    {
      string_t name = minsk_syntax_kind_display_name(&test_arena, kind);
      test_get_text_round_trips(&test_arena, name, kind);
    }
  }
  arena_free(&test_arena);
}
