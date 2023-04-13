#include "minsk/code_analysis/syntax/ast/node.h"

#include <arena.h>
#include <minsk-platform/debugger.h>
#include <minsk-string/string.h>
#include <stdbool.h>
#include <stdio.h>

#include "minsk/code_analysis/syntax/ast/expression.h"
#include "minsk/code_analysis/syntax/ast/expressions/binary.h"
#include "minsk/code_analysis/syntax/ast/expressions/literal.h"
#include "minsk/code_analysis/syntax/ast/expressions/parenthesized.h"
#include "minsk/code_analysis/syntax/ast/node_type.h"
#include "minsk/code_analysis/syntax/kind.h"
#include "minsk/runtime/object.h"

extern minsk_syntax_node_t*
minsk_syntax_node_promote(Arena* arena, minsk_syntax_node_t node)
{
  minsk_syntax_node_t* new_node = arena_alloc(arena, sizeof(*new_node));
  *new_node = node;
  return new_node;
}

static void pretty_print(
  Arena* arena,
  minsk_syntax_node_t node,
  FILE* stream,
  string_t indent,
  bool is_last,
  bool do_colors
)
{
  string_t marker = is_last ? STRING_REF("└───") : STRING_REF("├───");
  if (do_colors)
  {
    fprintf(stream, "\x1b[2m");
  }
  fprintf(
    stream,
    STRING_FMT STRING_FMT,
    STRING_ARG(indent),
    STRING_ARG(marker)
  );
  if (do_colors)
  {
    fprintf(
      stream,
      "%s",
      node.type == MINSK_SYNTAX_NODE_TYPE_TOKEN ? "\x1b[0;34m" : "\x1b[0;36m"
    );
  }
  if (node.type == MINSK_SYNTAX_NODE_TYPE_TOKEN)
  {
    string_t disp = minsk_syntax_kind_display_name(arena, node.token.kind);
    fprintf(stream, STRING_FMT, STRING_ARG(disp));
    if (node.token.value.type != MINSK_OBJECT_TYPE_NIL)
    {
      fprintf(stream, " ");
      if (do_colors)
      {
        fprintf(stream, "\x1b[0;35m");
      }
      minsk_object_show(node.token.value, stream);
    }
  }
  else
  {
    string_t disp = minsk_syntax_node_type_display_name(arena, node.type);
    fprintf(stream, STRING_FMT, STRING_ARG(disp));
  }
  if (do_colors)
  {
    fprintf(stream, "\x1b[0m");
  }
  fprintf(stream, "\n");

  indent = string_printf_arena(
    arena,
    STRING_FMT STRING_FMT,
    STRING_ARG(indent),
    STRING_ARG(is_last ? STRING_REF("    ") : STRING_REF("│   "))
  );

  minsk_syntax_node_buf_t children = minsk_syntax_node_children(arena, node);
  for (size_t i = 0; i < children.len; i++)
  {
    pretty_print(
      arena,
      children.ptr[i],
      stream,
      indent,
      i == children.len - 1,
      do_colors
    );
  }
}

extern void
minsk_syntax_node_pretty_print(minsk_syntax_node_t node, FILE* stream)
{
  Arena arena = {0};
  pretty_print(&arena, node, stream, EMPTY_STRING, true, true);
  arena_free(&arena);
}

extern minsk_syntax_node_buf_t
minsk_syntax_node_children(Arena* arena, minsk_syntax_node_t node)
{
  switch (node.type)
  {
    case MINSK_SYNTAX_NODE_TYPE_BINARY_EXPRESSION:
    {
      minsk_syntax_expression_binary_t b = node.expression.binary;
      return BUF_LIT_ARENA(
        arena,
        minsk_syntax_node_buf_t,
        *b.left,
        MINSK_SYNTAX_NODE_TOKEN(b.op),
        *b.right
      );
    }
    case MINSK_SYNTAX_NODE_TYPE_LITERAL_EXPRESSION:
    {
      minsk_syntax_expression_literal_t l = node.expression.literal;
      return BUF_LIT_ARENA(
        arena,
        minsk_syntax_node_buf_t,
        MINSK_SYNTAX_NODE_TOKEN(l.literal_token)
      );
    }
    case MINSK_SYNTAX_NODE_TYPE_PARENTHESIZED_EXPRESSION:
    {
      minsk_syntax_expression_parenthesized_t p = node.expression.parenthesized;
      return BUF_LIT_ARENA(
        arena,
        minsk_syntax_node_buf_t,
        MINSK_SYNTAX_NODE_TOKEN(p.open_parenthesis_token),
        *p.expression,
        MINSK_SYNTAX_NODE_TOKEN(p.close_parenthesis_token)
      );
    }
    case MINSK_SYNTAX_NODE_TYPE_TOKEN:
    {
      return (minsk_syntax_node_buf_t){0};
    }
  }

  DEBUGGER_FATAL("invalid node type %d", node.type);
}
