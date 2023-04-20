#include "minsk/code_analysis/binding/ast/node.h"

#include "arena.h"

extern minsk_bound_node_t *
minsk_bound_node_promote(Arena * arena, minsk_bound_node_t node)
{
  minsk_bound_node_t * new_node = arena_alloc(arena, sizeof(*new_node));
  *new_node = node;
  return new_node;
}
