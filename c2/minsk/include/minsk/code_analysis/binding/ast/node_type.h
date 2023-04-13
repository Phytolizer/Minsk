#pragma once

#include <arena.h>
#include <minsk-string/string.h>

#include "minsk/meta/concat.h"

#define MINSK_PREFIX_BOUND_NODE MINSK_BOUND_NODE_TYPE_

typedef enum
{
#define X(x) MINSK_CONCAT(MINSK_PREFIX_BOUND_NODE, x),
#include "./private/node_types.xmacro"
#undef X
} minsk_bound_node_type_t;

extern string_t
minsk_bound_node_type_display_name(Arena * arena, minsk_bound_node_type_t type);
