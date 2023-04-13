#pragma once

#include <stdint.h>

#include "./kind.h"

typedef int_fast8_t minsk_syntax_facts_precedence_t;

extern minsk_syntax_facts_precedence_t
minsk_syntax_facts_binary_operator_precedence(minsk_syntax_kind_t kind);

extern minsk_syntax_facts_precedence_t
minsk_syntax_facts_unary_operator_precedence(minsk_syntax_kind_t kind);
