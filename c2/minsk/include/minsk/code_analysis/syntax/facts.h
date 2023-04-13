#pragma once

#include <minsk-string/string.h>
#include <stdint.h>

#include "./kind.h"

typedef int_fast8_t minsk_syntax_facts_precedence_t;

extern minsk_syntax_facts_precedence_t
minsk_syntax_facts_binary_operator_precedence(minsk_syntax_kind_t kind);

extern minsk_syntax_facts_precedence_t
minsk_syntax_facts_unary_operator_precedence(minsk_syntax_kind_t kind);

extern minsk_syntax_kind_t minsk_syntax_facts_keyword_kind(string_t text);

extern void minsk_syntax_facts_free_keyword_table(void);
