#pragma once

#include <minsk-string/string.h>
#include <stdint.h>

#include "minsk/code_analysis/syntax/kind.h"

typedef int_fast8_t minsk_syntax_facts_precedence_t;

extern minsk_syntax_facts_precedence_t
minsk_syntax_facts_binary_operator_precedence(minsk_syntax_kind_t kind);

extern minsk_syntax_facts_precedence_t
minsk_syntax_facts_unary_operator_precedence(minsk_syntax_kind_t kind);

extern minsk_syntax_kind_t
minsk_syntax_facts_keyword_kind(string_t text);

extern void
minsk_syntax_facts_free_keyword_table(void);

extern string_t
minsk_syntax_facts_get_text(minsk_syntax_kind_t kind);
