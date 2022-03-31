#pragma once

#include "kind.h"

int facts_binary_operator_precedence(syntax_kind_t kind);
int facts_unary_operator_precedence(syntax_kind_t kind);
syntax_kind_t facts_keyword_kind(const char* text);
const char* facts_get_text(syntax_kind_t kind);
