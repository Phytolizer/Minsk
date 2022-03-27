#pragma once

#include "minsk/analysis/syntax/kind.h"
#include <stdbool.h>

typedef struct {
  syntax_kind_t kind;
  bool present;
} syntax_kind_set_bucket_t;

typedef struct {
  syntax_kind_set_bucket_t *data;
  size_t length;
  size_t capacity;
} syntax_kind_set_t;

void syntax_kind_set_init(syntax_kind_set_t *set);
void syntax_kind_set_insert(syntax_kind_set_t *set, syntax_kind_t kind);
bool syntax_kind_set_contains(const syntax_kind_set_t *set, syntax_kind_t kind);
void syntax_kind_set_free(syntax_kind_set_t *set);
