#pragma once

#include <stdbool.h>
#include <stdio.h>
#define OBJECT_KINDS_X                                                         \
  X(integer)                                                                   \
  X(boolean)

typedef enum {
#define X(x) object_kind_##x,
  OBJECT_KINDS_X
#undef X
} object_kind_t;

typedef struct {
  object_kind_t kind;
} object_t;

typedef struct {
  object_t base;
  int value;
} integer_t;

typedef struct {
  object_t base;
  bool value;
} boolean_t;

object_t *integer_new(int value);
void object_print(object_t *object, FILE *stream);
