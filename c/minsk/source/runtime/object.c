#include "minsk/runtime/object.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

object_t *integer_new(int value) {
  integer_t *result = malloc(sizeof(integer_t));
  result->base.kind = object_kind_integer;
  result->value = value;
  return (object_t *)result;
}

void object_print(object_t *object, FILE *stream) {
  switch (object->kind) {
  case object_kind_integer:
    fprintf(stream, "%d", ((integer_t *)object)->value);
    break;
  case object_kind_boolean:
    fprintf(stream, "%s", ((boolean_t *)object)->value ? "true" : "false");
    break;
  default:
    assert(false && "corrupt object");
  }
}
