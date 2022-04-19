#include "minsk/runtime/object.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

const char* object_kind_to_string(object_kind_t kind) {
  switch (kind) {
#define X(x) \
  case object_kind_##x: \
    return #x;
    OBJECT_KINDS_X
#undef X
  default:
    assert(false && "corrupt object kind");
  }
}

object_t* integer_new(int value) {
  integer_t* result = malloc(sizeof(integer_t));
  result->base.kind = object_kind_integer;
  result->value = value;
  return (object_t*)result;
}

void object_print(object_t* object, FILE* stream) {
  switch (object->kind) {
  case object_kind_integer:
    fprintf(stream, "%d", ((integer_t*)object)->value);
    break;
  case object_kind_boolean:
    fprintf(stream, "%s", ((boolean_t*)object)->value ? "true" : "false");
    break;
  case object_kind_null:
  default:
    assert(false && "corrupt object");
  }
}
void object_free(object_t* object) {
  free(object);
}
bool object_equals(object_t* a, object_t* b) {
  if (a->kind != b->kind) {
    return false;
  }
  switch (a->kind) {
  case object_kind_integer:
    return ((integer_t*)a)->value == ((integer_t*)b)->value;
  case object_kind_boolean:
    return ((boolean_t*)a)->value == ((boolean_t*)b)->value;
  case object_kind_null:
  default:
    assert(false && "corrupt object");
  }
}
object_t* object_copy(object_t* object) {
  if (object == NULL) {
    return NULL;
  }
  switch (object->kind) {
  case object_kind_integer:
    return integer_new(((integer_t*)object)->value);
  case object_kind_boolean:
    return boolean_new(((boolean_t*)object)->value);
  case object_kind_null:
  default:
    assert(false && "corrupt object");
  }
}
object_t* boolean_new(bool value) {
  boolean_t* result = malloc(sizeof(boolean_t));
  result->base.kind = object_kind_boolean;
  result->value = value;
  return (object_t*)result;
}
