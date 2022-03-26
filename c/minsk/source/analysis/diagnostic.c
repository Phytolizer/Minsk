#include "minsk/analysis/diagnostic.h"
#include <stddef.h>

diagnostic_t diagnostic_new(text_span_t span, sds message) {
  return (diagnostic_t){.span = span, .message = message};
}

void diagnostic_free(diagnostic_t *diagnostic) {
  if (diagnostic == NULL || diagnostic->message == NULL) {
    return;
  }
  sdsfree(diagnostic->message);
  diagnostic->message = NULL;
}
