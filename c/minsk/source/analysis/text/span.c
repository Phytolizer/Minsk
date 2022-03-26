#include "minsk/analysis/text/span.h"

size_t text_span_end(text_span_t span) { return span.start + span.length; }
