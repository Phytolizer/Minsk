#include "minsk_test/esc.h"
#include <ctype.h>
#include <stdio.h>

sds esc(const char *str) {
  // Convert special characters to escape codes.
  sds result = sdsempty();
  for (size_t i = 0; str[i] != '\0'; i++) {
    switch (str[i]) {
    case '\n':
      result = sdscat(result, "\\n");
      break;
    case '\r':
      result = sdscat(result, "\\r");
      break;
    case '\t':
      result = sdscat(result, "\\t");
      break;
    case '\v':
      result = sdscat(result, "\\v");
      break;
    case '\f':
      result = sdscat(result, "\\f");
      break;
    case '\b':
      result = sdscat(result, "\\b");
      break;
    case '\a':
      result = sdscat(result, "\\a");
      break;
    case '\\':
      result = sdscat(result, "\\\\");
      break;
    case '\'':
      result = sdscat(result, "\\'");
      break;
    case '\"':
      result = sdscat(result, "\\\"");
      break;
    default:
      if (isprint(str[i])) {
        result = sdscatlen(result, &str[i], 1);
      } else {
        // compute hex code
        char hex[8];
        snprintf(hex, sizeof(hex), "\\x%02x", (unsigned char)str[i]);
        result = sdscat(result, hex);
      }
      break;
    }
  }

  return result;
}
