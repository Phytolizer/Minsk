#include <bstrlib.h>
#include <linenoise.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "mc/cwd.h"

#define HISTORY_PATH ".minsk-history"

extern int main(int argc, char** argv)
{
  if (!mc_set_cwd_from_meson())
  {
    return 1;
  }

  linenoiseHistoryLoad(HISTORY_PATH);

  while (true)
  {
    const char* prompt = "\x1b[32m»\x1b[0m ";
    char* raw_line = linenoise(prompt);
    if (raw_line == NULL)
    {
      break;
    }
    linenoiseHistoryAdd(raw_line);
    struct tagbstring line;
    cstr2tbstr(line, raw_line);

    if (biseqStatic(&line, "1 + 2 * 3"))
    {
      fputs("7\n", stderr);
    }
    else
    {
      fputs("ERROR: Invalid expression!\n", stderr);
    }
    free(raw_line);
  }

  linenoiseHistorySave(HISTORY_PATH);
  linenoiseHistoryFree();
  return 0;
}
