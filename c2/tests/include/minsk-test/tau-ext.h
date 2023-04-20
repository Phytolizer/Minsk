#pragma once

#include <tau/tau.h>

#define EXTTAU_CHECKREQUIRE(cond, failOrAbort, macroName, ...) \
 do                                                            \
 {                                                             \
  if (!(cond))                                                 \
  {                                                            \
   tauPrintf("%s:%u: ", __FILE__, __LINE__);                   \
   tauColouredPrintf(TAU_COLOUR_BRIGHTRED_, __VA_ARGS__);      \
   printf("\n");                                               \
   printf("The following assertion failed: \n");               \
   tauColouredPrintf(                                          \
     TAU_COLOUR_BRIGHTCYAN_,                                   \
     "    %s( %s )\n",                                         \
     #macroName,                                               \
     #cond                                                     \
   );                                                          \
   failOrAbort;                                                \
   if (shouldAbortTest)                                        \
   {                                                           \
    return;                                                    \
   }                                                           \
  }                                                            \
 } while (0)

#define EXTREQUIRE(cond, ...) \
 EXTTAU_CHECKREQUIRE(cond, TAU_ABORT_IF_INSIDE_TESTSUITE, REQUIRE, __VA_ARGS__)
#define EXTCHECK(cond, ...) \
 EXTTAU_CHECKREQUIRE(cond, TAU_FAIL_IF_INSIDE_TESTSUITE, CHECK, __VA_ARGS__)
