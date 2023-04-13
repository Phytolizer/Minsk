#pragma once

#ifndef NDEBUG
 #define DEBUG
#endif

#ifdef DEBUG
 #include <stdio.h>
 #include <stdlib.h>

 #ifdef _WIN32
  #define DEBUGGER_TRAP() __debugbreak()
 #else
  #include <signal.h>
  #define DEBUGGER_TRAP() raise(SIGTRAP)
 #endif

 #define DEBUGGER_ABORT() abort()

 #define DEBUGGER(fmt, ...)                              \
  do                                                     \
  {                                                      \
   fprintf(stderr, fmt "\n" __VA_OPT__(, ) __VA_ARGS__); \
   DEBUGGER_TRAP();                                      \
  } while (0)

 #define DEBUGGER_FATAL(...) \
  do                         \
  {                          \
   DEBUGGER(__VA_ARGS__);    \
   DEBUGGER_ABORT();         \
  } while (0)

 #define DEBUGGER_ASSERT(condition, ...) \
  do                                     \
  {                                      \
   if (!(condition))                     \
   {                                     \
    DEBUGGER_FATAL(__VA_ARGS__);         \
   }                                     \
  } while (0)
#else
 #define DEBUGGER_TRAP()
 #define DEBUGGER_ABORT()
 #define DEBUGGER(...)
 #define DEBUGGER_FATAL(...)
 #define DEBUGGER_ASSERT(...)
#endif
