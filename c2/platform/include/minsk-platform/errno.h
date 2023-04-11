#pragma once

#ifdef _WIN32
#include <windows.h>
typedef DWORD minsk_errno_t;
#define MINSK_NO_ERROR 0
#else
typedef int minsk_errno_t;
#define MINSK_NO_ERROR 0
#endif

extern void minsk_report_error(minsk_errno_t error);
