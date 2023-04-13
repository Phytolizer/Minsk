#pragma once

#include <minsk-string/string.h>

#include "minsk-platform/errno.h"

extern minsk_errno_t
minsk_chdir(string_t path);
