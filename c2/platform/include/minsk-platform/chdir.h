#pragma once

#include <bstrlib.h>

#include "minsk-platform/errno.h"

extern minsk_errno_t minsk_chdir(const_bstring path);
