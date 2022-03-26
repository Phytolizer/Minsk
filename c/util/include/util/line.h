#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>

bool util_read_line(char **line_buffer, size_t *buffer_length, FILE *fp);
