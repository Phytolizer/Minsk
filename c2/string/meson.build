string_inc = include_directories('include')
string_lib = library(
    'minsk-string',
    'src' / 'string.c',
    include_directories: string_inc,
    dependencies: [
        arena_dep,
    ],
)

string_dep = declare_dependency(
    link_with: string_lib,
    include_directories: string_inc,
    dependencies: [
        arena_dep,
    ],
)
