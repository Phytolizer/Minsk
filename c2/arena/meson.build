arena_lib = library(
    'arena',
    'arena.c',
    dependencies: [
        arena_hdr_dep,
    ],
    c_args: ['-DARENA_IMPLEMENTATION'],
)

arena_dep = declare_dependency(
    link_with: arena_lib,
    dependencies: [
        arena_hdr_dep,
    ],
)
