project(
    'linenoise',
    'c',
)

includes = include_directories('.')
linenoise_lib = library(
    'linenoise',
    'linenoise.c',
    'stringbuf.c',
    'utf8.c',
    include_directories: includes,
    c_args: '-DUSE_UTF8',
)
configure_file(
    input: 'linenoise.h',
    output: 'linenoise.h',
    copy: true,
)
linenoise_dep = declare_dependency(
    link_with: linenoise_lib,
    include_directories: include_directories('.'),
)
