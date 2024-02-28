project(
    'textus_coloris',
    'c',
)

includes = include_directories('split-out')
lib = library(
    'textus_coloris',
    ['split-out' / 'textus_coloris.c'],
    include_directories: includes,
)
textus_coloris_dep = declare_dependency(
    include_directories: includes,
    link_with: lib,
)
