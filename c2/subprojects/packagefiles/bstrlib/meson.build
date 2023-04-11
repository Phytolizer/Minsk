project(
    'bstr',
    'c',
)

includes = include_directories('.')
bstr_lib = library(
    'bstr',
    'bstrlib.c',
    'bstraux.c',
    include_directories: includes,
)
bstr_dep = declare_dependency(
    link_with: bstr_lib,
    include_directories: include_directories('.'),
)
