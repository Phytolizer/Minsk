platform_inc = include_directories('include')

platform_lib = static_library(
    'minsk-platform',
    'src' / 'chdir.c',
    'src' / 'errno.c',
    include_directories: platform_inc,
    dependencies: [
        string_dep,
    ],
)

platform_dep = declare_dependency(
    link_with: platform_lib,
    include_directories: platform_inc,
)
