minsk_inc = include_directories('include')

# Collect sources
minsk_src = []
subdir('src')

minsk_lib = static_library(
    'minsk',
    minsk_src,
    include_directories: minsk_inc,
    dependencies: [
        string_dep,
        platform_dep,
        utf8proc_dep,
    ],
)

minsk_dep = declare_dependency(
    link_with: minsk_lib,
    include_directories: minsk_inc,
    dependencies: [
        string_dep,
        utf8proc_dep,
    ],
)
