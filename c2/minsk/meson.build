minsk_inc = include_directories('include')

# Collect sources
minsk_src = []
subdir('src')

minsk_lib = static_library(
    'minsk',
    minsk_src,
    include_directories: minsk_inc,
    dependencies: [
        arena_dep,
        icuuc_dep,
        platform_dep,
        string_dep,
        verstable_dep,
        textus_coloris_dep,
    ],
)

minsk_dep = declare_dependency(
    link_with: minsk_lib,
    include_directories: minsk_inc,
    dependencies: [
        arena_dep,
        icuuc_dep,
        string_dep,
        verstable_dep,
    ],
)
