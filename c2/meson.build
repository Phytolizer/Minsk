project(
    'Minsk',
    'c',
    'cpp',
    default_options: [
        'c_std=gnu17',
        'cpp_std=c++20',
        'warning_level=2',
        'default_library=static',
        'b_sanitize=address,undefined',
        'b_lundef=false',
    ],
)

linenoise_dep = dependency(
    'linenoise',
    fallback: [
        'linenoise',
        'linenoise_dep',
    ],
    default_options: ['warning_level=0'],
)
arena_hdr_dep = dependency(
    'arena',
    fallback: [
        'arena',
        'arena_dep',
    ],
    default_options: ['warning_level=0'],
)

icuuc_dep = dependency('icu-uc')
uthash_dep = dependency('uthash')

subdir('arena')
subdir('string')
subdir('platform')
subdir('minsk')
subdir('mc')
subdir('tests')
