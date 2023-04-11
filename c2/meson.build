project(
    'Minsk',
    'c',
    default_options: [
        'c_std=gnu17',
        'warning_level=2',
        'default_library=static',
        'b_sanitize=address,undefined',
        'b_lundef=false',
    ],
)

cmake = import('cmake')

linenoise_dep = dependency(
    'linenoise',
    fallback: [
        'linenoise',
        'linenoise_dep',
    ],
    default_options: ['warning_level=0'],
)
bstr_dep = dependency(
    'bstrlib',
    fallback: [
        'bstrlib',
        'bstr_dep',
    ],
    default_options: ['warning_level=0'],
)
arena_dep = dependency(
    'arena',
    fallback: [
        'arena',
        'arena_dep',
    ],
    default_options: ['warning_level=0'],
)
utf8proc_proj = cmake.subproject('utf8proc')
utf8proc_dep = utf8proc_proj.dependency('utf8proc')

subdir('platform')
subdir('minsk')
subdir('mc')
