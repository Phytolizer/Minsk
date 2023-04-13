doctest_dep = dependency('doctest')
magic_enum_dep = dependency('magic_enum')
fmt_dep = dependency('fmt')

test_src = []
subdir('src')

test_exe = executable(
    'minsk-test',
    test_src,
    include_directories: include_directories('include'),
    dependencies: [
        arena_dep,
        doctest_dep,
        fmt_dep,
        magic_enum_dep,
        minsk_dep,
    ],
    cpp_args: [
        '-DDOCTEST_CONFIG_REQUIRE_STRINGIFICATION_FOR_ALL_USED_TYPES',
        '-DDOCTEST_CONFIG_SUPER_FAST_ASSERTS',
        '-DDOCTEST_CONFIG_USE_STD_HEADERS',
    ],
)

test(
    'minsk',
    test_exe,
)
