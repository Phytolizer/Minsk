tau_dep = dependency('tau')

test_src = []
subdir('src')

test_exe = executable(
    'minsk-test',
    test_src,
    include_directories: include_directories('include'),
    dependencies: [
        arena_dep,
        minsk_dep,
        tau_dep,
    ],
)

test(
    'minsk',
    test_exe,
)
