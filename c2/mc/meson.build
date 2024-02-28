mc_exe = executable(
    'mc',
    'app' / 'cwd.c',
    'app' / 'main.c',
    dependencies: [
        linenoise_dep,
        minsk_dep,
        platform_dep,
        string_dep,
        textus_coloris_dep,
    ],
    install: true,
    c_args: '-DNO_COMPLETION',
)

run_target(
    'run-mc',
    command: [mc_exe],
)
