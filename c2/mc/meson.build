mc_exe = executable(
    'mc',
    'app' / 'cwd.c',
    'app' / 'main.c',
    dependencies: [
        linenoise_dep,
        minsk_dep,
        platform_dep,
        bstr_dep,
    ],
    install: true,
    c_args: '-DNO_COMPLETION',
)

mc_run = run_target(
    'run-mc',
    command: [mc_exe],
)
