project(
    'Minsk',
    'c',
    default_options: [
        'c_std=gnu17',
        'warning_level=3',
        'default_library=static',
        'b_sanitize=address,undefined',
        'b_lundef=false',
    ],
)

linenoise_proj = subproject('linenoise')
linenoise_dep = linenoise_proj.get_variable('linenoise_dep')
bstr_proj = subproject('bstrlib')
bstr_dep = bstr_proj.get_variable('bstr_dep')
arena_proj = subproject('arena')
arena_dep = arena_proj.get_variable('arena_dep')

subdir('platform')
subdir('minsk')
subdir('mc')
