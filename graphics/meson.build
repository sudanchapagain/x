project('graphics-experiments', 'cpp')

cpp = run_command('find', 'src', '-name', 'main.cpp', check: false)
main_files = cpp.stdout().strip().split('\n')

foreach file : main_files
    if file == ''
        continue
    endif

    dir_parts = file.split('/')
    dir_name = dir_parts[-2]

    executable(
        dir_name,
        sources: file,
        dependencies: [dependency('gl'), dependency('glut'), dependency('glu')],
        cpp_args: ['-std=c++11'],
    )
endforeach
