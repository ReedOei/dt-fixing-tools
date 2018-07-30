#!/usr/bin/env swipl

:- initialization(main).

:- use_module(library(filesex)).
:- use_module(library(achelois)).

main([FilePath|_]) :-
    try_subjects(FilePath, R),
    writeln(R).

try_subjects(Path, Results) :-
    read_file(Path, Lines),
    findall([Url, Commit, ModulePath],
        (
            member(Line, Lines),
            try_subject(Line, Url, Commit, Modules),
            member(ModulePath, Modules),
            run(ModulePath)
        ),
        Results).

try_subject(Line, Url, Commit, [ModulePath]) :-
    atomic_list_concat([Url, Commit, RelativeModulePath], ' ', Line),
    clone_project(Url, Commit, ProjectPath),
    directory_file_path(ProjectPath, RelativeModulePath, ModulePath).

try_subject(Line, Url, Commit, ModulePaths) :-
    atomic_list_concat([Url, Commit], ' ', Line),
    clone_project(Url, Commit, ProjectPath),
    maven_modules(ProjectPath, ModulePaths).

run(Path) :-
    format('[INFO] Running path: ~s~n', Path),
    working_directory(CWD, CWD),
    directory_file_path(CWD, 'diagnose.sh', DiagnoseScript),

    writeln('[INFO] Compiling classes'),
    compiles(Path),
    writeln('[INFO] Compiling tests'),
    compiles(Path, testCompile),
    writeln('[INFO] Fetching dependencies'),
    compiles(Path, dependencies),

    !,
    format('[INFO] Running main script at ~s~n', DiagnoseScript),
    run_process(Path, path(bash), [DiagnoseScript]).

