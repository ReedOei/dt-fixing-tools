:- use_module(library(filesex)).
:- use_module(library(achelois)).

main :-
    current_prolog_flag(argv, [_, FilePath]),
    try_subjects(FilePath, R),
    writeln(R).

try_subjects(Path, Results) :-
    read_file(Path, Lines),
    findall([Url, Commit, ModulePath],
        (
            member(Line, Lines),
            try_subject(Line, Url, Commit, Modules),
            member(ModulePath, Modules)
        ),
        Results).

try_subject(Line, Url, Commit, [ModulePath]) :-
    split_string(Line, " ", "", [Url, Commit, RelativeModulePath]),
    clone_project(Url, Commit, ProjectPath),
    directory_file_path(ProjectPath, RelativeModulePath, ModulePath).

try_subject(Line, Url, Commit, ModulePaths) :-
    split_string(Line, " ", "", [Url, Commit]),
    clone_project(Url, Commit, ProjectPath),
    maven_modules(ProjectPath, ModulePaths).

run(Path) :-
    working_directory(CWD, CWD),
    directory_file_path(CWD, "diagnose.sh", DiagnoseScript),

    compiles(Path),
    compiles(Path, testCompile),
    compiles(Path, dependencies),

    read_process(Path, path(bash), [DiagnoseScript], _).

