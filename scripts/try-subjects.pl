:- use_module(library(filesex)).

:- use_module(utility).

try_subjects(Path, Results) :-
    read_file(Path, Lines),
    findall([Url, Commit],
        (
            member(Line, Lines),
            split_string(Line, " ", "", [Url, Commit]),
            run(Url, Commit)
        ),
        Results).

run(Url, Commit) :-
    working_directory(CWD, CWD),
    clone_project(Url, Commit, Path),

    directory_file_path(CWD, "diagnose.sh", DiagnoseScript),
    % read_process(Path, path(bash), [DiagnoseScript], Output),

    directory_file_path(Path, "log.txt", LogPath),
    write_file(LogPath, Output).

