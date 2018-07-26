#!/usr/bin/env swipl

:- initialization(main, main).

:- use_module(library(filesex)).
:- use_module(library(achelois)).

main([BaseDir|_]) :- run(BaseDir).
main(_) :- working_directory(CWD, CWD), run(CWD).

run(Path) :-
    make_directory('all-results'),
    copy(Path, 'pollution-data'),
    copy(Path, 'minimized'),
    copy(Path, 'detection-results'),
    copy(Path, 'static-field-info'),
    zip('all-results', _).

copy(BaseDir, Name) :-
    format('Looking for ~w in ~w~n', [Name, BaseDir]),
    walk(BaseDir, Files),
    findall(File,
    (
        member(File, Files),
        file_base_name(File, Name),
        file_directory_name(File, Dir),
        file_base_name(Dir, DirName),
        atomic_list_concat(['all-results/', DirName, '-', Name], '', OutName),
        format('Copying ~w to ~w~n', [File, OutName]),
        copy_directory(File, OutName)
    ), _).

zip(Dir, ZipName) :-
    file_base_name(Dir, DirName),
    file_name_extension(DirName, 'zip', ZipName),
    run_process(path(zip), ['-r', ZipName, Dir]).

walk(Path, Files) :-
    exists_directory(Path),
    list_files(Path, TempFiles),
    findall(ChildFiles, (member(Dir, TempFiles), exists_directory(Dir), walk(Dir, ChildFiles)), ChildFileList),
    flatten([TempFiles|ChildFileList], Files).
walk(Path, [Path]) :- exists_file(Path).
walk(_, []). % Path isn't a file or directory, so it just doesn't exist.


