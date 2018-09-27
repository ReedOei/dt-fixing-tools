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
    copy(Path, 'static-field-info-TRACK'),
    zip('all-results', _).

copy(BaseDir, Name) :-
    format('Looking for ~w in ~w~n', [Name, BaseDir]),
    forall(walk(BaseDir, File),
        (
            file_base_name(File, Name),
            file_directory_name(File, Dir),
            file_base_name(Dir, DirName),
            atomic_list_concat(['all-results/', DirName, '-', Name], '', OutName),
            format('Copying ~w to ~w~n', [File, OutName]),
            copy_directory(File, OutName)
        )).

