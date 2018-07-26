#!/usr/bin/env swipl

:- initialization(main, main).

:- use_module(library(sgml)).
:- use_module(library(achelois)).

main([Path|_]) :-
    walk(Path, Paths),
    findall(PollutionPath, (member(PollutionPath, Paths), findFields(PollutionPath, _)), Paths).

findFields(Path, Fields) :-
    atom_concat(_, '.xml', Path),
    fields(Path, Fields),
    length(Fields, Length),
    format("Found ~w fields: ~w~n", [Length, Fields]).

fields(Path, Fields) :-
    load_xml(Path, [element(map, [], Elements)], []),
    findall(Field,
        (
            member(element(entry, [], Subelements), Elements),
            member(element(string, [], [Field]), Subelements),
            atomic_list_concat(Parts, '.', Field),
            last(Parts, FieldName),
            \+ string_upper(FieldName, FieldName)
        ),
        Fields).

walk(Path, Files) :-
    exists_directory(Path),
    list_files(Path, TempFiles),
    findall(ChildFiles, (member(Dir, TempFiles), exists_directory(Dir), walk(Dir, ChildFiles)), ChildFileList),
    flatten([TempFiles|ChildFileList], Files).
walk(Path, [Path]) :- exists_file(Path).
walk(_, []). % Path isn't a file or directory, so it just doesn't exist.

