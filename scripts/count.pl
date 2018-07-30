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
    format('Found ~w fields (~w): ~w~n', [Length, Path, Fields]).

fields(Path, Fields) :-
    load_xml(Path, [element(map, [], Elements)], []),
    findall(Field,
        (
            member(element(entry, [], Subelements), Elements),
            member(element(string, [], [Field]), Subelements),
            atomic_list_concat(Parts, '.', Field),
            last(Parts, FieldName),
            \+ upcase_atom(FieldName, FieldName)
        ),
        Fields).

