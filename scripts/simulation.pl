:- use_module(library(clpfd)).

cart_prod([], []).
cart_prod([L|Ls], [X|Rest]) :-
    member(X, L),
    cart_prod(Ls, Rest).

fst((A, _), A).
snd((_, B), B).

group_by(Pred, Xs, Groups) :- group_by(Pred, [], Xs, Groups).
group_by(_Pred, Groups, [], Groups).
group_by(Pred, Groups, [H|T], NewGroups) :-
    call(Pred, H, Key),
    (
        select((Key, CurVals), Groups, Temp) ->
            group_by(Pred, [(Key, [H|CurVals])|Temp], T, NewGroups);

        group_by(Pred, [(Key, [H])|Groups], T, NewGroups)
    ).

range(Start, Stop, R) :-
    Start #> Stop -> R = [Stop];
    findall(X, between(Start, Stop, X), R).

gen(Tag, Idx, Name, R) :-
    Tag =.. Parts,
    append(Parts, [Idx, Name], Temp),
    R =.. Temp.

gen_index_list(Tag, Start, Stop, Res) :-
    range(Start, Stop, Range),
    same_length(Range, Vars),
    maplist(gen(Tag), Range, Vars, Res).

gen_tests([], []).
gen_tests([test(Tag, I, N)|OtherTests], AllGenerated) :-
    T =.. [Tag, I, N],
    gen_index_list(T, 1, N, Generated),

    gen_tests(OtherTests, OtherGenerated),
    append(Generated, OtherGenerated, AllGenerated).

test_class_name(TestName, TestClassName) :-
    var(TestName) -> TestClassName = '';

    atomic_list_concat(Parts, '.', TestName),
    append(TestClassParts, [_Method], Parts),
    atomic_list_concat(TestClassParts, '.', TestClassName).

test_class(Test, TestClassName) :-
    Test =.. [_Tag, _Id, _N, _C, TestName],
    test_class_name(TestName, TestClassName).

test_perm(Tests, Perm) :-
    group_by(test_class, Tests, TempTestClasses),
    maplist(snd, TempTestClasses, TestClasses),

    permutation(TestClasses, TestClassPerm),
    maplist(permutation, TestClassPerm, MethodPerms),
    append(MethodPerms, Perm).

rand_test_perm(Tests, Perm) :-
    group_by(test_class, Tests, TempTestClasses),
    maplist(snd, TempTestClasses, TestClasses),

    random_permutation(TestClasses, TestClassPerm),
    maplist(random_permutation, TestClassPerm, MethodPerms),
    append(MethodPerms, Perm).

group_idx(Tag, Order, Id, Idx) :-
    G =.. [Tag, Id, N, _, _],
    member(G, Order),

    GTemplate =.. [Tag, Id, N],
    gen_index_list(GTemplate, 1, N, FullList),
    append([_, FullList, _], Order),

    First =.. [Tag, Id, N, 1, _],
    nth1(Idx, Order, First).

before(BeforeTag, AfterTag, Order) :-
    group_idx(BeforeTag, Order, Id, BIdx),
    group_idx(AfterTag, Order, Id, AIdx),
    BIdx #< AIdx.

fails(Order) :-
    before(polluter, victim, Order),
    (
        not(before(polluter, cleaner, Order));

        not(before(cleaner, victim, Order))
    ).

prob_failure(VictimName, Tests, FailingPerms, Total, Prob) :-
    findall(Perm, test_perm([victim(_,1,1,VictimName)|Tests], Perm), Perms),
    include(fails, Perms, FailingPerms),
    length(Perms, Total),
    length(FailingPerms, FailingCount),
    Prob is FailingCount / Total.

sample(0, _, []).
sample(N, Pred, [R|Rest]) :-
    N1 #= N - 1,
    call(Pred, R),
    sample(N1, Pred, Rest).

prob_failure_rand(VictimName, Tests, FailingPerms, SampleN, Prob) :-
    sample(SampleN, rand_test_perm([victim(_,1,1,VictimName)|Tests]), Perms),
    include(fails, Perms, FailingPerms),
    length(Perms, Total),
    length(FailingPerms, FailingCount),
    Prob is FailingCount / Total.

