/** <file> Default

Contains the default interval_/3 clause for nested expressions, 
e.g., interval(5 + (1...2 - 1), Res). This is stored here in a separate file 
to ensure more flexibility.
**/

interval_(A0, Res, Flags) :-
    compound(A0),
    compound_name_arguments(A0, Name, Args0),
    maplist(interval__(Flags), Args0, Args1),
    compound_name_arguments(A1, Name, Args1),
    dif(A0, A1), 
    !, interval_(A1, Res, Flags).

interval_(A, _Res, _Flags) :-
    !,
    functor(A, Name, Arity),
    throw(
        error(
            existence_error(function, Name/Arity),
            context(interval_/3, 'No matching interval_/3 clause for this function')
        )
    ).

interval__(Flags, A, Res) :-
    interval_(A, Res, Flags).