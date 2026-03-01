:- module(arguments, [process_args/4]).

/** <module> Processing R arguments

Matching named arguments and assigning default values in R functions 
*/

:- use_module(cleaning).

% Interface
 process_args(Fun, Signature, UserArgs, Call) :-
    args_named_unnamed(UserArgs, Named, Unnamed), % Splitting into named and unnamed args
    match_signature(Signature, Named, Unnamed, Canonical0), % Traversing signature
    compound_name_arguments(Call0, Fun, Canonical0),
    clean(Call0, Call).

% Split named and unnamed arguments into two lists
args_named_unnamed(Args, Named, Unnamed) :-
    split(Args, [], Named, [], Unnamed).

split([], Named0, Named, Unnamed0, Unnamed) :-
    reverse(Unnamed0, Unnamed), % Unnamed arguments as list to preserve oder
    dict_create(Named, args, Named0). % Named arguments as dict

split([atomic(Name)=Value | Args], Named0, Named, Unnamed0, Unnamed) :-
    !, split(Args, [Name=Value | Named0], Named, Unnamed0, Unnamed).

split([Value | Args], Named0, Named, Unnamed0, Unnamed) :-
    !, split(Args, Named0, Named, [Value | Unnamed0], Unnamed).

% Traverse the signature. For each argument check whether it has been provided by the user in "Named". Otherwise, assign a value from "Unnamed". If "Unnamed" is empty, assign the default value.
match_signature(Signature, Named, Unnamed, Canonical) :-
    dict_size(Signature, Size),
    match_signature_(1, Size, Signature, Named, Unnamed, [], Canonical).

match_signature_(Index0, Size, Signature, Named, Unnamed0, Canonical0, Canonical) :-
    Index0 =< Size,
    get_dict(Index0, Signature, Arg),
    match(Index0, Arg, Named, Unnamed0, Unnamed, Canonical0, Canonical1),
    Index is Index0 + 1,
    !, match_signature_(Index, Size, Signature, Named, Unnamed, Canonical1, Canonical).

% Additional arguments not defined in the signature are simply appended
match_signature_(Index, Size, Signature, Named, [Value | Unnamed], Canonical0, Canonical) :-
    append(Canonical0, [Value], Canonical1),
    !, match_signature_(Index, Size, Signature, Named, Unnamed, Canonical1, Canonical). 

match_signature_(_Index, _Size, _Signature, _Named, _Unnamed, Canonical, Canonical).

% Assign named argument provided by user
match(Index, [Name | _Default], Named, Unnamed, Unnamed, Canonical0, Canonical1) :-
    get_dict(Name, Named, Value),
    nth1(Index, Canonical1, (Name=Value), Canonical0).

% User has not provided named argument. Assign the first unnamed argument
match(Index, [Name | _Default], _Named, [Value | Unnamed], Unnamed, Canonical0, Canonical1) :-
    nth1(Index, Canonical1, (Name=Value), Canonical0).

% No named and no unnamed argument. Assign default value
match(Index, [Name | [Default]], _Named, Unnamed, Unnamed, Canonical0, Canonical1) :-
    nth1(Index, Canonical1, (Name=Default), Canonical0).
