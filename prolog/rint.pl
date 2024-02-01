:- module(rint, []).

:- reexport(library(interval)).
:- use_module(library(rologp)).

interval:int(Expr, Res),
    compound(Expr),
    compound_name_arity(Expr, Name, Arity),
    rmono(Name/Arity, Signs)
 => compound_name_arguments(Expr, Name, Args),
    findall(R,
        (   maplist(lower, Signs, Args, Lower),
            compound_name_arguments(LowerExpr, Name, Lower),
            r_eval(LowerExpr, R)
        ), Ls),
    min_list(Ls, L),
    findall(R,
        (   maplist(upper, Signs, Args, Upper),
            compound_name_arguments(UpperExpr, Name, Upper),
            r_eval(UpperExpr, R)
        ), Us),
    max_list(Us, U),
    Res = L...U.

rmono(pbinom/3, [/, /, -]).
rmono(pnorm/1, [+]).
rmono(pnorm/3, [+, -, *]). % probably wrong


%qnorm
rmono(qnorm/1, [+]).
rmono(qnorm/3, [+, +, *]).
