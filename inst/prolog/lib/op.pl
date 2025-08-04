/** <file> General operators

Depends on these externally defined predicates:
 - eval/1
 - eval/2
 - mixed/2
 - positive/2
 - zeropos/2
 - strictpos/2
 - negative/2
 - zeroneg/2
 - strictneg/2
 - zero/2
 - even/1
 - natural/1
 - floor/2
 - ceiling/2
 - min_list/2
 - max_list/2

Every operator is defined with an interval_/3 clause. 
Macros may be used for automatic generation of interval_/3 clauses. 
A 'macro' predicate matches the arguments of 'macro_clause' except for the last one being the result:
    macro(Op/Arity, MacroType) <---> macro_clause(Op/Arity, MacroType, Clauses)
    macro(Op/Arity, Fn/Arity, Dir) <---> macro_clause(Op/Arity, Fn/Arity, Dir, Clauses)

For more information on the meaning of macro arguments, refer to the module 'expansion'.
*/

:- use_module(expansion).

:- multifile(interval_/3).

user:term_expansion(macro(Op/Arity, Fn, Dir), Clauses) :-
    macro_clause(Op/Arity, Fn, Dir, Clauses).

user:term_expansion(macro(Op/Arity, Fn, Dir, Options), Clauses) :-
    macro_clause(Op/Arity, Fn, Dir, Options, Clauses).

%
% No evaluation
%
interval_(number(A), number(A), _Flags) :- !.
interval_(number(A), A...A, _Flags) :- !.
interval_(L...U, L...U, _Flags) :- !.
interval_(bool(A), bool(A), _Flags) :- !.
interval_(atomic(A), atomic(A), _Flags) :- !.
interval_(string(A), string(A), _Flags) :- !.

%
% List
%
interval_([H | T], Res, Flags) :-
    !,
    maplist(interval__(Flags), [H | T], Res).

interval_([], Res, _Flags) :- 
    !, Res = [].

%
% Comparison
%
% Less
interval_(number(A) < number(B), Res, _Flags) :-
    less(A, B, Res0),
    !, Res = Res0.

less(A, B, bool(true)) :-
    eval(A < B), !.

less(_, _, bool(false)).

interval_(_...A2 < B1..._, Res, _Flags) :-
    less(A2, B1, Res0), 
    !, Res = Res0.

macro((<)/2, interval_, []).

% Greater
interval_(number(A) > number(B), Res, _Flags) :-
    less(B, A, Res0),
    !, Res = Res0.

interval_(A1..._ > _...B2, Res, _Flags) :-
    less(B2, A1, Res0),
    !, Res = Res0.

macro((>)/2, interval_, []).

% Less or equal
interval_(number(A) =< number(B), Res, _Flags) :-
    less_eq(A, B, Res0),
    !, Res = Res0.

less_eq(A, B, bool(true)) :-
    eval(A =< B), !.

less_eq(_, _, bool(false)).

interval_(A1..._ =< _...B2, Res, _Flags) :-
    less_eq(A1, B2, Res0),
    !, Res = Res0.

macro((=<)/2, interval_, []).

% Greater or equal
interval_(number(A) >= number(B), Res, _Flags) :-
    less_eq(B, A, Res0),
    !, Res = Res0.

interval_(_...A2 >= B1..._, Res, _Flags) :-
    less_eq(B1, A2, Res0),
    !, Res = Res0.

macro((>=)/2, interval_, []).

% Equal
interval_(number(A) =:= number(B), Res, _Flags) :-
    eq(A, B, Res0),
    !, Res = Res0.

eq(A, B, bool(true)) :-
    eval(A =:= B), !.

eq(_, _, bool(false)).

interval_(A1...A2 =:= B1...B2, Res, _Flags) :-
    eq(A1, B1, bool(true)),
    eq(A2, B2, bool(true)), 
    !, Res = bool(true).

interval_(_..._ =:= _..._, Res, _Flags) :-
    !, Res = bool(false).

macro((=:=)/2, interval_, []).

% Unequal
interval_(number(A) =\= number(B), Res, _Flags) :-
    not_eq(A, B, Res0),
    !, Res = Res0.

not_eq(A, B, bool(true)) :-
    eval(A =\= B), !.

interval_(A1...A2 =\= B1...B2, Res, _Flags) :-
    eq(A1, B1, bool(true)),
    eq(A2, B2, bool(true)),
    !, Res = bool(false).

interval_(_..._ =\= _..._, Res, _Flags) :- 
    !, Res = bool(true).

macro((=\=)/2, interval_, []).

%
% Addition
%
macro((+)/2, all, [+, +]).

%
% Subtraction
%
macro((-)/2, all, [+, -]).

%
% Multiplication
%
interval_(number(A) * number(B), Res, _Flags) :-
    mult(A, B, Res0),
    !, Res = number(Res0).

mult(A, B, Res) :-
    eval(A * B, Res).

interval_(A1...A2 * B1...B2, Res, _Flags) :-
    mult(A1, B1, Res0),
    mult(A1, B2, Res1),
    mult(A2, B1, Res2),
    mult(A2, B2, Res3),
    min_list([Res0, Res1, Res2, Res3], L),
    max_list([Res0, Res1, Res2, Res3], U), 
    !, Res = L...U.

macro((*)/2, interval_, []).

%
% Division
%
interval_(number(A) / number(B), Res, _Flags) :-
    div0(A, B, Res0),
    !, Res = number(Res0).

div0(A, B, Res) :-
    eval(A / B, Res).

interval_(A1...A2 / B1...B2, Res, _Flags) :-
    !,
    div(A1...A2, B1...B2, Res0),
    Res = Res0.

macro((/)/2, interval_, []).

div(A...B, C...D, Res),
    zero(A, B),
    (   negative(C, D)
    ;   mixed(C, D) 
    ;   positive(C, D)
    )
 => Res = number(0).

div(A...B, C...D, Res),
    zero(C, D),
    zeropos(A, B)
 => Res = number(1.0Inf).

div(A...B, C...D, Res),
    zero(C, D),
    zeroneg(A, B)
 => Res = number(-1.0Inf).

div(A...B, C...D, Res),
    zero(C, D),
    mixed(A, B)
 => (   Res = number(-1.0Inf)
    ;   Res = number(1.0Inf)
    ).

div(A...B, C...D, Res),
    zero(A, B),
    zero(C, D)
 => Res = number(1.5NaN).

%
% Hickey Theorem 8 and Figure 4
%
% P1 / P (special case, then general case)
div(A...B, C...D, Res),
    strictpos(A, B),
    zeropos(C, D)
 => div0(A, D, L),
    Res = L...1.0Inf.

div(A...B, C...D, Res),
    strictpos(A, B),
    positive(C, D)
 => div0(A, D, L),
    div0(B, C, U),
    Res = L...U.

% P0 / P
div(A...B, 0.0...D, Res),
    zeropos(A, B),
    positive(0.0, D)
 => Res = 0.0...1.0Inf.

div(A...B, C...D, Res),
    zeropos(A, B),
    positive(C, D)
 => div0(B, C, U),
    Res = 0.0...U.

% M / P
div(A...B, 0.0...D, Res),
    mixed(A, B),
    positive(0.0, D)
 => Res = -1.0Inf...1.0Inf.

div(A...B, C...D, Res),
    mixed(A, B),
    positive(C, D)
 => div0(A, C, L),
    div0(B, C, U),
    Res = L...U.

% N0 / P
div(A...B, 0.0...D, Res),
    zeroneg(A, B),
    positive(0.0, D)
 => Res = -1.0Inf...0.0.

div(A...B, C...D, Res),
    zeroneg(A, B),
    positive(C, D)
 => div0(A, C, L),
    Res = L...0.0.

% N1 / P
div(A...B, 0.0...D, Res),
    strictneg(A, B),
    positive(0.0, D)
 => div0(B, D, U),
    Res = -1.0Inf...U.

div(A...B, C...D, Res),
    strictneg(A, B),
    positive(C, D)
 => div0(A, C, L),
    div0(B, D, U),
    Res = L...U.

% P1 / M (2 solutions)
div(A...B, C...D, Res),
    strictpos(A, B),
    mixed(C, D)
 => (   div0(A, C, U),
        Res = -1.0Inf...U
    ;   div0(A, D, L),
        Res = L...1.0Inf
    ).

% P0 / M
div(A...B, C...D, Res),
    zeropos(A, B),
    mixed(C, D)
 => Res = -1.0Inf...1.0Inf.

% M / M
div(A...B, C...D, Res),
    mixed(A, B),
    mixed(C, D)
 => Res = -1.0Inf...1.0Inf.

% N0 / M
div(A...B, C...D, Res),
    zeroneg(A, B),
    mixed(C, D)
 => Res = -1.0Inf...1.0Inf.

% N1 / M (2 solutions)
div(A...B, C...D, Res),
    strictneg(A, B),
    mixed(C, D)
 => (   div0(B, D, U),
        Res = -1.0Inf...U
    ;   div0(B, C, L),
        Res = L...1.0Inf
    ).

% P1 / N
div(A...B, C...D, Res),
    strictpos(A, B),
    zeroneg(C, D)
 => div0(A, C, U),
    Res = -1.0Inf...U.

div(A...B, C...D, Res),
    strictpos(A, B),
    negative(C, D)
 => div0(B, D, L),
    div0(A, C, U),
    Res = L...U.

% P0 / N
div(A...B, C...D, Res),
    zeropos(A, B),
    zeroneg(C, D)
 => Res = -1.0Inf...0.0.

div(A...B, C...D, Res),
    zeropos(A, B),
    negative(C, D)
 => div0(B, D, L),
    Res = L...0.0.

% M / N
div(A...B, C...D, Res),
    mixed(A, B),
    zeroneg(C, D)
 => Res = -1.0Inf...1.0Inf.

div(A...B, C...D, Res),
    mixed(A, B),
    negative(C, D)
 => div0(B, D, L),
    div0(A, D, U),
    Res = L...U.

% N0 / N
div(A...B, C...D, Res),
    zeroneg(A, B),
    zeroneg(C, D)
 => Res = 0.0...1.0Inf.

div(A...B, C...D, Res),
    zeroneg(A, B),
    negative(C, D)
 => div0(A, D, U),
    Res = 0.0...U.

% N1 / N
div(A...B, C...D, Res),
    strictneg(A, B),
    zeroneg(C, D)
 => div0(B, C, L),
    Res = L...1.0Inf.

div(A...B, C...D, Res),
    strictneg(A, B),
    negative(C, D)
 => div0(B, C, L),
    div0(A, D, U),
    Res = L...U.

%
% Square root
%
% sqrt/1: "normal" behavior, returns nan for negative argument
interval_(sqrt(number(A)), Res, _Flags) :-
    sqrt_(A, Res0),
    !, Res = number(Res0).

sqrt_(A, Res) :-
    eval(sqrt(A), Res), !.

sqrt_(_, 1.5NaN).

macro(sqrt/1, sqrt_, [+]).

% sqrt1/1: only for intervals, crops negative part of interval at 0
interval_(sqrt1(A...B), Res, _Flags) :-
    strictneg(A, B),
    !, Res = number(1.5NaN).

interval_(sqrt1(A...B), Res, _Flags) :-
    zeroneg(A, B),
    !, Res = number(0.0).

interval_(sqrt1(A...B), Res, _Flags) :-
    mixed(A, B),
    !,
    sqrt_(B, U),
    Res = 0.0...U.

interval_(sqrt1(A...B), Res, Flags) :-
    !, interval_(sqrt(A...B), Res, Flags).

%
% Power
%
interval_(A ** B, Res, Flags) :-
    !, interval_(A ^ B, Res, Flags).

interval_(number(Base) ^ number(Exp), Res, _Flags) :-
    pow0(Base, Exp, Res0),
    !, Res = number(Res0).

pow0(Base, Exp, Res) :- 
    eval(Base^Exp, Res).

interval_(A...B ^ number(Exp), Res, _Flags) :-
    pow(A...B, Exp, Res0),
    !, Res = Res0.

% Even exponent with negative base
pow(A...B, Exp, Res),
    negative(A, B),
    natural(Exp),
    even(Exp)
 => pow0(A, Exp, U),
    pow0(B, Exp, L),
    Res = L...U.

% Even exponent with mixed base
pow(A...B, Exp, Res),
    mixed(A, B),
    natural(Exp),
    even(Exp)
 => eval(max(A^Exp, B^Exp), U),
    Res = 0...U.

% Positive also works with negative exponents
pow(A...B, Exp, Res),
    positive(A, B),
    eval(Exp < 0)
 => pow0(A, Exp, U),
    pow0(B, Exp, L), 
    Res = L...U.

% General case
pow(A...B, Exp, Res),
    natural(Exp)
 => pow0(A, Exp, L),
    pow0(B, Exp, U), 
    Res = L...U.

pow(A...B, Exp, Res),
    positive(A, B),
    eval(Exp > 0),
    eval(Exp < 1)
 => pow0(A, Exp, L),
    pow0(B, Exp, U), 
    Res = L...U.

%
% Exponential
%
interval_(exp(number(A)), Res, _Flags) :-
    eval(exp(A), Res0),
    !, Res = number(Res0).   

interval_(exp(A...B), Res, _Flags) :-
    eval(exp(A), exp(B), Res0),
    !, Res = Res0.

%
% Absolute value
%
interval_(abs(number(A)), Res, _Flags) :-
    abs0(A, Res0),
    !, Res = number(Res0).

abs0(A, Res) :-
    eval(abs(A), Res).

interval_(abs(A...B), Res, _Flags) :-
    abs1(A...B, Res0),
    !, Res = Res0.

abs1(A...B, Res) :-
    positive(A, B),
    !,
    Res = A...B.

abs1(A...B, Res) :-
    negative(A, B),
    !,
    abs0(A, U),
    abs0(B, L),
    Res = L...U.

abs1(A...B, Res) :-
    eval(max(abs(A), abs(B)), U),
    Res = 0.0...U.

%
% Round
%
% round/1: round to 0 decimals
interval_(round(A), Res, Flags) :-
    !, interval_(round(A, number(0)), Res, Flags).

% round/2: round to specified number of decimals
interval_(round(number(A), number(Dig)), Res, _Flags) :-
    round0(A...A, Dig, Res0),
    !, Res = Res0.

interval_(round(A...B, number(Dig)), Res, _Flags) :-
    round0(A...B, Dig, Res0),
    !, Res = Res0.
 
round0(A...B, Dig, Res) :-
    floor(A, Dig, A1),
    ceiling(B, Dig, B1),
    Res = A1...B1.

%
% Sine
%
interval_(sin(number(A)), Res, _Flags) :-
    eval(sin(A), Res0),
    !, Res = number(Res0).

interval_(sin(A...B), Res, _Flags) :-
    sin1(A...B, Res0),
    !, Res = Res0.

% interval extends over more than 2 max/mins
sin1(A...B, Res) :-
    eval(A/pi - 1/2, A1),
    eval(B/pi - 1/2, B1),
    eval(ceiling(A1), A2),
    eval(B1 >= A2 + 1),
    !, 
    Res = -1...1.

% interval extends over 1 max
sin1(A...B, Res) :-
    eval(A / (2*pi) - 1/4, A1),
    eval(B / (2*pi) - 1/4, B1),
    eval(ceiling(A1), A2),
    eval(B1 >= A2),
    !,
    eval(min(sin(A), sin(B)), L),
    Res = L...1.

% interval extends over 1 min
sin1(A...B, Res) :-
    eval(A / (2*pi) + 1/4, A1),
    eval(B / (2*pi) + 1/4, B1),
    eval(ceiling(A1), A2),
    eval(B1 >= A2),
    !,
    eval(max(sin(A), sin(B)), U),
    Res = -1...U.

% default rising
sin1(A...B, Res) :-
    eval(sin(A), A1),
    eval(sin(B), B1),
    ( eval(A1 < B1) 
    -> Res = A1...B1
    ; Res = B1...A1 ).
    
%
% Unary plus
%
macro((+)/1, all, [+]).

%
% Unary negate
%
macro((-)/1, all, [-]).

%
% Max/2
%
macro(max/2, all, [+, +]).

%
% Min/2
%
macro(min/2, all, [+, +]).

%
% Evaluation of arguments.
%
interval_(A0, Res, Flags) :-
    compound(A0),
    compound_name_arguments(A0, Name, Args0),
    maplist(interval__(Flags), Args0, Args1),
    compound_name_arguments(A1, Name, Args1),
    dif(A0, A1), 
    !, interval_(A1, Res, Flags).

interval_(A, _Res, _Flags) :-
    !, 
    term_string(A, String),
    string_concat("No rule matches ", String, Message),
    writeln(Message),
    fail.

interval__(Flags, A, Res) :-
    interval_(A, Res, Flags).