%
% Hook for custom interval functions
%
% 1. Declare function
%    with int_hook(Name, pred_name(Args), Ret, [Options]).
%    'Args' specify the type of the arguments, '...' for intervals and 'atomic'
%    for numbers and logical values. Same for 'Ret'.
%    Add 'evaluate(false)' to 'Options' list to skip evaluation of arguments.
% 2. Calculate result with interval:pred_name(Args, Res)
%    'Args' are the argument types as defined in the hook with variable names,
%    e.g., 'L...U', 'atomic(A)'.
%
% See example below for (/)/2.
%

%
% Monotonically behaving functions
%
% +: increasing
% -: decreasing
% *: increasing or decreasing
% **: commutative, all *
% /: symbolic argument or flag
mono((+)/1, [+]).
mono((+)/2, [+, +]).
mono((-)/1, [-]).
mono((-)/2, [+, -]).
mono((*)/2, **).
mono((**)/2, [*, /]). % power
mono((exp)/1, [+]).

%
% Comparison
%
int_hook(<, less1(atomic, atomic), _, []).

less1(atomic(A), atomic(B), Res, _Flags) :-
    A < B,
    !,
    Res = true.

less1(atomic(_) < atomic(_), Res, _Flags) :-
    !,
    Res = false.

int_hook(<, less2(..., ...), _, []).

less2(_...A2, B1..._, Res, _Flags) :-
    A2 < B1,
    !,
    Res = true.

less2(_..._, _..._, false2, _Flags).

int_hook(=<, less_eq(..., ...), _, []).

less_eq(A1..._, _...B2, Res, _Flags) :-
    A1 =< B2,
    !,
    Res = true.

less_eq(_..._, _..._, false, _Flags).

int_hook(>, great(..., ...), _, []).
great(A1..._, _...B2, Res, _Flags) :-
    A1 > B2,
    !,
    Res = true.

great(_..._, _..._, false, _Flags).

int_hook(>=, great_eq(..., ...), _, []).
great_eq(_...A2, B1..._, Res, _Flags) :-
    A2 >= B1,
    !,
    Res = true.

great_eq(_..._, _..._, false, _Flags).

int_hook(=\=, not_eq(..., ...), _, []).
not_eq(A...B, C...D, Res, Flags) :-
    (   less2(A...B, C...D, true, Flags)
    ;   great(A...B, C...D, true, Flags)
    ), !,
    Res = true.

not_eq(_..._, _..._, false, _Flags).

int_hook(=:=, eq(..., ...), _, []).
eq(A...B, C...D, Res, Flags) :-
    less_eq(A...B, C...D, true, Flags),
    great_eq(A...B, C...D, true, Flags),
    !,
    Res = true.

eq(_..._, _..._, false, _Flags). 

%
% Division
%
int_hook(/, div1(atomic, atomic), atomic, []).
div1(atomic(A), atomic(B), atomic(Res), _Flags) :-
    Res is A / B.

int_hook(/, div2(..., ...), ..., []).
div2(A...B, C...D, Res, Flags) :-
    !,
    div(A...B, C...D, Res, Flags).

% Hickey Figure 1
mixed(L, U) :-
    L < 0,
    U > 0.

positive(L, U) :-
    L >= 0,
    U > 0.

zeropos(L, U) :-
    L =:= 0,
    U > 0.

strictpos(L, _) :-
    L > 0.

negative(L, U) :-
    L < 0,
    U =< 0.

zeroneg(L, U) :-
    L < 0,
    U =:= 0.

strictneg(_, U) :-
    U < 0.

zero(L, U) :-
    L =:= 0,
    U =:= 0.

%
% atomic 0 in numerator or denominator
%
div(A...B, C...D, Res, _Flags),
    zero(A, B),
    (   negative(C, D)
    ;   mixed(C, D) 
    ;   positive(C, D)
    )
 => Res = atomic(0).

div(A...B, C...D, Res, _Flags),
    zero(C, D),
    zeropos(A, B)
 => Res = atomic(1.0Inf).

div(A...B, C...D, Res, _Flags),
    zero(C, D),
    zeroneg(A, B)
 => Res = atomic(-1.0Inf).

div(A...B, C...D, Res, _Flags),
    zero(C, D),
    mixed(A, B)
 => (   Res = atomic(-1.0Inf)
    ;   Res = atomic(1.0Inf)
    ).

div(A...B, C...D, Res, _Flags),
    zero(A, B),
    zero(C, D)
 => Res = atomic(1.5NaN).

%
% Hickey Theorem 8 and Figure 4
%
% P1 / P (special case, then general case)
div(A...B, C...D, Res, _Flags),
    strictpos(A, B),
    zeropos(C, D)
 => eval(A / D, L),
    Res = L...1.0Inf.

div(A...B, C...D, Res, _Flags),
    strictpos(A, B),
    positive(C, D)
 => eval(A / D, L),
    eval(B / C, U),
    Res = L...U.

% P0 / P
div(A...B, 0.0...D, Res, _Flags),
    zeropos(A, B),
    positive(0.0, D)
 => Res = 0.0...1.0Inf.

div(A...B, C...D, Res, _Flags),
    zeropos(A, B),
    positive(C, D)
 => eval(B / C, U),
    Res = 0.0...U.

% M / P
div(A...B, 0.0...D, Res, _Flags),
    mixed(A, B),
    positive(0.0, D)
 => Res = -1.0Inf...1.0Inf.

div(A...B, C...D, Res, _Flags),
    mixed(A, B),
    positive(C, D)
 => eval(A / C, L),
    eval(B / C, U),
    Res = L...U.

% N0 / P
div(A...B, 0.0...D, Res, _Flags),
    zeroneg(A, B),
    positive(0.0, D)
 => Res = -1.0Inf...0.0.

div(A...B, C...D, Res, _Flags),
    zeroneg(A, B),
    positive(C, D)
 => eval(A / C, L),
    Res = L...0.0.

% N1 / P
div(A...B, 0.0...D, Res, _Flags),
    strictneg(A, B),
    positive(0.0, D)
 => eval(B / D, U),
    Res = -1.0Inf...U.

div(A...B, C...D, Res, _Flags),
    strictneg(A, B),
    positive(C, D)
 => eval(A / C, L),
    eval(B / D, U),
    Res = L...U.

% P1 / M (2 solutions)
div(A...B, C...D, Res, _Flags),
    strictpos(A, B),
    mixed(C, D)
 => (   eval(A / C, U),
        Res = -1.0Inf...U
    ;   eval(A / D, L),
        Res = L...1.0Inf
    ).

% P0 / M
div(A...B, C...D, Res, _Flags),
    zeropos(A, B),
    mixed(C, D)
 => Res = -1.0Inf...1.0Inf.

% M / M
div(A...B, C...D, Res, _Flags),
    mixed(A, B),
    mixed(C, D)
 => Res = -1.0Inf...1.0Inf.

% N0 / M
div(A...B, C...D, Res, _Flags),
    zeroneg(A, B),
    mixed(C, D)
 => Res = -1.0Inf...1.0Inf.

% N1 / M (2 solutions)
div(A...B, C...D, Res, _Flags),
    strictneg(A, B),
    mixed(C, D)
 => (   eval(B / D, U),
        Res = -1.0Inf...U
    ;   eval(B / C, L),
        Res = L...1.0Inf
    ).

% P1 / N
div(A...B, C...D, Res, _Flags),
    strictpos(A, B),
    zeroneg(C, D)
 => eval(A / C, U),
    Res = -1.0Inf...U.

div(A...B, C...D, Res, _Flags),
    strictpos(A, B),
    negative(C, D)
 => eval(B / D, L),
    eval(A / C, U),
    Res = L...U.

% P0 / N
div(A...B, C...D, Res, _Flags),
    zeropos(A, B),
    zeroneg(C, D)
 => Res = -1.0Inf...0.0.

div(A...B, C...D, Res, _Flags),
    zeropos(A, B),
    negative(C, D)
 => eval(B / D, L),
    Res = L...0.0.

% M / N
div(A...B, C...D, Res, _Flags),
    mixed(A, B),
    zeroneg(C, D)
 => Res = -1.0Inf...1.0Inf.

div(A...B, C...D, Res, _Flags),
    mixed(A, B),
    negative(C, D)
 => eval(B / D, L),
    eval(A / D, U),
    Res = L...U.

% N0 / N
div(A...B, C...D, Res, _Flags),
    zeroneg(A, B),
    zeroneg(C, D)
 => Res = 0.0...1.0Inf.

div(A...B, C...D, Res, _Flags),
    zeroneg(A, B),
    negative(C, D)
 => eval(A / D, U),
    Res = 0.0...U.

% N1 / N
div(A...B, C...D, Res, _Flags),
    strictneg(A, B),
    zeroneg(C, D)
 => eval(B / C, L),
    Res = L...1.0Inf.

div(A...B, C...D, Res, _Flags),
    strictneg(A, B),
    negative(C, D)
 => eval(B / C, L),
    eval(A / D, U),
    Res = L...U.

%
% Square root
%
% sqrt/1: "normal" behavior, returns nan for negative argument
% sqrt1/1: crops negative part of interval at 0
%
mono(sqrt/1, [+]).

int_hook(sqrt1, sqrt1(...), _, []).
sqrt1(A...B, Res, _Flags) :-
    strictneg(A, B),
    !,
    Res = 1.5NaN.

sqrt1(A...B, Res, _Flags) :-
    zeroneg(A, B),
    !,
    Res = 0.0.

sqrt1(A...B, Res, _Flags) :-
    mixed(A, B),
    !,
    eval(sqrt(B), U),
    Res = 0.0...U.

%
% Power
%
int_hook(^, pow0(atomic, atomic), atomic, []).
pow0(atomic(X), atomic(Y), atomic(Res), _Flags) :-
    eval(X^Y, Res).

% Even exponent with negative base
int_hook(^, pow(..., atomic), ..., []).
pow(L...U, atomic(Exp), Res, _Flags),
    negative(L, U),
    natural(Exp),
    even(Exp)
 => eval(U^Exp, L^Exp, Res).

% Even exponent with mixed base
pow(A...B, atomic(Exp), Res, _Flags),
    mixed(A, B),
    natural(Exp),
    even(Exp)
 => eval(max(A^Exp, B^Exp), U),
    Res = 0...U.

% Positive also works with negative exponents
pow(L...U, atomic(Exp), Res, _Flags),
    positive(L, U),
    Exp < 0
 => eval(U^Exp, L^Exp, Res).

% General case
pow(L...U, atomic(Exp), Res, _Flags),
    natural(Exp)
 => eval(L^Exp, U^Exp, Res).

% Utility
even(A) :-
    A mod 2 =:= 0.

natural(A) :-
    A >= 0,
    integer(A).

%
% Absolute value
%
int_hook(abs, abs1(...), ..., []).
abs1(A...B, Res, _Flags) :-
    positive(A, B),
    !,
    Res = A...B.

abs1(A...B, Res, _Flags) :-
    negative(A, B),
    !,
    eval(abs(A), U),
    eval(abs(B), L),
    Res = L...U.

% mixed
abs1(A...B, Res, _Flags) :-
    !,
    L = 0.0,
    U is max(abs(A), abs(B)),
    Res = L...U.

%
% round
%
int_hook(round, round1(atomic, atomic), atomic, []).
round1(atomic(A), atomic(Dig), atomic(Res), _Flags) :-
    Mul is 10^Dig,
    Res is round(A*Mul) / Mul.
    
int_hook(round, round2(..., atomic), ..., []).
round2(A...B, atomic(Dig), Res, _Flags) :-
    eval(floor(A, Dig), A1),
    eval(ceiling(B, Dig), B1),
    Res = A1...B1.

int_hook(round, round3(_, atomic), _, []).
round3(A, _Dig, A, _Flags).

eval_hook(floor(A, Dig), Res) :-
    Mul is 10^Dig,
    Res is floor(A * Mul) / Mul.

eval_hook(ceiling(A, Dig), Res) :-
    Mul is 10^Dig,
    Res is ceiling(A * Mul) / Mul.

%
% sine
%
int_hook(sin, sin(...), ..., []).

% interval extends over more than 2 max/mins
sin(A...B, Res, _Flags) :-
    A1 is A/pi - 1/2,
    B1 is B/pi - 1/2,
    B1 >= ceiling(A1) + 1,
    !,
    Res = -1...1.

% interval extends over 1 max
sin(A...B, Res, _Flags) :-
    A1 is A / (2*pi) - 1/4,
    B1 is B / (2*pi) - 1/4,
    B1 >= ceiling(A1),
    !,
    L is min(sin(A), sin(B)),
    Res = L...1.

% interval extends over 1 min
sin(A...B, Res, _Flags) :-
    A1 is A / (2*pi) + 1/4,
    B1 is B / (2*pi) + 1/4,
    B1 >= ceiling(A1),
    !,
    U is max(sin(A), sin(B)),
    Res = -1...U.

% default rising
sin(A...B, Res, _Flags) :-
    A1 is sin(A),
    B1 is sin(B),
    sort([A1, B1], [L, U]),
    Res = L...U.
