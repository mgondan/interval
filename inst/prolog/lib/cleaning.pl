:- module(cleaning, [clean/2, op(150, xfx, ...)]).

/** <module> Clean representations

Mapping between expression from the user-level to clean representations 
*/

% Number
clean(A, Cleaned),
    number(A)
 => Cleaned = number(A).

clean(number(A), Cleaned)
 => Cleaned = number(A).

clean(L...U, number(A))
 => L = A,
    U = A.

clean(User, number(A))
 => User = A.

% Constants pi and e
clean(pi, Cleaned)
 => Cleaned = number(pi).

clean(e, Cleaned)
 => Cleaned = number(e).

% Interval
clean(L...U, Cleaned)
 => Cleaned = L...U.

clean(User, A...A)
 => User = A.

clean(User, L...U)
 => User = L...U.

% Empty list
clean([], Cleaned)
 => Cleaned = [].

clean(User, [])
 => User = [].

% Boolean
clean(true, Cleaned) 
 => Cleaned = bool(true).

clean(false, Cleaned) 
 => Cleaned = bool(false).

clean(bool(Bool), Cleaned) 
 => Cleaned = bool(Bool).

clean(User, bool(Boolean)) 
 => User = Boolean.

% String
clean(A, Cleaned),
    string(A)
 => Cleaned = string(A).

clean(string(A), Cleaned)
 => Cleaned = string(A).

clean(User, string(A))
 => User = A.

% Atom
clean(A, Cleaned),
    atomic(A)
 => Cleaned = atomic(A).

clean(atomic(A), Cleaned)
 => Cleaned = atomic(A).

clean(User, atomic(A))
 => User = A.

% Compound (recursive call on arguments)
clean(Expr, Expr1),
    compound(Expr)
 => mapargs(clean, Expr, Expr1).

clean(Expr, Expr1),
    compound(Expr1)
 => mapargs(clean, Expr, Expr1).

% Variables
clean(A, B),
    var(A),
    var(B)
 => A = B.
