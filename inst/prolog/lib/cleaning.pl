/** <file> Clean representations

Mapping between expression from the user-level to clean representations 
*/

% Number
clean(A, Cleaned),
    number(A)
 => Cleaned = number(A).

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

clean(User, bool(Boolean)) 
 => User = Boolean.

% String
clean(A, Cleaned),
    string(A)
 => Cleaned = string(A).

clean(User, string(A))
 => User = A.

% Atom
clean(A, Cleaned),
    atomic(A)
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
