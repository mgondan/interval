:- use_module(library(clpBNR)).

/** <file> Support for clpBNR representations 

Maps results from clpBNR to user-friendly representations
*/

:- multifile(clean/2).

% Number
clean(User, number(A)), 
    clpBNR:interval(A)
 => clean_clp(User, A).

clean(User, number(A0)),
    \+ integer(A0),
    A is float(A0)
 => User = A.

% Interval
clean(User, L0...U0),
    clean_lower(L, L0),
    clean_upper(U, U0)
 => User = L...U.

% Representation from clpBNR
clean_clp(User, A),
    A::(real(L, U)),
    L = -1.0Inf
 => User = L...U.

clean_clp(User, A),
    A::(real(L, U)),
    U = 1.0Inf
 => User = L...U.

clean_clp(User, A),
    A::(real(L, _U))
 => User = L.

clean_lower(L, L0),
   domain(L0, real(L1, _))
 => L = L1.

clean_lower(L, L0),
    \+ integer(L0),
    L1 is float(L0)
 => L = L1.

clean_lower(L, L0)
 => L = L0.

clean_upper(U, U0),
   domain(U0, real(_, U1))
 => U = U1.

clean_upper(U, U0),
    \+ integer(U0),
    U1 is float(U0)
 => U = U1.

clean_upper(U, U0)
 => U = U0.
