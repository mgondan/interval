:- module(r, [r/1, r/2, r_source/2]).

% :- use_module(library(rolog)).

% Call R
r(Expr)
 => r_call(Expr).

% Evaluate R expression
r(Expr, Res)
 => r_eval(Expr, Res).

r_source(Name, Dir)
 => file_name_extension(Name, 'R', File),
    directory_file_path(Dir, File, Path),
    atom_string(Path, String),
    r(source(String)).

