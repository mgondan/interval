:- module(r, [r_initialize/0, r/1, r/2, r_source/2]).

:- use_module(library(rologp)).

:- dynamic r_initialized/0.

% Initialize R, load some code into the base environment.
r_initialize,
    r_initialized
 => true.

r_initialize
 => pack_property(interval, directory(Dir)),
    directory_file_path(Dir, prolog, Dir1),
    r_source(r, Dir1),
    assert(r_initialized).

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

