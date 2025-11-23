:-  module(r, 
    [
      r_initialize/1,
      r_close/1,
      r/1,
      r/2,
      r_source/2
    ]).

:- use_module(library(rs_rolog)).

:- dynamic initialized/0, initialized/1.

:- initialization(r_initialize).

% Initialize R, load some code into the base environment.
r_initialize,
    initialized
 => true.

r_initialize
 => pack_property(interval, directory(Dir)),
    directory_file_path(Dir, 'R/rint', Dir1),
    r_source(r, Dir1),
    assert(initialized).

r_initialize(Session),
    initialized(Session)
 => true.

r_initialize(Session)
 => rs_init(Session),
    pack_property(interval, directory(Dir)),
    directory_file_path(Dir, 'R/rint', Dir1),
    r_source(r, Dir1),
    assert(initialized(Session)).

r_close(Session),
    initialized
 => rs_close(Session).

r_close(_)
 => true.

% Call R
r(Expr)
 => rx_call(Expr).

% Evaluate R expression
r(Expr, Res)
 => rx_eval(Expr, Res).

r_source(Name, Dir)
 => file_name_extension(Name, 'R', File),
    directory_file_path(Dir, File, Path),
    atom_string(Path, String),
    r(source(String)).

