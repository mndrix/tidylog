:- module(tidylog_atom_name, [name//1]).

% Define unquoted atom

:- use_module(library(tidylog/common), [ when_generating//1
                                       , when_parsing//1
                                       ]).

name(A) -->
    when_generating(atom_codes(A,[Code|Codes])),

    initial(Code),
    inner(Codes),

    when_parsing(atom_codes(A,[Code|Codes])).


initial(C) -->
    [C],
    { initial_char(C) }.

inner([C|Codes]) -->
    [C],
    { inner_char(C) },
    inner(Codes).
inner([]) -->
    [].


initial_char(C) :-
    between(0'a, 0'z, C).

inner_char(C) :-
    C = 0'_;
    initial_char(C);
    between(0'0,0'9,C);
    between(0'A,0'Z,C).
