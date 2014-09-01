:- module(tidylog_atom_punc, [punctuation//1]).

% Define unquoted atom

:- use_module(library(tidylog/common), [ when_generating//1
                                       , when_parsing//1
                                       ]).

punctuation(A) -->
    when_generating(atom_codes(A,Codes)),

    ( grawlix(Codes)
    ; solo(Codes)
    ; brackets(Codes)
    ),

    when_parsing(atom_codes(A,Codes)).


grawlix([C|Codes]) -->
    [C],
    { grawlix_char(C) },
    grawlix_(Codes).

grawlix_([C|Codes]) -->
    [C],
    { grawlix_char(C) },
    grawlix_(Codes).
grawlix_([]) -->
    [].


grawlix_char(0'#). %' placate syntax highlighters
grawlix_char(0'$). %'
grawlix_char(0'&). %'
grawlix_char(0'*). %'
grawlix_char(0'+). %'
grawlix_char(0'-). %'
grawlix_char(0'.). %'
grawlix_char(0'/). %'
grawlix_char(0':). %'
grawlix_char(0'<). %'
grawlix_char(0'=). %'
grawlix_char(0'>). %'
grawlix_char(0'?). %'
grawlix_char(0'@). %'
grawlix_char(0'^). %'
grawlix_char(0'~). %'


solo([0';]) --> %'
    ";".
solo([0'!]) --> %'
    "!".


brackets([0'[, 0']]) -->
    "[]".
brackets([0'{,0'}]) -->
    "{}".
