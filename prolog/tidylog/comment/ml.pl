:- module(tidylog_comment_ml, [ml_comment//1]).

:- use_module(library(tidylog/common),[ codes//1
                                      , prefer_one//1
                                      , rest_of_line//1
                                      , when_generating//1
                                      , when_parsing//1
                                      ]).
:- use_module(library(tidylog/char),[ invisible//0
                                    , nl//0
                                    ]).

% Define multiline comment
ml_comment('tidylog %multi'(Lines)) -->
    when_generating(lines_codes(Lines,Codes)),

    "/*",
    prefer_one(invisible),
    codes(Codes),
    prefer_one(invisible),
    "*/",

    when_parsing(lines_codes(Lines,Codes)).


lines_codes(Lines,Codes) :-
    nonvar(Lines),
    !,
    maplist(string_codes,Lines,LinesOfCodes),
    once(phrase(lines(LinesOfCodes),Codes)).
lines_codes(Lines,Codes) :-
    nonvar(Codes),
    once(phrase(lines(LinesOfCodes),Codes)),
    maplist(string_codes,Lines,LinesOfCodes).


lines([]) -->
    [].
lines([Line]) -->
    rest_of_line(Line).
lines([Line|Lines]) -->
    rest_of_line(Line),
    nl,
    lines(Lines).
