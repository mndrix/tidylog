:- module(tidylog_number_decimal, [decimal//1]).

% Define Prolog decimal format

:- use_module(library(tidylog/common), [ parsing//0
                                       , when_generating//1
                                       , when_parsing//1
                                       ]).


decimal(N) -->
    parsing,
    !,
    ( "-", decimal_(M), { N is -M }
    ; decimal_(N)
    ).
decimal(N) -->
    % generating
    { integer(N) },
    ( { N < 0 }, "-", { M is -N }, decimal_(M)
    ; decimal_(N)
    ).

% positive decimal integer
decimal_(N) -->
    when_generating(decimal_codes(N,[Code|Codes])),

    digit(Code),
    digits(Codes),

    when_parsing(decimal_codes(N,[Code|Codes])).


digit(C) -->
    [C],
    { between(0'0,0'9,C) }.


digits([Sep,Code|Codes]) -->
    separator(Sep),
    digit(Code),
    digits(Codes).
digits([Code|Codes]) -->
    digit(Code),
    digits(Codes).
digits([]) -->
    [].


%% decimal_codes(?N:integer, ?Code:list) is det.
%
%  True if integer N, in decimal notation, is represented as list of Codes.
%  When generating Codes from N, underscore characters are inserted every 3
%  digits.
decimal_codes(N,Codes) :-
    integer(N),
    !,
    format(codes(Codes),"~3I",[N]).
decimal_codes(N,Codes) :-
    nonvar(Codes),
    name(N,Codes),
    integer(N).


separator(C) -->
    [C],
    { separator_char(C) }.


separator_char(0'_).
separator_char(0'\s).
