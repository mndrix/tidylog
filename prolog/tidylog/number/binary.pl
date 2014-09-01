:- module(tidylog_number_binary, [binary//1]).

% Parse Prolog binary number (never used for generating)

binary(Neg) -->
    "-",
    binary_(N),
    { Neg is -N }.
binary(N) -->
    binary_(N).

binary_(N) -->
    "0b",
    digit(Code),
    digits(Codes),
    { name(N,[0'0, 0'b, Code|Codes]) }.


digit(C) -->
    [C],
    { C=0'0; C=0'1 }.


digits([0'_,Code|Codes]) -->
    [0'_],
    digit(Code),
    digits(Codes).
digits([Code|Codes]) -->
    digit(Code),
    digits(Codes).
digits([]) -->
    [].
