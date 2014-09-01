:- module(tidylog_number_hex, [hex//1]).

% Parse Prolog hexadecimal number (never used for generating)

hex(Neg) -->
    "-",
    hex_(N),
    { Neg is -N }.
hex(N) -->
    hex_(N).

hex_(N) -->
    "0x",
    digit(Code),
    digits(Codes),
    { name(N,[0'0, 0'x, Code|Codes]) }.


digit(C) -->
    [C],
    { between(0'0,0'9,C)
    ; between(0'a,0'f,C)
    ; between(0'A,0'F,C)
    }.


digits([0'_,Code|Codes]) -->
    [0'_],
    digit(Code),
    digits(Codes).
digits([Code|Codes]) -->
    digit(Code),
    digits(Codes).
digits([]) -->
    [].
