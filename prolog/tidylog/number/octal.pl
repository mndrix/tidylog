:- module(tidylog_number_octal, [octal//1]).

% Parse Prolog octal number (never used for generating)

octal(Neg) -->
    "-",
    octal_(N),
    { Neg is -N }.
octal(N) -->
    octal_(N).

octal_(N) -->
    "0o",
    digit(Code),
    digits(Codes),
    { name(N,[0'0, 0'o, Code|Codes]) }.


digit(C) -->
    [C],
    { between(0'0,0'7,C) }.


digits([0'_,Code|Codes]) -->
    [0'_],
    digit(Code),
    digits(Codes).
digits([Code|Codes]) -->
    digit(Code),
    digits(Codes).
digits([]) -->
    [].
