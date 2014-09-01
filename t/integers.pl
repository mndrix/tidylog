:- use_module(library(tidylog), [codes_term/2]).
:- use_module(library(tap)).


'positive decimal: read' :-
    codes_term(`1234.`, T),
    T == 1234.

'positive decimal: write' :-
    codes_term(C,1234),
    C = `1_234.`.

'negative decimal: read' :-
    codes_term(`-98765.`,T),
    T == -98765.

'negative decimal: write' :-
    codes_term(C,-98765),
    C = `-98_765.`.


'positive hex: read' :-
    codes_term(`0xbeef.`,T),
    T == 0xbeef.

'positive hex: write' :-
    codes_term(C, 0xbeef),
    C == `48_879.`.
