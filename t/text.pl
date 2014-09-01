:- use_module(library(tidylog), [codes_term/2]).
:- use_module(library(tap)).


'quoted atom: read' :-
    codes_term(`'hello world'.`, T),
    T == 'hello world'.

'quoted atom: write' :-
    codes_term(C, 'hello world'),
    C = `'hello world'.`.

'plain atom: write' :-
    codes_term(C,howdy),
    C = `howdy.`.

'string: read' :-
    codes_term(`"some words".`, T),
    T == "some words".

'string: write' :-
    codes_term(C, "some words"),
    C == `"some words".`.

'slash s: read' :-
    codes_term(`"one\\stwo".`,T),
    T == "one two".

'slash n: read' :-
    codes_term(`\`above\\nbelow\`.`, T),
    T = `above\nbelow`.

'slash n: write' :-
    codes_term(C, `above\nbelow`),
    C == `\`above\\nbelow\`.`.
