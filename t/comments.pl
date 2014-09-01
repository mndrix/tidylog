:- use_module(library(tidylog), [codes_term/2]).
:- use_module(library(tap)).


'single line comment: read' :-
    codes_term(`% full line comment\n`, T),
    T == 'tidylog %full'("full line comment").

'single line comment: write' :-
    codes_term(C, 'tidylog %full'("full line comment")),
    C == `% full line comment\n`.


'multiline comment: read' :-
    codes_term(`/*\nmultiline comment\n*/`, T),
    T == 'tidylog %multi'(["multiline comment"]).

'multiline comment: write' :-
    codes_term(C, 'tidylog %multi'(["multiline comment"])),
    C == `/*\nmultiline comment\n*/`.


'single line: trailing spaces' :-
    codes_term(`% word   `, T),
    T == 'tidylog %full'("word").

'multiline: trailing spaces' :-
    codes_term(`/* word  \nanother    */`,T),
    T == 'tidylog %multi'(["word","another"]).

'single line: extra internal spaces' :-
    codes_term(`% word  etc`, T),
    T == 'tidylog %full'("word  etc").
