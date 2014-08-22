:- use_module(library(tidylog), [ codes_term/2
                                , variable_name/2
                                ]).

term_expansion(Codes -> Term, Test) :-
    format(atom(Head), "~s", [Codes]),
    Test = (
        Head :-
            codes_term(Codes,T),
            T == Term
    ),
    tap:register_test(Head).

:- use_module(library(tap)).

% atoms
`hello.` -> hello.
`mixed_Case.` -> mixed_Case.
`'quoted atom'.` -> 'quoted atom'.

% numbers
`42.` -> 42.
`3.1415.` -> 3.1415.
`2.998e8.` -> 2.998e8.

% strings
`"double quoted string".` -> "double quoted string".
`\`back quoted string\`.` -> `back quoted string`.

% compound terms
`foo(a,b,c).` -> foo(a,b,c).
`hello :- true.` -> (hello :- true).
`1+2.` -> 1+2.
`(:-).` -> (:-).
`@mndrix.` -> @mndrix.


% variables
'named variable' :-
    codes_term(`X.`, Var),
    var(Var),
    variable_name(Var, Name),
    Name == 'X'.
'anonymous variable' :-
    codes_term(`_.`, Var),
    var(Var),
    variable_name(Var, Name),
    Name == '_'.
'named singleton variable' :-
    codes_term(`_Ignore.`, Var),
    var(Var),
    variable_name(Var, Name),
    Name == '_Ignore'.
