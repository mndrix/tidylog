:- use_module(library(tidylog), [ codes_term/2
                                , variable_name/2
                                ]).

term_expansion(Codes -> Term, Test) :-
    format(atom(Head), "~s", [Codes]),
    Test = (
        Head :-
            codes_term(C,Term),
            C == Codes
    ),
    tap:register_test(Head).

:- use_module(library(tap)).

% variables
'named variable' :-
    codes_term(`Foo.`, Var),
    codes_term(Codes, Var),
    Codes == `Foo.`.
'anonymous variable' :-
    codes_term(`_.`, Var),
    codes_term(Codes, Var),
    Codes == `_.`.
'named singleton variable' :-
    codes_term(`_Ignore.`, Var),
    codes_term(Codes, Var),
    Codes == `_Ignore.`.


:- ['t/samples'].
