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

:- ['t/samples'].
