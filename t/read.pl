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


:- ['t/samples'].
