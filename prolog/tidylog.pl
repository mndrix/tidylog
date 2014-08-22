:- module(tidylog, [ codes_term/2
                   ]).

:- use_module(library(tidylog/dcg)).


% avoid "Undefined procedure" errors
attr_unify_hook(_,_).

%% codes_term(+Codes,-Term) is det.
%
%  True if Term can be read from Codes.  All characters of
%  Codes must be consumed.
codes_term(Codes, Term) :-
    once(phrase(prolog(Term),Codes)).


%% variable_name(-Var, -Name:atom) is det.
%
%  True if Var was named Name in the original source code.
%  Throws an exception if Var was not created by tidylog
%  parsing source code.
variable_name(Var,Name) :-
    must_be(var, Var),
    once( get_attr(Var, tidylog, name(Name))
        ; throw("Variable not created by tidylog")
        ).
