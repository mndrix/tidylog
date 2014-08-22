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
