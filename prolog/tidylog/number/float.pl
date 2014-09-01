:- module(tidylog_number_float, [float//1]).

:- use_module(library(tidylog/common), [format//2,parsing//0]).

:- use_module(library(dcg/basics), []).

% Define Prolog floats
% (cheat by punting to existing SWI Prolog code)

float(F) -->
    ( parsing ->
        dcg_basics:float(F)
    ; [] ->
        { float(F) },
        format("~g",[F])
    ).
