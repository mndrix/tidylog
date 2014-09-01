% Common utility predicates
:- module(tidylog_common, [ codes//1
                          , end_of_line//0
                          , eos//0
                          , format//2
                          , parsing//0
                          , prefer_none//1
                          , prefer_one//1
                          , rest_of_line//1
                          , when_generating//1
                          , when_parsing//1
                          ]).

:- use_module(library(tidylog/char), [horizontal//0]).


%% parsing// is semidet.
%
%  True if DCG is operating as a parser.  Specifically,
%  the DCG list is not a variable.
parsing(H,H) :-
    nonvar(H).


%% eos//
%
%  True if DCG list is at its end.
eos([],[]).


%% greedy(:Rule)//
%
%  True if Rule matches as many times as possible.
:- meta_predicate greedy(//,?,?).
greedy(Rule) -->
    call(Rule),
    greedy(Rule).
greedy(_) -->
    [].


%% end_of_line//
%
%  True if list is at the end of a line.  This could be either
%  a newline character or the end of input.  It prefers a newline character.
end_of_line -->
    "\n".
end_of_line -->
    eos.


%% codes(?Codes:list)//
%
%  Take as few characters as possible, more on backtracking.
codes([]) -->
    [].
codes([Code|Codes]) -->
    [Code],
    codes(Codes).


%% rest_of_line(?Line:list)//
%
%  Line is all characters before a newline or end of stream.  Newline
%  character is left on the stream.
rest_of_line([]) -->
    prefer_none(horizontal),
    ( \+ \+ "\n" ; eos ).
rest_of_line([Code|Codes]) -->
    [Code],
    { Code \== 0'\n },
    rest_of_line(Codes).


%% prefer_none(:Rule)//
%
%  True if DCG Rule matches.  When parsing, Rule may matches as many
%  times as possible.  When generating, Rule matches 0 times.
:- meta_predicate prefer_none(//,?,?).
prefer_none(Rule) -->
    ( parsing -> greedy(Rule); [] ).


%% prefer_one(:Rule)//
%
%  True if DCG Rule matches.  When parsing, Rule may matches as many
%  times as possible.  When generating, Rule matches 1 time.
:- meta_predicate prefer_one(//,?,?).
prefer_one(Rule) -->
    ( parsing -> greedy(Rule); call(Rule) ).


%% when_generating(:Goal)//
%
%  Call Goal when DCG operates in generator mode.
:- meta_predicate when_generating(0,?,?).
when_generating(Goal) -->
    ( parsing -> []; { call(Goal) } ).


%% when_parsing(:Goal)//
%
%  Call Goal when DCG operates in parsing mode.
:- meta_predicate when_parsing(0,?,?).
when_parsing(Goal) -->
    ( parsing -> { call(Goal) }; [] ).


%% format(+Pattern,+Args)//
%
%  Generate format/2 output onto DCG list.
format(Pattern,Args,H,T) :-
    format(codes(H,T),Pattern,Args).
