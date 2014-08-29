:- module(tidylog_comment_aol, [aol_comment//1]).

:- use_module(library(tidylog/common),[ end_of_line//0
                                      , prefer_none//1
                                      , prefer_one//1
                                      , rest_of_line//1
                                      , when_generating//1
                                      , when_parsing//1
                                      ]).
:- use_module(library(tidylog/char),[horizontal//0]).


% Define "all of line" comment
aol_comment('tidylog %full'(Text)) -->
    when_generating(string_codes(Text,Codes)),

    "%",
    prefer_one(horizontal),
    rest_of_line(Codes),
    prefer_none(horizontal),
    end_of_line,

    when_parsing(string_codes(Text,Codes)).
