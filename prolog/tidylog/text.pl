:-module(tidylog_text, [text//2]).

% Describe Prolog text (quoted atoms, code lists, built-in strings)

:- use_module(library(tidylog/common), [ parsing//0
                                       , when_generating//1
                                       , when_parsing//1
                                       ]).

text(Type,Text) -->
    when_generating(text_codes(Type,Text,Codes)),

    quote(Type),
    content(Type,Codes),
    quote(Type),

    when_parsing(text_codes(Type,Text,Codes)).


text_codes(atom,Atom,Codes) :-
    when(ground(Atom),atom(Atom)),
    atom_codes(Atom,Codes).
text_codes(codes,Codes,Codes).
text_codes(string,String,Codes) :-
    when(ground(String),string(String)),
    string_codes(String,Codes).


quote(Type) -->
    [C],
    { quote_char(Type,C) }.


quote_char(atom,0'').
quote_char(codes,0'`). %'
quote_char(string,0'"). %'


content(Type,[C|Codes]) -->
    "\\",
    [Esc],
    escape(Esc,Type,C),
    content(Type,Codes).
content(Type,[C|Codes]) -->
    quote(Type),
    quote(Type),
    { quote_char(Type,C) },
    content(Type,Codes).
content(Type,[C|Codes]) -->
    [C],
    content(Type,Codes).
content(_,[]) -->
    [].


% escape(Char,Type,Code)
escape(0's,_,0'\s) -->
    parsing.
escape(Q,Type,Q) -->
    { quote_char(Type,Q) }.
escape(Esc,_,C) -->
    { escape_char(Esc,C) }.
escape(C,_,C) -->
    parsing.


escape_char(0'\\, 0'\\).
escape_char(0'a, 0'\a).
escape_char(0'b, 0'\b).
escape_char(0'e, 0'\e).
escape_char(0'f, 0'\f).
escape_char(0'n, 0'\n).
escape_char(0'r, 0'\r).
escape_char(0't, 0'\t).
escape_char(0'v, 0'\v).
