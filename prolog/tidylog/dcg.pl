:- module(tidylog_dcg, [ read_prolog//1, write_prolog//1 ]).

:- use_module(library(tidylog/atom/name), [name//1]).
:- use_module(library(tidylog/atom/punc), [punctuation//1]).

:- use_module(library(tidylog/comment/aol), [aol_comment//1]).
:- use_module(library(tidylog/comment/ml), [ml_comment//1]).

:- use_module(library(tidylog/number/decimal), [decimal//1]).
:- use_module(library(tidylog/number/hex), [hex//1]).
:- use_module(library(tidylog/number/octal), [octal//1]).
:- use_module(library(tidylog/number/binary), [binary//1]).

:- use_module(library(tidylog/text), [text//2]).

:- use_module(library(dcg/basics), [ eos//0
                                   , float//1
                                   ]).
:- use_module(library(lists), [proper_length/2]).
:- use_module(library(portray_text), []).

% Thank you to P. Deransart, A. Ed-Dbali, L. Cervoni whose
% "Prolog: The Standard" provided guidance for the initial
% DCG that parsed Prolog code.

read_prolog(T) -->
    comment(T),
    eos.
read_prolog(T) -->
    term(T,1200),
    end.


write_prolog(T) -->
    term_out(T),
    ( { is_comment(T) } -> []; end ).


% parse terms from a list of codes
term(T,P) -->
    number_term(T,P).
term(T,P) -->
    variable_term(T,P).
term(T,P) -->
    compound_term(T,P).
term(T,P) -->
    atom_term(T,P).
term(T,P) -->
    paren_term(T,P).
term(T,P) -->
    string_term(T,P).
term(T,P) -->
    prefix_operator_term(T,P).


% generate list of codes from a term
term_out(Var) -->
    { var(Var), ! },
    { name_the_vars(Var, Names) },
    write_term(Var,[variable_names(Names)]).
term_out(AolComment) -->
    aol_comment(AolComment).
term_out(MlComment) -->
    ml_comment(MlComment).
term_out(Integer) -->
    { integer(Integer) },
    decimal(Integer).
term_out(Op) -->
    { is_operator(Op) },
    format("(~w)",[Op]).
term_out(Atom) -->
    { atom(Atom) },
    name(Atom).
term_out(Punctuation) -->
    { atom(Punctuation) },
    punctuation(Punctuation).
term_out(Text) -->
    text(_,Text).
term_out(Head :- Body) -->
    term_out(Head),
    " :-",
    nl,
    indent,
    term_out(Body).
term_out(F) -->
    { float(F) },
    format("~g",[F]).
term_out(T) -->
    format("~q",[T]).


number_term(T,P) -->
    float_number(F),
    rest_term(F,T,0,P).
number_term(T,P) -->
    decimal(I),
    rest_term(I,T,0,P).
number_term(T,P) -->
    hex(I),
    rest_term(I,T,0,P).
number_term(T,P) -->
    octal(I),
    rest_term(I,T,0,P).
number_term(T,P) -->
    binary(I),
    rest_term(I,T,0,P).


variable_term(T,P) -->
    variable(V),
    rest_term(V,T,0,P).

name_the_vars(Term,Names) :-
    term_variables(Term, Vars),
    maplist(variable_name,Vars,Names).

variable_name(Var,Name=Var) :-
    get_attr(Var, tidylog, name(Name)).

atom_term(T,P) -->
    atom(A),
    { \+ is_operator(A) },
    rest_term(A,T,0,P).
atom_term(Op,_P) -->
    atom(Op),
    { is_operator(Op) }.

atom(A) -->
    optional_layout_text,
    name_token(A).


compound_term(T,P) -->
    % standard notation
    atom(F),
    open_paren,
    term(Arg,999),
    arg_list(L),
    { Term =.. [F, Arg | L ] },
    rest_term(Term,T,0,P).
compound_term(T,P) -->
    % list notation
    open_bracket,
    term(Arg,999),
    items(List),
    rest_term('[|]'(Arg,List),T,0,P).
compound_term(T,P) -->
    % curly notation
    open_curly,
    term(Term,1200),
    close_curly,
    rest_term('{}'(Term),T,0,P).

arg_list([]) -->
    close_paren.
arg_list([H|T]) -->
    comma,
    term(H,999),
    arg_list(T).

items('[|]'(H,T)) -->
    comma,
    term(H,999),
    items(T).
items(T) -->
    head_tail_separator,
    term(T,999),
    close_bracket.
items('[]') -->
    close_bracket.


paren_term(T,P) -->
    open_paren,
    term(Term,1200),
    close_paren,
    rest_term(Term,T,0,P).
paren_term(T,P) -->
    "(",
    term(Term,1200),
    close,
    rest_term(Term,T,0,P).


string_term(T,P) -->
    text(string,S),
    rest_term(S,T,0,P).
string_term(T,P) -->
    text(codes,S),
    rest_term(S,T,0,P).


prefix_operator_term(T,P) -->
    atom(Op),
    term(Arg,ArgP),
    { prefix_operator(Op,OpP,ArgP) },
    { P >= OpP },
    { Term =.. [Op, Arg] },
    rest_term(Term,T,OpP,P).


rest_term(LeftArg,T,LeftP,P) -->
    atom(Op),
    { infix_operator(Op,OpP,LAP,RAP) },
    { P >= OpP },
    { LeftP =< LAP },
    term(RightArg,RAP),
    { Term =.. [Op,LeftArg,RightArg] },
    rest_term(Term,T,OpP,P).
rest_term(LeftArg,T,LeftP,P) -->
    atom(Op),
    { postfix_operator(Op,OpP,LAP) },
    { P >= OpP },
    { LeftP =< LAP },
    { Term =.. [Op, LeftArg] },
    rest_term(Term,T,OpP,P).
rest_term(Left,T,LeftP,P) -->
    comma,
    { P >= 1000 },
    { LeftP < 1000 },
    term(Right,1000),
    rest_term(','(Left,Right),T,1000,P).
rest_term(Term,Term,_,_) -->
    [].


variable(Var) -->
    optional_layout_text,
    variable_token(X),
    { atom_codes(Name,X) },
    { set_variable_name(Var,Name) }.

set_variable_name(Var,Name) :-
    put_attr(Var,tidylog,name(Name)).


float_number(F) -->
    optional_layout_text,
    float(F).


open_paren -->
    optional_layout_text,
    "(".


close_paren -->
    optional_layout_text,
    ")".


open_bracket -->
    optional_layout_text,
    "[".


close_bracket -->
    optional_layout_text,
    "]".


open_curly -->
    optional_layout_text,
    "{".


close_curly -->
    optional_layout_text,
    "}".


head_tail_separator -->
    optional_layout_text,
    "|".


comma -->
    optional_layout_text,
    ",".


end -->
    optional_layout_text,
    ".".


% true if DCG is operating as a parser
parsing(H,H) :-
    nonvar(H).

% matches Rule 0 or more times, consuming as many as possible
:- meta_predicate greedy(//,?,?).
greedy(Rule) -->
    call(Rule),
    greedy(Rule).
greedy(_) -->
    [].

format(Pattern,Args,H,T) :-
    format(codes(H,T),Pattern,Args).

write_term(Term,Options,H,T) :-
    with_output_to(codes(H,T),write_term(Term,Options)).


optional_layout_text -->
    ( parsing -> greedy(layout_text) ; [] ).


layout_text -->
    comment(_).
layout_text -->
    layout_char(_).


is_comment(Var) :-
    var(Var),
    !,
    fail.
is_comment('tidylog %full'(_)).
is_comment('tidylog %multi'(_)).


comment(Comment) -->
    aol_comment(Comment).
comment(Comment) -->
    ml_comment(Comment).


name_token(A) -->
    name(A).
name_token(A) -->
    text(atom, A).
name_token(A) -->
    punctuation(A).


alpha_num_seq_char([A|L]) -->
    alpha_num_char(A),
    alpha_num_seq_char(L).
alpha_num_seq_char([]) -->
    [].


variable_token(V) -->
    anonymous_variable(V).
variable_token(V) -->
    named_variable(V).

anonymous_variable(`_`) -->
    "_".

named_variable([0'_,A|S]) -->
    "_",
    alpha_num_char(A),
    alpha_num_seq_char(S).
named_variable([C|S]) -->
    capital_letter_char(C),
    alpha_num_seq_char(S).


type_char(Type,C) -->
    [C],
    { code_type(C,Type) }.

alpha_num_char(C) -->
    type_char(alnum,C).
alpha_num_char(0'_) -->
    "_".


layout_char(C) -->
    type_char(space,C).


capital_letter_char(C) -->
    type_char(upper,C).


is_operator(Op) :-
    atom(Op),
    current_op(_,_,Op).

infix_operator(Op,P,LeftP,RightP) :-
    current_op(P,Spec,Op),
    Op \= '.',
    ( Spec = xfx ->
        LeftP is P-1,
        RightP is P-1
    ; Spec = xfy ->
        LeftP is P-1,
        RightP is P
    ; Spec = yfx ->
        LeftP is P,
        RightP is P-1
    ).

postfix_operator(Op,P,LeftP) :-
    current_op(P,Spec,Op),
    ( Spec = xf ->
        LeftP is P-1
    ; Spec = yf ->
        LeftP is P
    ).

prefix_operator(Op,P,RightP) :-
    current_op(P,Spec,Op),
    ( Spec = fx ->
        RightP is P-1
    ; Spec = fy ->
        RightP is P
    ).


nl -->
    "\n".

indent -->
    "    ".  % 4 spaces
