:- module(tidylog_dcg, [ read_prolog//1, write_prolog//1 ]).

:- use_module(library(dcg/basics), [eos//0, float//1]).
:- use_module(library(lists), [proper_length/2]).
:- use_module(library(portray_text), []).

% Thank you to P. Deransart, A. Ed-Dbali, L. Cervoni whose
% "Prolog: The Standard" provided guidance for the initial
% DCG that parsed Prolog code.

read_prolog(T) -->
    term(T,1200),
    end.

write_prolog(T) -->
    term_out(T),
    end.


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
term_out(Head :- Body) -->
    term_out(Head),
    " :-",
    nl,
    indent,
    term_out(Body).
term_out(F) -->
    { float(F) },
    format("~g",[F]).
term_out(Codes) -->
    { proper_length(Codes,Length) },
    { Length > 2 },
    { portray_text:all_ascii(Codes) },  % predicate not exported
    format("`~s`",[Codes]).
term_out(Op) -->
    { is_operator(Op) },
    format("(~w)", [Op]).
term_out(T) -->
    format("~q",[T]).


number_term(T,P) -->
    float_number(F),
    rest_term(F,T,0,P).
number_term(T,P) -->
    % positive integer
    integer_number(I),
    rest_term(I,T,0,P).
number_term(T,P) -->
    % negative integer
    name('-'),
    integer_number(I),
    { NI is -I },
    rest_term(NI,T,0,P).


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
    name(A).
atom('[]') -->
    open_bracket,
    close_bracket.
atom('{}') -->
    open_curly,
    close_curly.


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
    double_quoted_string(S),
    rest_term(S,T,0,P).
string_term(T,P) -->
    back_quoted_string(S),
    rest_term(S,T,0,P).

double_quoted_string(S) -->
    optional_layout_text,
    double_quoted_string_token(S).
double_quoted_string(S) -->
    double_quoted_string_token(S).

back_quoted_string(S) -->
    optional_layout_text,
    back_quoted_string_token(S).


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


name(A) -->
    optional_layout_text,
    name_token(X),
    { atom_codes(A,X) }.


variable(Var) -->
    optional_layout_text,
    variable_token(X),
    { atom_codes(Name,X) },
    { set_variable_name(Var,Name) }.

set_variable_name(Var,Name) :-
    put_attr(Var,tidylog,name(Name)).


integer_number(N) -->
    optional_layout_text,
    integer_token(N).


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
    comment.
layout_text -->
    layout_char(_).


comment -->
    single_line_comment.
comment -->
    multi_line_comment.


single_line_comment -->
    "%",
    comment_text,
    newline_char.


multi_line_comment -->
    comment_open,
    comment_text,
    comment_close.


comment_open -->
    "/*".


comment_close -->
    "*/".


comment_text -->
    [_],
    comment_text.
comment_text -->
    [].


name_token(A) -->
    letter_digit_token(A).
name_token(A) -->
    quoted_token(A).
name_token(A) -->
    semicolon_token(A).
name_token(A) -->
    cut_token(A).
name_token(A) -->
    graphic_token(A).


letter_digit_token([S|A]) -->
    small_letter_char(S),
    alpha_num_seq_char(A).

alpha_num_seq_char([A|L]) -->
    alpha_num_char(A),
    alpha_num_seq_char(L).
alpha_num_seq_char([]) -->
    [].

graphic_token([C|L]) -->
    graphic_token_char(C),
    graphic_token(L).
graphic_token([C]) -->
    graphic_token_char(C).

graphic_token_char('\\') -->
    "\\".
graphic_token_char(C) -->
    graphic_char(C).

quoted_token(Q) -->
    "'",
    single_quoted_item_seq(Q),
    "'".

single_quoted_item_seq([C|S]) -->
    single_quoted_char(C),
    single_quoted_item_seq(S).
single_quoted_item_seq(S) -->
    continuation_escape_sequence,
    single_quoted_item_seq(S).
single_quoted_item_seq([]) -->
    [].

continuation_escape_sequence -->
    "\\",
    newline_char.

semicolon_token([';']) -->
    ";".

cut_token(['!']) -->
    "!".

single_quoted_char(C) -->
    non_quote_char(C).
single_quoted_char(0'') -->
    "''".
single_quoted_char(0'") -->  % "' silly syntax highlighters
    "\"".
single_quoted_char(0'`) -->  % `'
    "`".

double_quoted_char(C) -->
    non_quote_char(C).
double_quoted_char(0'") --> % "'
    "\"\"".
double_quoted_char(0'') -->
    "'".
double_quoted_char(0'`) -->  % `'
    "`".

back_quoted_char(C) -->
    non_quote_char(C).
back_quoted_char(0'') -->
    "'".
back_quoted_char(0'") --> % "'
    "\"".
back_quoted_char(0'`) --> % `'
    "``".

non_quote_char(C) -->
    graphic_char(C);
    alpha_num_char(C);
    solo_char(C);
    space_char(C);
    control_escape_seq(C);
    octal_escape_seq(C);
    hex_escape_seq(C);
    meta_escape_seq(C).


meta_escape_seq(C) -->
    "\\",
    meta_char(C).

control_escape_seq(C) -->
    "\\",
    symbolic_control_char(C).

symbolic_control_char(0'\a) -->
    "a".
symbolic_control_char(0'\b) -->
    "b".
symbolic_control_char(0'\f) -->
    "f".
symbolic_control_char(0'\n) -->
    "n".
symbolic_control_char(0'\r) -->
    "r".
symbolic_control_char(0'\t) -->
    "t".
symbolic_control_char(0'\v) -->
    "v".

octal_escape_seq(C) -->
    "\\",
    octal_digit_seq_char(Octal),
    "\\",
    { compute_integer(Octal,8,C) }.

octal_digit_seq_char([D|L]) -->
    octal_digit_char(D),
    octal_digit_seq_char(L).

hex_escape_seq(C) -->
    "\\x",
    hex_digit_seq_char(Hex),
    "\\",
    { compute_integer(Hex,16,C) }.

hex_digit_seq_char([D|L]) -->
    hex_digit_char(D),
    hex_digit_seq_char(L).


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


integer_token(N) -->
    integer_constant(Chars),
    { number_codes(N, Chars) }.
integer_token(N) -->
    character_code_constant(N);
    binary_constant(N);
    octal_constant(N);
    hex_constant(N).

integer_constant([C|N]) -->
    decimal_digit_char(C),
    integer_constant(N).
integer_constant([C]) -->
    decimal_digit_char(C).

character_code_constant(C) -->
    "0'",
    single_quoted_char(C).

binary_constant(N) -->
    "0b",
    binary_digit_seq_char(Digits),
    { compute_integer(Digits,2,N) }.

binary_digit_seq_char([C|L]) -->
    binary_digit_char(C),
    binary_digit_seq_char(L).
binary_digit_seq_char([C]) -->
    binary_digit_char(C).

octal_constant(N) -->
    "0o",
    octal_digit_seq_char(Digits),
    { compute_integer(Digits,8,N) }.

hex_constant(N) -->
    "0x",
    hex_digit_seq_char(Digits),
    { compute_integer(Digits,16,N) }.


double_quoted_string_token(T) -->
    "\"",
    double_quoted_item_seq(L),
    "\"",
    { translate_double_quotes(L,T) }.

double_quoted_item_seq([C|S]) -->
    double_quoted_char(C),
    double_quoted_item_seq(S).
double_quoted_item_seq(S) -->
    continuation_escape_sequence,
    double_quoted_item_seq(S).
double_quoted_item_seq([]) -->
    [].


back_quoted_string_token(T) -->
    "`",
    back_quoted_item_seq(L),
    "`",
    { translate_back_quotes(L,T) }.

back_quoted_item_seq([C|S]) -->
    back_quoted_char(C),
    back_quoted_item_seq(S).
back_quoted_item_seq(S) -->
    continuation_escape_sequence,
    back_quoted_item_seq(S).
back_quoted_item_seq([]) -->
    [].


type_char(Type,C) -->
    [C],
    { code_type(C,Type) }.

alpha_num_char(C) -->
    type_char(alnum,C).
alpha_num_char(0'_) -->
    "_".

newline_char -->
    type_char(newline,_).

space_char(C) -->
    type_char(white,C).

layout_char(C) -->
    type_char(space,C).

graphic_char(C) -->
    [C],
    { memberchk(C, `#$&*+-./:<=>?@^~`) }.

solo_char(C) -->
    [C],
    { memberchk(C, `!(),;[]{}|&`) }.

meta_char(C) -->
    [C],
    { memberchk(C, `\\'"\``) }.

small_letter_char(C) -->
    type_char(lower,C).

capital_letter_char(C) -->
    type_char(upper,C).

binary_digit_char(C) -->
    type_char(digit(N),C),
    { between(0,1,N) }.

octal_digit_char(C) -->
    type_char(digit(N),C),
    { between(0,7,N) }.

decimal_digit_char(C) -->
    type_char(digit,C).

hex_digit_char(C) -->
    type_char(xdigit(_),C).


compute_integer(Codes,Base,N) :-
    compute_integer_(Codes,Base,0,N).

compute_integer_([],_,Accum,Accum).
compute_integer_([Code|Codes],Base,Accum0,N) :-
    code_type(Code, xdigit(V)),
    Accum is Base*Accum0 + V,
    compute_integer_(Codes,Base,Accum,N).


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

translate_back_quotes(Codes,Codes).

translate_double_quotes(Codes, String) :-
    string_codes(String,Codes).


nl -->
    "\n".

indent -->
    "    ".  % 4 spaces
