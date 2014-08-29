% Define character classes
:- module(tidylog_char, [ horizontal//0
                        , nl//0
                        , invisible//0
                        ]).


%% horizontal//
%
%  Matches a single horizontal character (space or tab).  It
%  prefers a space character.
horizontal -->
    " ".
horizontal -->
    "\t".


%% invisible//
%
%  Matches a single non-visible (whitespace) character.  It
%  prefers a newline.
invisible -->
    nl.
invisible -->
    horizontal.


%% nl//
%
%  Matches a single newline character.
nl -->
    "\n".
