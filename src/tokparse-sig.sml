(* Signatures of language definitions for lexing and lexers
   based on those definitions *)

signature LANGUAGE_DEF =
sig

    type scanner = char CharParser.charParser

    (* multiline comment start/end sequence *)
    val commentStart    : string
    val commentEnd      : string

    (* single line comment start *)
    val commentLine     : string

    (* do the multiline comments support nesting *)
    val nestedComments  : bool

    (* parsers for first and subsequent letters of identifiers *)
    val identStart      : scanner
    val identLetter     : scanner

    (* parsers for first and subsequent chars of operators *)
    val opStart         : scanner
    val opLetter        : scanner

    (* reserved keywords and operators *)
    val reservedNames   : string list
    val reservedOpNames : string list

    (* is the language case sensitive *)
    val caseSensitive   : bool

end

signature TOKEN_PARSER =
sig

    type 'a charParser = 'a CharParser.charParser

    val identifier     : string charParser
    val reserved       : string -> unit charParser
    val operator       : string charParser
    val reservedOp     : string -> unit charParser
           
    val charLiteral    : char charParser
    val stringLiteral  : string charParser
    (* val natural        : IntInf charParser*)
    val integer        : int charParser
    (* val float          : real charParser *)
    (* val naturalOrFloat : CharParser st (Either Integer Double)*)
    val decimal        : int charParser
    val hexadecimal    : int charParser
    val octal          : int charParser

    val symbol         : string -> string charParser
    val lexeme         : 'a charParser -> 'a charParser
    val whiteSpace     : unit charParser

    val parens         : 'a charParser -> 'a charParser
    val braces         : 'a charParser -> 'a charParser
    val brackets       : 'a charParser -> 'a charParser
    val squares        : 'a charParser -> 'a charParser

    val semi           : string charParser
    val comma          : string charParser
    val colon          : string charParser
    val dot            : string charParser
    val semiSep        : ('a charParser) -> ('a list) charParser
    val semiSep1       : ('a charParser) -> ('a list) charParser
    val commaSep       : ('a charParser) -> ('a list) charParser
    val commaSep1      : ('a charParser) -> ('a list) charParser
 
end
