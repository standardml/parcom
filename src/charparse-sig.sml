(* Signatures for utility combinators for parsing character streams *)

signature CHAR_PARSER =
sig

    (* type synonym for Parsing.parser working on character streams *)
    type 'a charParser = ('a, char) Parsing.parser

    (* (oneOf cs) succeeds if the current character is in the supplied list of
       characters cs. Returns the parsed character. See also satisfy.

       vowel  = oneOf "aeiou"
     *)
    val oneOf : char list -> char charParser

    (* As the dual of oneOf, (noneOf cs) succeeds if the current character not
       in the supplied list of characters cs. Returns the parsed character. 

       consonant = noneOf "aeiou"
     *)
    val noneOf : char list -> char charParser

    (* (char c) parses a single character c. Returns the parsed character
       (i.e. c).

       semiColon  = char #";"
     *)
    val char : char -> char charParser

    (* (string s) parses a sequence of characters given by s. Returns the
       parsed string (i.e. s). 

       divOrMod    =   string "div" 
                   || string "mod"
     *)
    val string : string -> string charParser

    (* This parser succeeds for any character. Returns the parsed character. *)
    val anyChar : char charParser

    (* Parses an upper case letter (a character between #"A" and #"Z"). Returns
       the parsed character.
     *)
    val upper : char charParser

    (* Parses a lower case character (a character between #"a" and #"z").
       Returns the parsed character.
     *)
    val lower : char charParser

    (* Parses a letter (an upper case or lower case character). Returns the
       parsed character.
     *)
    val letter : char charParser

    (* Parses a letter or digit. Returns the parsed character. *)
    val alphaNum : char charParser
    (* Parses a digit (a character between '0' and '9'). Returns the parsed
       character.
     *)
    val digit : char charParser

    (* Parses a hexadecimal digit (a digit or a letter between 'a' and 'f' or
       'A' and 'F'). Returns the parsed character.
     *)
    val hexDigit : char charParser

    (* Parses an octal digit (a character between '0' and '7'). Returns the
       parsed character.
     *)
    val octDigit : char charParser

    (* Parses a newline character (#"\n"). Returns a newline character. *)
    val newLine : char charParser

    (* Parses a tab character (#"\t"). Returns a newline character. *)
    val tab : char charParser

    (* Parses a white space character (any character in " \v\f\t\r\n"). Returns
       the parsed character.
     *)
    val space : char charParser

    (* Skips zero or more white space characters. See also repeati
     *)
    val spaces : unit charParser

    (* The parser (satisfy f) succeeds for any character for which the supplied
       function f returns true. Returns the character that is actually parsed.
     *)
    val satisfy : (char -> bool) -> char charParser

end
