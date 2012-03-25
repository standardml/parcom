(* Several functors that facilitate building languages in certain style *)

functor SimpleStyle (Def : MINI_LANGUAGE_DEF) :> LANGUAGE_DEF =
struct

    open ParserCombinators
    open CharParser
    infixr 1 <|>

    type scanner = char CharParser.charParser

    val commentStart    = NONE
    val commentEnd      = NONE
    val commentLine     = NONE
    val nestedComments  = false

    val identStart      = letter <|> char #"_"
    val identLetter     = alphaNum <|> oneOf (String.explode "_'")
    val opLetter        = oneOf (String.explode ":!#$%&*+./< =>?@\\^|-~")
    val opStart         = opLetter

    val reservedNames   = Def.reservedNames
    val reservedOpNames = Def.reservedOpNames

    val caseSensitive   = true

end

functor JavaStyle (Def : MINI_LANGUAGE_DEF) :> LANGUAGE_DEF =
struct

    open ParserCombinators
    open CharParser
    infixr 1 <|>

    type scanner = char CharParser.charParser

    val commentStart    = SOME "/*"
    val commentEnd      = SOME "*/"
    val commentLine     = SOME "//"
    val nestedComments  = true

    val identStart      = letter
    val identLetter     = alphaNum <|> oneOf (String.explode "_'")
    val opLetter        = oneOf (String.explode ":!#$%&*+./< =>?@\\^|-~")
    val opStart         = opLetter

    val reservedNames   = Def.reservedNames
    val reservedOpNames = Def.reservedOpNames

    val caseSensitive   = false

end

functor MLStyle (Def : MINI_LANGUAGE_DEF) :> LANGUAGE_DEF =
struct

    open ParserCombinators
    open CharParser
    infixr 1 <|>

    type scanner = char CharParser.charParser

    val commentStart    = SOME "(*"
    val commentEnd      = SOME "*)"
    val commentLine     = NONE
    val nestedComments  = true

    val identStart      = letter
    val identLetter     = alphaNum <|> oneOf (String.explode "_'")

    (* did I miss anything? add to much? *)
    val opLetter        = oneOf (String.explode ":!#$%&*+./<=>?@\\^|-~")
    val opStart         = opLetter

    val reservedNames   = Def.reservedNames
    val reservedOpNames = Def.reservedOpNames

    val caseSensitive   = true

end
