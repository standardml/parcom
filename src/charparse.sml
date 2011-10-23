(* Character parsers implementation, useful for building lexers *)

structure CharParser :> CHAR_PARSER =
struct

    open Parsing
    infixr 4 << >>

    type 'a charParser = ('a, char) parser

    fun oneOf  xs = satisfy (fn x => List.exists (fn y => x = y) xs)
    fun noneOf xs = satisfy (fn x => List.all (fn y => x <> y) xs)
    fun char   x  = satisfy (fn y => x = y)
    fun string s  =
        let fun string_aux xs = case xs of
                                    nil        => succeed s
                                  | (x :: xs') => char x >> string_aux xs'
        in string_aux (String.explode s) end

    val anyChar   = any
    val upper     = satisfy Char.isUpper
    val lower     = satisfy Char.isLower
    val letter    = satisfy Char.isAlpha
    val alphaNum  = satisfy Char.isAlphaNum
    val digit     = satisfy Char.isDigit
    val hexDigit  = satisfy Char.isHexDigit
    val octDigit  = satisfy (fn x => Char.isDigit x andalso Char.<= (x, #"7"))
    val newLine   = char #"\n"
    val tab       = char #"\t"
    val space     = satisfy Char.isSpace
    val spaces    = repeatSkip space
    val satisfy   = satisfy

end
