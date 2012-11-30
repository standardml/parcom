(* Character parsers implementation, useful for building lexers *)

structure CharParser :> CHAR_PARSER =
struct

    open ParserCombinators
    infixr 4 << >>
    infixr 1 ??

    type 'a charParser = ('a, char) parser
    type message = char message

    fun oneOf  xs = try (satisfy (fn x => List.exists (fn y => x = y) xs))
    fun noneOf xs = try (satisfy (fn x => List.all (fn y => x <> y) xs))
    fun char   x  = try (satisfy (fn y => x = y)) ?? "'" ^ str x ^ "'"
    fun string s  =
        let fun string_aux xs = case xs of
                                    nil        => succeed s
                                  | (x :: xs') => char x >> string_aux xs'
        in string_aux (String.explode s) end

    val anyChar   = any
    val upper     = try (satisfy Char.isUpper) ?? "upper case letter"
    val lower     = try (satisfy Char.isLower) ?? "lower case letter"
    val letter    = try (satisfy Char.isAlpha) ?? "letter"
    val alphaNum  = try (satisfy Char.isAlphaNum) ?? "alphanumeric character"
    val digit     = try (satisfy Char.isDigit) ?? "digit"
    val hexDigit  = try (satisfy Char.isHexDigit) ?? "hexadecimal digit"
    val octDigit  = try (satisfy (fn x => Char.isDigit x 
					  andalso Char.<= (x, #"7"))) ?? "octal digit"
    val newLine   = char #"\n" ?? "'\n'"
    val tab       = char #"\t" ?? "'\t'"
    val space     = try (satisfy Char.isSpace)
    val spaces    = repeatSkip space
    val satisfy   = satisfy

    fun messageToString m =
	case m of
	    Unexpected (SOME t) => "unexpected '" ^ str t ^ "'"
	  | Unexpected NONE     => "unexpected end of stream"
	  | Expected s          => s
	  | Message m           => m

    fun parseChars p = parse messageToString p
    fun parseString p s =
	let val s = CoordinatedStream.coordinate (fn x => Stream.hd x = #"\n" handle Stream.Empty => false) (Coord.init "-")
						 (Stream.fromString s)
	in parseChars p s
	end

end
