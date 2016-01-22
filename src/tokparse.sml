(* Token parsers, a simple lexer implementation based on language definitions *)

functor TokenParser (Lang : LANGUAGE_DEF) :> TOKEN_PARSER =
struct

  fun elem x = List.exists (fn y => x = y)
  fun notElem x = List.all (fn y => x <> y)

  open ParserCombinators
  open CharParser
  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 || <|> ??

  type 'a charParser = 'a charParser

  val lineComment =
    let
      fun comLine _  = newLine <|> done #"\n" <|> (anyChar >> $ comLine)
    in
      case Lang.commentLine of
          SOME s => string s >> $ comLine return ()
        | NONE   => fail "Single-line comments not supported"
    end

  val mlComment =
    case (Lang.commentStart, Lang.commentEnd) of
        (SOME st, SOME ed) =>
          let
            fun bcNest _ = try (string st) >> $contNest
            and contNest _ = try (string ed return ()) <|> ($bcNest <|> (anyChar return ())) >> $contNest
            val bcU = try (string st) >> repeat (not (string ed) >> anyChar) >> string ed return ()
          in
            if Lang.nestedComments then $ bcNest else bcU
          end
      | _ => fail "Multi-line comments not supported"
  val comment        = lineComment <|> mlComment

  val whiteSpace = repeatSkip ((space return ()) || comment)
  fun lexeme p = p << whiteSpace
  fun symbol s = lexeme (string s)

  val name =
    Lang.identStart
      && repeat Lang.identLetter
      wth implode o op::

  val identifier =
    try (lexeme (name suchthat (fn x => notElem x Lang.reservedNames)))

  fun reserved kw =
    if elem kw Lang.reservedNames then
      try (lexeme (name suchthat (fn x => x = kw)) return ())
    else
      fail "Not a reserved name"

  val opName =
    Lang.opStart
      && repeat Lang.opLetter
      wth implode o op::

  val operator =
    try (lexeme (opName suchthat (fn x => notElem x Lang.reservedOpNames)))

  fun reservedOp rop =
    if elem rop Lang.reservedOpNames then
      try (lexeme (opName suchthat (fn x => x = rop)) return ())
    else
      fail "Not a reserved operator"

  fun parens p = middle (symbol "(") p (symbol ")")
  fun braces p = middle (symbol "{") p (symbol "}")
  fun brackets p = middle (symbol "<") p (symbol ">")
  fun squares p = middle (symbol "[") p (symbol "]")

  val semi = symbol ";"
  val comma = symbol ","
  val colon = symbol ":"
  val dot = symbol "."
  fun semiSep p = separate p semi
  fun semiSep1 p = separate1 p semi
  fun commaSep p = separate p comma
  fun commaSep1 p = separate1 p comma

  val chrEscape =
    string "\\"
      && (anyChar wth Char.toString)
      wth op^
      when Char.fromString

  val charLiteral =
    middle
      (char #"'")
      (chrEscape <|> anyChar)
      (symbol "'")

  val stringLiteral =
    (middle
      (char #"\"")
      (repeat (chrEscape <|> (anyChar suchthat (fn x => x <> #"\""))))
      (symbol "\"")) wth String.implode

  fun dig d =
    if Char.isDigit d then
      Char.ord d - Char.ord #"0"
    else
      Char.ord (Char.toLower d) - Char.ord #"a" + 10

  fun transnum b = List.foldl (fn (s, d) => b*d + s) 0
  val decimal = repeat1 digit wth transnum 10 o List.map dig
  val hexadecimal = repeat1 hexDigit wth transnum 16 o List.map dig
  val octal = repeat1 octDigit wth transnum 8 o List.map dig
  val positive = (char #"0" >> ((char #"x" >> hexadecimal) || octal)) || decimal
  val integer = lexeme ((char #"-" >> positive wth op~) || positive)

end
