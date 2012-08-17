(* This is a demo example of a simple grammar for receipts.

   Consider the following EBNF:
     receipt ::= product* total
     product ::= "return" price ";"
              |  identifier price ";"
     total   ::= price "total"
     price   ::= digit+ "." digit digit

*)

structure SimpleReceipt =
(* Simple implementation, w/o a proper lexer *)
struct

  open ParserCombinators
  open CharParser

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 || <|> ??

  val price   : int charParser =
    repeat1 digit && (char #"." >> digit && digit << spaces) when
      (fn (xs, (y, z)) => Int.fromString (String.implode (xs@[y,z])))

  val total   = price << string "total" << spaces
  val product =
    (string "return" >> spaces >> price << char #";" << spaces wth op~)
      <|> (repeat1 letter >> spaces >> price << char #";" << spaces)

  val receipt : bool charParser =
    spaces >> repeat product && total << eos wth
      (fn (ps, tot) => List.foldl op+ 0 ps = tot)

end

structure LexReceipt =
(* An implementation that uses token parser. *)
struct

  open ParserCombinators
  open CharParser

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 || <|> ??

  structure ReceiptDef :> LANGUAGE_DEF =
  (* can also use the SimpleStyle functor from langparse.sml *)
  struct

    type scanner = char CharParser.charParser

    val commentStart   = NONE
    val commentEnd     = NONE
    val commentLine    = NONE
    val nestedComments = false

    val identLetter    = CharParser.letter
    val identStart     = identLetter
    val opStart        = fail "Operators not supported" : scanner
    val opLetter       = opStart
    val reservedNames  = ["return", "total"]
    val reservedOpNames= []
    val caseSensitive  = true

  end

  structure RTP = TokenParser (ReceiptDef)
  open RTP

  val price   =
      (lexeme (repeat1 digit && (char #"." >> digit && digit)) when
	      (fn (xs, (y, z)) => Int.fromString (String.implode (xs@[y,z]))))
	  ?? "price expression"

  val total   = price << reserved "total" ?? "total"
  val product =
      (reserved "return" >> price << symbol ";" wth op~) <|>
      (identifier >> price << symbol ";") ?? "product"
  val receipt =
      (repeat product && total << (eos ?? "end of stream")
	      wth (fn (xs, tot) => List.foldl op+ 0 xs = tot))
      
end

fun printRes pr =
    print (Sum.sumR (fn b => Bool.toString b ^ "\n") pr)

(* returns: SOME true *)
val example1 = "book 12.00; plant 2.55; 14.55 total"

(* returns: SOME false *)
val example2 = "book 12.00; plant 2.55; 12.55 total"

val example3 = "book 12.00; 14.55 total; plant 2.55"

fun doit s = printRes (CharParser.parseString LexReceipt.receipt s)

val _ = print "Lexer-less implementation:\n  Example 1: ";
    printRes (CharParser.parseString SimpleReceipt.receipt example1);
    print "  Example 2: ";
    printRes (CharParser.parseString SimpleReceipt.receipt example2);
    print "  Example 3: ";
    printRes (CharParser.parseString SimpleReceipt.receipt example3);
    print "\nToken-parser-using implementation:\n  Example 1: ";
    printRes (CharParser.parseString LexReceipt.receipt example1);
    print "  Example 2: ";
    printRes (CharParser.parseString LexReceipt.receipt example2);
    print "  Example 3: ";
    printRes (CharParser.parseString LexReceipt.receipt example3);
