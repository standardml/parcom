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
  infixr 1 ||

  val price   : int charParser =
    repeat1 digit && (char #"." >> digit && digit << spaces) when
      (fn (xs, (y, z)) => Int.fromString (String.implode (xs@[y,z])))

  val total   = price << string "total" << spaces
  val product =
    (string "return" >> spaces >> price << char #";" << spaces wth op~)
      || (repeat1 letter >> spaces >> price << char #";" << spaces)

  val receipt : bool charParser =
    spaces >> repeat product && total wth
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
  infixr 1 ||

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
    lexeme (repeat1 digit && (char #"." >> digit && digit)) when
      (fn (xs, (y, z)) => Int.fromString (String.implode (xs@[y,z])))

  val total   = price << reserved "total"
  val product =
      (reserved "return" >> price << symbol ";" wth op~) ||
      (identifier >> price << symbol ";")
  val receipt =
      repeat product && total wth (fn (xs, tot) => List.foldl op+ 0 xs = tot)

end

fun markstream s fileName =
    let
	val initcoord  = Coord.init fileName
	fun aux xs crd =
	    Stream.lazy (fn _ => case Stream.front xs of
				     Stream.Nil          => Stream.Nil
				   | Stream.Cons (x, xs) =>
				     Stream.Cons ((x, Pos.pos crd crd),
						  (aux xs (Coord.nextchar crd))))
    in aux s initcoord
    end

(* returns: SOME true *)
val example1 =
    markstream (Stream.fromString "book 12.00; plant 2.55; 14.55 total") ""

fun printoptb ob =
    case ob of
	SOME b => print ("SOME " ^ Bool.toString b ^ "\n")
      | NONE => print "NONE\n"

(* returns: SOME false *)
val example2 =
    markstream (Stream.fromString "book 12.00; plant 2.55; 12.55 total") ""

val _ = print "Lexer-less implementation:\n  Example 1: ";
    printoptb (ParserCombinators.parse SimpleReceipt.receipt example1);
    print "  Example 2: ";
    printoptb (ParserCombinators.parse SimpleReceipt.receipt example2);
    print "\nToken-parser-using implementation:\n  Example 1: ";
    printoptb (ParserCombinators.parse LexReceipt.receipt example1);
    print "  Example 2: ";
    printoptb (ParserCombinators.parse LexReceipt.receipt example2);

