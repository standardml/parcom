(* Implementation of the parsing combinators *)

structure BasicParser :> BASIC_PARSER =
  (* LL-style parsing combinators. *)
struct

  type pos = Pos.t
  type coord = Coord.coord
  type 't stream = ('t * coord) Stream.stream
  open Sum

  datatype 't message  = Unexpected of 't option | Expected of string
		       | Message of string
  type ('a, 't) parser = bool ref -> coord * 't stream ->
			 (pos * 't message list, 'a * pos * coord * 't stream) sum

  infix  2 -- ##
  infixr 1 <|> ??

  (* Primitive Parsers *)

  fun succeed x _ (c, ts) = INR (x, Pos.pos c c, c, ts)
  fun fail    s _ (c, ts) = INL (Pos.pos c c, [Message s])

  fun eos _ (c, ts) =
      case Stream.front ts of
	  Stream.Nil => INR ((), Pos.pos c c, c, ts)
	| Stream.Cons ((x, c), _) => INL (Pos.pos c c, [Unexpected (SOME x)])

  fun any b (c, ts) =
        case Stream.front ts of
            Stream.Nil => INL (Pos.pos c c, [Unexpected NONE])
          | Stream.Cons ((x, c), ts) => (b := true; INR (x, Pos.pos c c, c, ts))

  fun (p -- q) b (c, ts) =
      bindR (p b (c, ts))
	    (fn (x, posx, c, ts) =>
		let val nb = ref false
		in map (fn e => (b := (!b orelse !nb); e))
		       (fn (y, posy, c, ts) => (y, Pos.union posx posy, c, ts))
		       (q x nb (c, ts))
		end)

  fun (p ## q) b (c, ts) =
      case p b (c, ts) of
	  INL (pf, errs) => q pf b (c, ts)
	| INR x => INR x

  fun (p <|> q) b (c, ts) =
      bindL (p b (c, ts)) (fn e => if !b then INL e else q b (c, ts))

  fun try p b (c, ts) =
      mapL (fn e => (b := false; e)) (p b (c, ts))

  fun (p ?? s) b (c, ts) =
      mapL (fn (pos, errs) => (pos, errs @ [Expected s])) (p b (c, ts))

  fun lookahead p q b (c, ts) =
      bindR (p b (c, ts)) (fn (x, _, _, _) => q x b (c, ts))

  fun !! p b (c, ts) =
      mapR (fn (x, posx, c, ts) => ((x, posx), posx, c, ts)) (p b (c, ts))

  fun get f b (c, ts) = f (Pos.pos c c) b (c, ts)
      
  fun $ p b (c, ts) = p () b (c, ts)

  fun fix f b (c, ts) = f (fix f) b (c, ts)

  val initc        = Coord.init ""

  fun runParser (p : ('a, 't) parser) ts =
      mapR #1 (p (ref false) (initc, ts))
  fun parsewith s f p =
      sum f s o runParser p

  (*  fun push ns p (pos, ts) =
      p (initpos, Stream.append ns ts)*)
  fun messageToString m =
      case m of
	  Unexpected (SOME t) => "unexpected token"
	| Unexpected NONE     => "unexpected end of stream"
	| Expected s          => s
	| Message m           => m

  fun printError fmt (p, msgs) =
      let fun unex msgs =
	      case List.filter (fn Unexpected _ => true | _ => false) msgs of
		  x :: _ => fmt x ^ ". "
		| _ => ""
	  fun exps xs = case xs of
			    [] => "" (* impossible case *)
			  | [x] => " or " ^ fmt x ^ ". "
			  | x :: xs => ", " ^ fmt x ^ exps xs
	  fun exp msgs =
	      case List.filter (fn Expected _ => true | _ => false) msgs of
		  [] => ""
		| [x] => "Expected " ^ fmt x ^ ". "
		| x :: xs  => "Expected " ^ fmt x ^ exps xs
	  fun msg msgs = (String.concatWith ". " o List.map (fn Message m => m))
			     (List.filter (fn Message _ => true | _ => false) msgs)
      in "Parse error at " ^ Pos.toString p ^ ": " ^
	 unex msgs ^ exp msgs ^ msg msgs ^ "\n"
      end

  fun parse fmt p = mapL (printError fmt) o runParser p
  fun simpleParse p = parse messageToString p

  fun transform p ts =
    let
        fun trans (pos, ts) () =
	    case p (ref false) (pos, ts) of
		INR (x, _, pos', ts') =>
                Stream.Cons (x, Stream.lazy (trans (pos', ts')))
	      | INL _ => Stream.Nil
    in
        Stream.lazy (trans (initc, ts))
    end

end

structure ParserCombinators :> PARSER_COMBINATORS =
struct

  open BasicParser

  fun flat3 (a, (b, c)) = (a, b, c)
  fun flat4 (a, (b, (c, d))) = (a, b, c, d)
  fun flat5 (a, (b, (c, (d, e)))) = (a, b, c, d, e)

  infixr 4 << >>
  infixr 3 &&
  infix  2 -- ##
  infix  2 wth suchthat return guard when
  infixr 1 || <|> ??

  fun p && q = p -- (fn x => q -- (fn y => succeed (x, y)))
  fun p || q = try p <|> q (*p ## (fn _ => q)*)

  fun p wth f      = p -- succeed o f
  fun p suchthat g =
      p -- (fn x => if g x then succeed x else fail "match failed")
  fun p when f     = 
      p -- (fn x => case f x of SOME r => succeed r | NONE => fail "")
  fun p return x   = p -- (fn _ => succeed x)
  (*fun p guard g    = p ## (fn errpos => (ignore (g errpos); fail ""))*)

  fun seq ps = foldr (fn (ph, pt) => ph && pt wth op::) (succeed []) ps
  fun alt ps = foldr op|| (fail "") ps

  fun satisfy g = any suchthat g

  fun maybe f = any -- (fn x => case f x of SOME r => succeed r | _ => fail "")

  fun literal t = try (satisfy (fn t' => t = t'))

  fun string ts = seq (List.map literal ts)
  fun oneof ts  = alt (List.map literal ts)

  fun opt p = p wth SOME || succeed NONE
  fun optional f x p = p wth f || succeed x


  fun first  p q   = p -- (fn x => q return x)
  fun second p q   = p -- (fn _ => q)
  fun middle p q r = p -- (fn _ => q -- (fn x => r return x))

  fun (p << q) = first p q
  fun (p >> q) = second p q

  fun done x = eos >> succeed x

  fun repeat p  = fix (fn rep => p && rep wth op:: || succeed [])

  fun repeat1 p = p && repeat p wth op::

  fun repeatn n p =
    let
      fun rep 0 () = succeed []
        | rep n () = p && ($(rep (n - 1))) wth op::
    in
      $(rep n)
    end

  fun repeatSkip  p = fix (fn rep => p >> rep || succeed ())
  fun repeatSkip1 p = p >> repeatSkip p

  fun separate1 p q = p && repeat (second q p) wth op::
  fun separate  p q = separate1 p q || succeed []
  fun sepEnd'   p q = first (separate p q) (opt q)
  fun sepEnd1'  p q = separate1 p q << opt q
  fun sepEnd    p q = repeat (p << q)
  fun sepEnd1   p q = repeat1 (p << q)

  fun join p = p -- (fn q => q)

  fun until    p q =
      let fun aux _ = (q return []) <|> p >> $ aux
      in $ aux end

  (* chaining of parsers *)
  fun chainr1 p opp   =
      p -- (fn v => (opp && chainr1 p opp wth (fn (f, v') => f (v, v')))
		      <|> succeed v)
  fun chainr  p opp d = chainr1 p opp <|> succeed d
  fun chainl1 p opp   =
      p && (repeat (opp && p)) wth
        (fn (v, ts) => List.foldl (fn ((f, vr), vl) => f (vl, vr)) v ts)
  fun chainl  p opp d = chainl1 p opp <|> succeed d

  fun not p = try (p >> fail "unexpected token") <|> succeed ()

  (***** pre/in/post-fix parsing *****)

  datatype associativity = Left | Right | Non

  datatype 'a opr =
      Prefix of int * ('a -> 'a)
    | Infix of associativity * int * ('a * 'a -> 'a)
    | Postfix of int * ('a -> 'a)

  datatype 'a fixityitem =
      Atm of 'a
    | Opr of 'a opr

  fun assoc (Prefix _) = Non
    | assoc (Infix(asc, _, _)) = asc
    | assoc (Postfix _) = Non

  fun prec (Prefix(n, _)) = n
    | prec (Infix(_, n, _)) = n
    | prec (Postfix(n, _)) = n

  fun resolvefixity ys =
      let fun resolve (xs, c as Atm _, ys) =
                 next (c::xs, ys)
            | resolve (xs, c as Opr(Prefix _), ys) =
                 next (c::xs, ys)
            | resolve (x::[], c as Opr(Infix _), ys) =
                 next (c::x::[], ys)
            | resolve (x::(c' as Opr(f'))::xs, c as Opr(f as Infix _), ys) =
                 if prec(f) > prec(f') then next (c::x::c'::xs, ys)
                 else if prec(f') > prec(f) then reduce (x::c'::xs, c::ys)
                 else (case (assoc(f'), assoc(f))
                         of (Left, Left) => reduce (x::c'::xs, c::ys)
                          | (Right, Right) => next (c::x::c'::xs, ys)
                          | _ => fail "Operator ambiguous")
            | resolve(x::[], c as Opr(Postfix _), ys) =
                 reduce (c::x::[], ys)
            | resolve (x::(c' as Opr(f'))::xs, 
                       c as Opr(f as Postfix _), ys) =
                 if prec(f) > prec(f') then reduce (c::x::c'::xs, ys)
                 else if prec(f') > prec(f) then reduce (x::c'::xs, c::ys)
                 else fail "Operator ambiguous"
            | resolve _ = fail "Atom/operator mismatch"

          and reduce (Atm(a)::Opr(Prefix(_, cprefix))::xs, ys) =
                 next(Atm(cprefix(a))::xs, ys)
            | reduce (Atm(a)::Opr(Infix(_, _, cinfix))::Atm(a')::xs, ys) =
                 next(Atm(cinfix(a', a))::xs, ys)
            | reduce (Opr(Postfix(_, cpostfix))::Atm(a)::xs, ys) =
                 next(Atm(cpostfix(a))::xs, ys)
            | reduce _ = fail "Atom/operator mismatch"

          and next (Atm(a)::[], []) = succeed a
            | next (xs, []) = reduce (xs, [])
            | next (xs, y::ys) = resolve (xs, y, ys)

       in next ([], ys) end

  fun resolvefixityadj cadj cassoc ys =
      let fun resolve (Atm(a)::xs, Atm(a'), ys) =

            (* treat adjacent tokens as if they have an infix operator 
               of high precedence between them -- Tom *)
                 resolve (Atm(a)::xs, Opr(Infix(cassoc, 999, cadj)),
                          Atm(a')::ys)
            | resolve (xs, Atm(a), ys) =
                 next (Atm(a)::xs, ys)
            | resolve (xs, c as Opr(Prefix _), ys) =
                 next (c::xs, ys)
            | resolve (x::[], c, ys) =
                 next (c::x::[], ys)
            | resolve ((c' as Opr _)::xs, c, ys) =
                 reduce (c'::xs, c::ys)
            | resolve (x::(c' as Opr(f'))::xs, c as Opr(f), ys) =
                 if prec(f) > prec(f') then next (c::x::c'::xs, ys)
                 else if prec(f') > prec(f) then reduce (x::c'::xs, c::ys)
                 else (case (assoc(f'), assoc(f))
                         of (Left, Left) => reduce (x::c'::xs, c::ys)
                          | (Right, Right) => next (c::x::c'::xs, ys)
                          | _ => fail "Operator ambiguous")
            | resolve _ = fail "Operator mismatch"

          and reduce (Atm(a)::Opr(Prefix(_, cprefix))::xs, ys) =
                 next (Atm(cprefix(a))::xs, ys)
            | reduce (Atm(a)::Opr(Infix(_, _, cinfix))::Atm(a')::xs, ys) =
                 next (Atm(cinfix(a', a))::xs, ys)
            | reduce (Opr(Postfix(_, cpostfix))::Atm(a)::xs, ys) =
                 next (Atm(cpostfix(a))::xs, ys)
            | reduce _ = fail "Operator mismatch"

          and next (Atm(a)::[], []) = succeed a
            | next (xs, []) = reduce(xs, [])
            | next (xs, y::ys) = resolve(xs, y, ys)

       in next ([], ys) end

  fun parsefixity p =
      (repeat1 p) -- (fn ys => resolvefixity ys)

  fun parsefixityadj p assoc adj =
      (repeat1 p) -- (resolvefixityadj adj assoc)

end
