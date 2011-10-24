(* Implementation of the parsing combinators *)

structure BasicParser : BASIC_PARSER =
  (* LL-style parsing combinators. *)
struct

  type pos = Pos.t
  type 't stream = ('t * pos) Stream.stream

  type ('a, 't) parser = pos * 't stream -> 'a * pos * pos * 't stream

  infix  2 -- ##
  infixr 1 <|> ??

  type message = string
  exception Fail of (pos * message list)

  (* Primitive Parsers *)

  fun succeed x (pos, ts) = (x, pos, pos, ts)
  fun fail s (pos, ts) = raise Fail (pos, [s])

  fun done x (pos, ts) =
        case Stream.front ts of
            Stream.Nil => (x, pos, pos, ts)
          | Stream.Cons _ =>
              raise Fail (pos, ["unexpected token"])

  fun any (pos, ts) =
        case Stream.front ts of
            Stream.Nil =>
              raise Fail (pos, ["unexepected end of stream"])
          | Stream.Cons ((x, pos), ts) => (x, pos, pos, ts)

  fun (p -- q) (pos, ts) =
        let val (x, posx, pos, ts) = p (pos, ts)
            val (y, posy, pos, ts) = q x (pos, ts)
        in
            (y, Pos.union posx posy, pos, ts)
        end

  fun (p ## q) (pos, ts) =
        p (pos, ts)
        handle Fail (p1, err1) =>
            q p1 (pos, ts)
            handle Fail (p2, err2) =>
                raise Fail (Pos.max p1 p2, err1 @ err2)

  fun (p <|> q) (pos, ts) =
      p (pos, ts)
      handle Fail (p1, err1) =>
	     (case Coord.compare (Pos.left pos, Pos.left p1) of
		  EQUAL => q (pos, ts)
		| _  => raise Fail (p1, err1))

  fun try p (pos, ts) =
      p (pos, ts)
      handle Fail (_, err1) => raise Fail (pos, err1)

  fun (p ?? s) (pos, ts) =
      p (pos, ts)
      handle Fail (pos, errs) => raise Fail (pos, errs @ ["expected "^s])

  fun lookahead p q (pos, ts) =
        let val (x, _, _, _) = p (pos, ts)
        in q x (pos, ts) end

  fun !! p (pos, ts) =
    let
      val (x, posx, pos, ts) = p (pos, ts)
    in
      ((x, posx), posx, pos, ts)
    end

  fun get f (pos, ts) = f pos (pos, ts)
      
  fun $ p (pos, ts) = p () (pos, ts)

  fun fix f (pos, ts) = f (fix f) (pos, ts)

  val initc        = Coord.init ""
  val initpos      = Pos.pos initc initc

  fun parsewith s f p ts =
        let val (x, _, _, _) = p (initpos, ts)
        in s x end
        handle Fail err => f err

(*  fun push ns p (pos, ts) =
      p (initpos, Stream.append ns ts)*)

  fun parse p = parsewith SOME (fn _ => NONE) p

  fun transform p ts =
    let
        fun trans (pos, ts) () =
            let
                val (x, _, pos', ts') = p (pos, ts)
            in
                Stream.Cons (x, Stream.lazy (trans (pos', ts')))
            end
            handle Fail err => Stream.Nil
    in
        Stream.lazy (trans (initpos, ts))
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
  fun p || q = p ## (fn _ => q)

  fun p wth f      = p -- succeed o f
  fun p suchthat g =
      p -- (fn x => if g x then succeed x else fail "match failed")
  fun p when f     = 
      p -- (fn x => case f x of SOME r => succeed r | NONE => fail "")
  fun p return x   = p -- (fn _ => succeed x)
  fun p guard g    = p ## (fn errpos => (ignore (g errpos); fail ""))

  fun seq ps = foldr (fn (ph, pt) => ph && pt wth op::) (succeed []) ps
  fun alt ps = foldr op|| (fail "") ps

  fun satisfy g = any suchthat g

  fun maybe f = any -- (fn x => case f x of SOME r => succeed r | _ => fail "")

  fun literal t = satisfy (fn t' => t = t')

  fun string ts = seq (List.map literal ts)
  fun oneof ts  = alt (List.map literal ts)

  fun opt p = p wth SOME || succeed NONE
  fun optional f x p = p wth f || succeed x


  fun first  p q   = p -- (fn x => q return x)
  fun second p q   = p -- (fn _ => q)
  fun middle p q r = p -- (fn _ => q -- (fn x => r return x))

  fun (p << q) = first p q
  fun (p >> q) = second p q

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
  fun separate  p q = separate p q || succeed []
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

