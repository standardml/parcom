signature EITHER =
sig

  datatype ('a, 'b) either = Left of 'a | Right of 'b

  val either : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) either -> 'c
  val lefts  : ('a, 'b) either list -> 'a list
  val rights : ('a, 'b) either list -> 'b list
  val part   : ('a, 'b) either list -> 'a list * 'b list

end
