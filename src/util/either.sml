structure Either :> EITHER =
struct

  datatype ('a, 'b) either = Left of 'a | Right of 'b

  fun either lft rgt e =
    case e of
        Left x => lft x
      | Right y => rgt y

  fun lefts  xs =
    case xs of
        [] => []
      | Left x :: xs => x :: lefts xs
      | _ :: xs => lefts xs

  fun rights xs =
    case xs of
        [] => []
      | Right x :: xs => x :: rights xs
      | _ :: xs => rights xs

  fun part xs =
    case xs of
        [] => ([], [])
      | x :: xs =>
          let
            val (ls, rs) = part xs
          in
            case x of
                Left  l => (l :: ls, rs)
              | Right r => (ls, r :: rs)
          end

end
