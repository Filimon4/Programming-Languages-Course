fun filter(f, xs) =
    case xs of
        [] => []
        |   x::xs' => if f x then x::(filter(f, xs')) else filter(f, xs')

fun allShortedThen1(xs, s) = 
    filter (fn x => String.size x < (print "!"; String.size s), xs)

(* closure used *)
fun allShortedThen2(xs, s) = 
    let
        val i = (print "!"; String.size s)
    in filter (fn x => String.size x < i, xs) end
    