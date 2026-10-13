
fun reduce(f, acc, xs) =
    case xs of
            [] => acc
        |   x::xs => reduce(f, f(acc,x), xs)

fun f1 xs = reduce((fn (x, y) => x+y), 0, xs) (*sum list*)

(* all all list elem non-negative *)
fun f2 xs = reduce((fn (x, y) => x andalso y >= 0), true, xs)

fun f3 (xs, lo, hi) = reduce ((fn (x,y) => x + (if y >= lo andalso y <= hi then 1 else 0)), true, xs)
