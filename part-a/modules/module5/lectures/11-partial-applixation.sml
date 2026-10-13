
fun sorted3 x y z = z >= y andalso y >= x

fun reduce f acc xs =
    case xs of
            [] => acc
        |   x::xs' => reduce f (f(acc,x)) xs'

val is_nonegative = sorted3 0 0

val sumReduce = reduce (fn (x,y) => x+y) 0

fun range i j = if i > j then [] else i :: range (i+1) j

val countup = range 1

fun exists perdicate xs = 
    case xs of
            [] => false
        |   x::xs' => perdicate x orelse exists perdicate xs'

val no = exists (fn x => x=7) [4,11,13]

val hasZero = exists (fn x => x=0)

val incrementAll = List.map (fn x => x + 1)

val remozeZeros = List.filter (fn x => x <> 0)

