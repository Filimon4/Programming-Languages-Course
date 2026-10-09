
exception MyUndersirableCondition

exception MyOtherExceptions of int * int

fun mydiv(x, y) =
    if x = 0
    then raise MyUndersirableCondition
    else x div y

fun maxlist(xs, ex) =
    case xs of
        [] => raise  ex
    |   x::[] => x
    |   x::xs' => Int.max(x, maxlist(xs', ex))

val w = maxlist([], MyUndersirableCondition)
        handle MyUndersirableCondition => -1


