
exception ListLengthMishmatch

fun old_zip3 (l1, l2, l3) =
    if null l1 andalso null l2 andalso null l3
    then []
    else if null l1 orelse null l2 orelse null l3
    then raise ListLengthMishmatch
    else (hd l1, hd l2, hd l3) :: old_zip3(tl l1, tl l2, tl l3)

fun shallow_zip3(l1, l2, l3) =
    case l1 of
            [] => 
                (case l2 of
                        [] => 
                        (
                            case l3 of
                                    [] => []
                                |   _ => raise ListLengthMishmatch
                        )
                    |   _ => raise ListLengthMishmatch
                )
        |   hd1::tl1 =>
            (case l2 of
                    [] => raise ListLengthMishmatch
                |   hd2::tl2 => 
                    (case l3 of
                            [] => raise ListLengthMishmatch
                        |   hd3::tl3 => (hd1, hd2, hd3)::shallow_zip3(tl1, tl2, tl3)
                    )
            ) 

(* right version *)
fun zip3 list_triple =
    case list_triple of
            ([], [], []) => []
        |   (hd1::tl1, hd2::tl2, hd3::tl3) => (hd1, hd2, hd3)::zip3(tl1, tl2, tl3)
        |   _ => raise ListLengthMishmatch

fun unzip3 lst =
    case lst of
            [] => ([],[],[])
        |   (a,b,c)::tl => let val (l1,l2,l3) = unzip3 tl
                            in (a::l1, b::l2, c::l3) end

fun nondecreasing xs =
    case xs of
            [] => true
        |   x::xs' => case xs' of [] => true | y::ys' => x <= y andalso nondecreasing xs'

fun nondecreasing2 xs =
    case xs of
            [] => true
        |   _::[] => true
        |   head::neck::rest =>  head <= neck andalso nondecreasing(neck::rest)

datatype sgn = P | N | Z

fun multsign(x1, x2) =
    let fun sign x = if x=0 then Z else if x > 0 then P else N
    in
        case (sign x1, sign x2) of
                (Z, _) => Z
            |   (_, Z) => Z
            |   (P, P) => P
            |   (N, N) => P
            |   (N, P) => N 
            |   (P, N) => N 
    end

fun len xs =
    case xs of
            [] => 0
        |   _::tl => 1 + len tl