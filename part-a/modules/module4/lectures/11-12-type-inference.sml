fun sum_triple1 (triple: int * int * int) = #1 triple + #2 triple + #3 triple

fun full_name1 {first=x, middle=y, last=z} = x ^ " " ^ y " " ^ " " ^ z

fun partial_sum(x, y, z) = x + z

fun partial_name {first=x, middle=y, last=z} = x ^ " " ^ y

fun same_think(x, y) = 
    if x=y then "yse" else "no"

fun is_three x =
    if x = 3 then "yes" else "no"