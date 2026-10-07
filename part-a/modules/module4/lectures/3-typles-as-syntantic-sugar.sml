val a_pair = (3+1, 4+1)
val a_record = {second=4+2, first=3+1}
val another_pair= {2=5, 1=6}

val x = #1 a_pair + #2 another_pair 
(* x = 9 *)

val x = {3="hi", 1=true}

val y = {3="hi", 1=true, 2=3+2}
(* val y = (true,5,"hi") : bool * int * string *)