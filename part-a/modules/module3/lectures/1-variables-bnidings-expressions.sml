(* This is comment *)

val x = 34;
(* static enviroemnt:  x : int *)

val y = 17;
(* static enviroemnt:  y : int *)

val z = (x + y) + (y + 2);
(* static enviroemnt:  z : int *)

val abs_of_z = if z < 0 then 0 - z else z;

val abs_of_z_simpler = abs z