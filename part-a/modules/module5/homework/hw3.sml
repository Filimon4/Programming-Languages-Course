(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

(* val only_capitals = fn : string list -> string list *)
fun only_capitals (ls) =
    List.filter (fn x => Char.isUpper(String.sub(x, 0)) ) ls

(* val longest_string1 = fn : string list -> string *)
fun longest_string1(ls) = List.foldl (fn (s, acc) => if String.size s > String.size acc then s else acc) "" ls

(* val longest_string2 = fn : string list -> string *)
fun longest_string2(ls) = List.foldl (fn (s, acc) => if String.size s >= String.size acc then s else acc) "" ls

(* val longest_string_helper = fn : (int * int -> bool) -> string list -> string *)
fun longest_string_helper f strings =
	List.foldl (fn (s, acc) => if f(String.size(s), String.size(acc)) then s else acc) "" strings

(* val longest_string3 = fn : string list -> string *)
fun longest_string3(ls) = longest_string_helper (fn (y, x) => y > x) ls  

(* val longest_string4 = fn : string list -> string *)
fun longest_string4(ls) = longest_string_helper (fn (y, x) => y >= x) ls  

(* val longest_capitalized = fn : string list -> string *)
fun longest_capitalized ls = (longest_string1 o only_capitals) ls

(* val rev_string = fn : string -> string *)
fun rev_string string = (String.implode o List.rev o String.explode) string

(* val first_answer = fn : (’a -> ’b option) -> ’a list -> ’b *)
fun first_answer f ls = 
	case ls of
			[] => raise NoAnswer
		|	x::ls' => 
				case f(x) of
						NONE => first_answer f ls'
					|	SOME z => z  

(* val all_answers = fn : (’a -> ’b list option) -> ’a list -> ’b list option *)
fun all_answers f ls =
	let
		fun helper(acc, list) =
			case list of
					[] => SOME acc
				|	x::list' =>
						case f x of
								NONE => NONE
							|	SOME s => helper(s @ acc, list')
	in 
		helper([], ls)
	end

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* val count_wildcards = fn : pattern -> int *)
fun count_wildcards pattern = g (fn _ => 1) (fn _ => 0) pattern

(* val count_wild_and_variable_lengths = fn : pattern -> int *)
fun count_wild_and_variable_lengths pattern = count_wildcards pattern + g (fn _ => 0) String.size pattern

(* val count_some_var = fn : string * pattern -> int *)
fun count_some_var (s, pattern) =
	g (fn _ => 0) (fn x => if x = s then 1 else 0) pattern

(* val check_pat = fn : pattern -> bool *)
fun check_pat pattern =
	let
		fun getString p =
			case p of
					Variable x => [x]
				|	TupleP xs => List.foldl (fn (i, acc) => acc @ getString i ) [] xs
				| 	ConstructorP(_,p) => getString p
				| 	_ => []
		
		fun duplicateExist ls =
			case ls of
					[] => false
				|	h::t => List.exists (fn str => str = h) t
	in
		not ((duplicateExist o getString) pattern)
	end


(* val match = fn : valu * pattern -> (string * valu) list option *)
fun match pair =
	case pair of
			(_,Wildcard) => SOME []
		|	(v, Variable s) => SOME [(s, v)]
		|	(Unit, UnitP) => SOME []
		|	(Const v, ConstP s) => if v = s then SOME [] else NONE
		|	(Tuple t', TupleP tp') => if List.length t' = List.length tp'
				then all_answers match (ListPair.zip(t', tp'))
				else NONE
		|	(Constructor(s, v), ConstructorP(s', p)) => if s = s' then match (v, p) else NONE
		|	(_, _) => NONE


(* val first_match = fn : valu -> pattern list -> (string * valu) list option *)
fun first_match va ptns = SOME (first_answer (fn x => match((va, x))) ptns) handle NoAnswer => NONE