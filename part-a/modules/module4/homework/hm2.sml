(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* val all_except_option = fn : string * string list -> string list option *)
fun all_except_option(x: string, xs: string list) =
    case xs of
            [] => NONE
        |   y::xs' => if same_string(y, x)
                then SOME xs'
                else case all_except_option(x, xs') of
                        SOME tail => SOME(y::tail)
                    |   _ => NONE

(* val get_substitutions1 = fn : string list list * string -> string list *)
fun get_substitutions1(xs: string list list, x: string) =
    case xs of
            [] => []
        |   y::xs' => case all_except_option(x, y) of
                    SOME l => l @ get_substitutions1(xs', x)
                |   NONE   => [] @ get_substitutions1(xs', x)

(* val get_substitutions2 = fn : string list list * string -> string list *)
fun get_substitutions2(xs: string list list, x: string) =
    let
        fun help(xs: string list list, x: string, acc: string list) =
            case xs of
                [] => acc
            |   y::xs' => case all_except_option(x, y) of
                        SOME l => help(xs', x, acc @ l)
                    |   NONE   => help(xs', x, acc)

    in help(xs, x, []) end

(* val similar_names = fn : string list list * {first:string, last:string, middle:string}
-> {first:string, last:string, middle:string} list *)
fun similar_names(xs : string list list, name: {first:string, last:string, middle:string}) =
    case name of
        {first=x, last=y, middle=z} =>
            let
                val getAllFirstNames = get_substitutions2(xs, x)

                fun helper(names: string list) =
                    case names of
                            [] => []
                        |   n::names' => [{first=n, last=y, middle=z}] @ helper(names')
            in
                [name] @ helper(getAllFirstNames)
            end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* val card_color = fn : card -> color *)
fun card_color(c: card) =
    case c of
            {1=Spades, 2=_} => Black
        |   {1=Clubs, 2=_} => Black
        |   _ => Red

(* val card_value = fn : card -> int *)
fun card_value(c: card) =
    case c of
            {1=_, 2=Ace} => 11
        |   {1=_, 2=Num x} => x
        |   _ => 10

(* val remove_card = fn : card list * card * exn -> card list *)
fun remove_card(cs, c, exp) =
    let
        fun help(cs, acc) =
            case cs of
                    [] => raise exp
                |   curr_c::cs' => 
                        if curr_c = c then 
                            cs'
                        else 
                            help(cs', curr_c::acc) 
    in help(cs, []) end

(* val all_same_color = fn : card list -> bool *)
fun all_same_color(cs) =
    case cs of
            [] => true
        |   _::[] => true
        |   f::s::cs' => (card_color(f) = card_color(s)) andalso all_same_color(s::cs')

(* val sum_cards = fn : card list -> int *)
fun sum_cards cs =
    let 
        fun helper(cs, acc) =
            case cs of
                    [] => acc
                |   c::cs' => helper(cs', card_value(c) + acc)
    in helper(cs, 0) end

(* val score = fn : card list * int -> int *)
fun score (cards, goal) =
  let
      val sum = sum_cards(cards)
      val prelim_score = if sum > goal
			 then 3 * (sum - goal)
			 else (goal - sum)
				  
  in
      if all_same_color(cards)
      then prelim_score div 2
      else prelim_score	       
  end

(* val officiate = fn : card list * move list * int -> int *)
fun officiate (cards, moves, goal) =
  let
      fun play (held, goal, cards, moves) =
        if sum_cards(held) > goal
        then score(held, goal)
        else case moves of
            [] => score(held, goal)
            | move::moves' => case move of
                        Discard(card) => play(remove_card(held, card, IllegalMove), goal, cards, moves')
                    | Draw => case cards of
                            [] => score(held, goal)
                            | card::[] => play(card::held, goal, [], moves')
                            | card::cards' => play(card::held, goal, cards', moves')
							   
  in
      play([], goal, cards, moves)
  end   
	

