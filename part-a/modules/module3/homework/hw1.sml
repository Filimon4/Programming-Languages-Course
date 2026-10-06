
fun is_older (x : int*int*int, y : int*int*int) =
    if #1 x < #1 y
    then true
    else if #1 x > #1 y
    then false
    else if #2 x < #2 y
    then true
    else if #2 x > #2 y
    then false
    else if #3 x < #3 y
    then true
    else false

fun number_in_month (dates: (int * int * int) list, m : int) =
    if null dates
    then 0
    else if #2 (hd dates) = m
    then 1 + number_in_month(tl dates, m)
    else number_in_month(tl dates, m)

fun number_in_months (dates: (int * int * int) list, m : int list) =
    if null dates
    then 0
    else if null m
    then 0
    else
        let
            val n = number_in_month(dates, hd m)
        in
            n + number_in_months(dates, tl m)
        end

fun dates_in_month (dates: (int * int * int) list, m : int) =
    if null dates
    then []
    else
        let
            val isValidDate = number_in_month(hd dates :: [], m)
        in 
            if isValidDate > 0
            then (hd dates)::dates_in_month(tl dates, m)
            else dates_in_month(tl dates, m)
        end

fun dates_in_months(dates : (int * int * int) list, m : int list) =
    if null dates
    then []
    else if null m
    then []
    else
        let
            val arr = dates_in_month(dates, hd m)
        in
            if null arr
            then dates_in_months(dates, tl m)
            else arr @ dates_in_months(dates, tl m)
        end
    
fun get_nth(list : string list, n : int) =
    if null list
    then ""
    else
        let
            fun findIndex(c : string list, t : int) =
                if t = n
                then hd c
                else findIndex(tl c, t+1)
        in
            if n = 1
            then hd list
            else findIndex(list, 1)
        end

fun date_to_string(year : int, mounth : int, day : int) =
    let
        val nameMounths = [
            "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"
        ]
        fun getMounthByInt(i : int) = get_nth(nameMounths, i)
    in
        getMounthByInt(mounth) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end

fun number_before_reaching_sum (sum : int, lst : int list) =
    if sum <= hd lst
    then 0
    else 1 + number_before_reaching_sum(sum - hd lst, tl lst)

fun what_month (day_of_year : int) =
    let 
	     val month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	     1 + number_before_reaching_sum(day_of_year, month_lengths)
    end

fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month day1 :: month_range(day1 + 1, day2)

fun oldest(dates : (int * int * int) list) = 
    if null dates
    then NONE
    else
        let
          fun oldest_nonenpty(d : (int * int * int) list) =
            if null (tl d)
            then hd d
            else 
                let
                    val tl_date = oldest_nonenpty(tl d)
                in
                    if is_older(hd d, tl_date)
                    then hd d
                    else tl_date
                end 
        in
          SOME (oldest_nonenpty(dates))
        end