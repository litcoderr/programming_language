fun merge (xs : int list, ys : int list) =
if null xs
    andalso null ys
then []
else if null xs
then ys
else if null ys
then xs
else if hd xs < hd ys
then hd xs :: merge(tl xs, ys)
else hd ys :: merge(xs, tl ys);

fun reverse (xs : int list) =
let fun res_aux(xs : int list, acc : int list) =
    if null xs
    then acc
    else res_aux(tl xs, hd xs :: acc) 
in res_aux(xs, [])
end;

fun pi (a : int , b : int, f : int -> int) =
if a = b
then f(b)
else f(a) * pi(a+1, b, f);

fun digits (a : int) =
let fun findDigit (x : int, dgt : int) =
        if (x div (dgt*10)) = 0
        then dgt
        else findDigit(x, dgt*10)

    fun g (x : int, dgt : int) =
        if dgt = 0
        then []
        else (x div dgt) :: g(x - (x div dgt)*dgt, dgt div 10)

in g(a, findDigit (a, 1))
end;

fun additivePersistence(a:int) =
if (a div 10) = 0
then 0
else
let fun sum_list (xs : int list) = if null xs
    then 0
    else hd(xs) + sum_list(tl(xs))
in additivePersistence(sum_list(digits(a))) +1
end;

fun digitalRoot(a:int) =
if (a div 10) = 0
then a
else
let fun sum_list (xs : int list) = if null xs
    then 0
    else hd(xs) + sum_list(tl(xs))
in digitalRoot(sum_list(digits(a)))
end;