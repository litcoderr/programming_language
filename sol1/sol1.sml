(*Q1 merge*)
fun merge(a: int list, b: int list): int list =
    if null a then
        b
    else if null b then
        a
    else if hd a < hd b then
        hd a::merge(tl a, b)
    else
        hd b::merge(a, tl b)

(*Q2 reverse*)
fun reverse(a: int list): int list =
    let fun rev_helper(a: int list, b: int list): int list =
        if null a then
            b
        else
            rev_helper(tl a, (hd a)::b)
    in
        rev_helper(a, [])
    end

(*Q3 pi*)
fun pi(a: int, b: int, f: int->int): int =
    if a > b then
        1
    else
        pi(a+1, b, f) * f(a)

(*Q4 digitis*)
fun digits_helper(a: int, l: int list): int list =
    if a=0 then
        l
    else
        digits_helper(a div 10, (a mod 10)::l)

fun digits(a: int): int list =
    digits_helper(a, [])

(*Q5 Digital Roots and Additive Persistence*)
fun sum(a: int list): int =
    if null a then
        0
    else if null (tl a) then
        hd a
    else
        (hd a) + sum(tl a)

fun q5_helper(a: int list, temp_num: int): (int*int) = 
    if null (tl a) then
        (hd a, temp_num)
    else
        q5_helper(digits(sum(a)), temp_num+1)

fun additivePersistence(a: int): int =
    let val result = q5_helper(digits(a), 0)
    in
        #2result
    end

fun digitalRoot(a: int): int =
    let val result = q5_helper(digits(a), 0)
    in
        #1result
    end
