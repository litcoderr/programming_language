datatype sgn = P | N | Z

fun multsign(a: sgn, b: sgn) =
    case (a, b) of
        (Z, _) => Z
        | (_, Z) => Z
        | (P, P) => P
        | (_, _) => N

fun converter(a: int) =
    if a > 0 then P
    else if a < 0 then N
    else Z

fun sign (a:int) (b: int) (f: (sgn * sgn)->sgn) =
    let
        val a_sign = converter(a);
        val b_sign = converter(b);
    in
        f(a_sign, b_sign)
    end

datatype set = S of { insert : int -> set, 
                      member : int -> bool }

(* implementation of sets: this is the fancy stuff, but clients using
   this abstraction do not need to understand it *)
val empty_set =
  let
    fun make_set xs = 
        let fun contains i = List.exists (fn x => i=x) xs
        in
            S({insert = fn i => if contains(i)
                                then make_set(xs)
                                else make_set(i::xs),
                member = contains
            })
        end
  in
    make_set [] (*  set value S{...}*)
  end 

val S(s1) = empty_set
val S(s2) = (#insert s1) 3

val arr = [1,2,3,4]

fun square x = x * x
fun map(f, xs) =
    case xs of
        x::xs_ => f(x)::map(f, xs_)
      | [] => []

fun mod_2 x = if x mod 2 = 1 then true else false
fun filter(f, xs) =
    case xs of
        x::xs_ => if f(x) then x::filter(f, xs_)
                    else filter(f, xs_)
        | []  => []

fun add(acc, x) = acc+x
fun bigger(acc, x) = if acc > x then acc else x
fun fold(f, acc, xs) =
    case xs of
      x::xs_ => fold(f, f(acc, x), xs_)
    | [] => acc

fun my_max xs = fold(bigger, ~987654321, xs)

val nums_list = [[9, 40, 75, 7],
                 [64, 34, 88, 96],
                 [91, 92, 53, 31],
                 [50, 84, 73, 65],
                 [54, 44, 75, 11],
                 [91, 71, 48, 46],
                 [70, 72, 5, 42],
                 [25, 77, 49, 56],
                 [89, 4, 73, 52],
                 [36, 56, 61, 1]]

fun local_max xs = map(my_max, xs)
fun global_max xs = fold(bigger, ~987654321, local_max xs)
fun count_multiples xs = 
    map(List.length, map((fn x => filter(mod_2, x)), xs))
fun imc nums_list =
    let
        val multiples = count_multiples nums_list;
        val res = {
            maxIndex = 0,
            curIndex = 0,
            maxVal = hd(multiples)
        };
    in
        fold(fn (res, y) => 
            if (#maxVal res) < y then
                {
                    maxIndex = (#curIndex res),
                    curIndex = (#curIndex res) + 1,
                    maxVal = y
                }
            else
                {
                    maxIndex = (#maxIndex res),
                    curIndex = (#curIndex res) + 1,
                    maxVal = (#maxVal res)
                }
        , res, multiples)
    end

fun ifall(test, xs) =
    fold((fn (acc, x) => acc andalso x), true, map(test, xs))

fun isPositive xs = ifall((fn x => x>0), xs)

fun ifany (test, xs) =
    fold((fn (acc, x) => acc orelse x), true, map(test, xs))

fun ifany2(f, xs) =
    not (null (filter(f, xs)))

fun ifall2(f, xs) =
    List.length xs = List.length (filter(f, xs))