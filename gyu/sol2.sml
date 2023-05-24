datatype expr = NUM of int
               | PLUS of expr * expr
               | MINUS of expr * expr
datatype formula = TRUE
               | FALSE
               | NOT of formula
               | ANDALSO of formula * formula
               | ORELSE of formula * formula
               | IMPLY of formula * formula
               | LESS of expr * expr
               (* LESS(a, b) is true if a < b *)
               
fun eval (f : formula) =
let fun eval_expr(e : expr) = 
   case e of 
   NUM e => e
   | PLUS (e1, e2) => eval_expr(e1) + eval_expr(e2)
   | MINUS (e1, e2) => eval_expr(e1) - eval_expr(e2)
in
   case f of
   TRUE => true
   | FALSE => false
   | NOT(f1) => not (eval(f1))
   | ANDALSO(f1, f2) => eval(f1) andalso eval(f2)
   | ORELSE(f1, f2) => eval(f1) orelse eval(f2)
   | IMPLY(f1, f2) => if(eval(f1)) then eval(f2) else true
   | LESS(e1, e2) => eval_expr(e1) < eval_expr(e2)
end;

type name = string
datatype metro = STATION of name
                | AREA of name * metro
                | CONNECT of metro * metro

fun checkMetro(m : metro) =
let fun StationNotInArea(a : metro) =
    case a of
    STATION name => [name]
    | AREA(name, metro) =>
    let val (withoutName, withName) = List.partition (fn x => x <> name) (StationNotInArea(metro)) in withoutName end
    | CONNECT(metro1, metro2) => StationNotInArea(metro1) @ StationNotInArea(metro2)

in
case m of
    AREA(name, metro) =>
    let val strList = StationNotInArea(metro)
    val (withoutName, withName) = List.partition (fn x => x <> name) strList
    in
    if null strList
    then true 
    else null withoutName
    end
end;

datatype 'a lazyList = nullList
| cons of 'a * (unit -> 'a lazyList)

fun seq(first : int, last : int) =
if(first > last)
then nullList
else cons(first, fn() => seq(first+1, last));

fun infSeq(first : int) =
cons(first, fn() => infSeq(first+1));

fun firstN(lazyListVal, n : int) =
case lazyListVal of
nullList => []
| cons(i, f) => if(n = 0) then [] else i::firstN(f(), n-1);

fun Nth(lazyListVal, n : int) =
case lazyListVal of
nullList => NONE
| cons(i, f) => if(n = 1) then SOME i else Nth(f(), n-1);

fun filterMultiples(lazyListVal, n : int) =
case lazyListVal of
nullList => nullList
| cons(i, f) => if((n <> 0) andalso (i mod n = 0)) then filterMultiples(f(), n) else cons(i, fn() => filterMultiples(f(), n));

fun primes() =
let fun sieve(lazyListVal) =
    case lazyListVal of
    nullList => nullList
    | cons(i, f) => cons(i, fn() => sieve(filterMultiples(f(), i)));

in sieve(infSeq(2))
end;