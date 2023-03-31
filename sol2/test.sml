use "sol2.sml";
exception AssertionFailure of string;

fun assert(condition: bool) =
    if not condition then
        raise AssertionFailure("Invalid result") else ();

(* Question 1 Test*)
assert(eval(LESS(NUM(3), PLUS(NUM(2), NUM(2)))) = true);
assert(eval(LESS(NUM(3), MINUS(NUM(2), NUM(2)))) = false);
assert(eval(NOT(FALSE)) = true);
assert(eval(NOT(TRUE)) = false);
assert(eval(ANDALSO(TRUE, FALSE)) = false);
assert(eval(ANDALSO(TRUE, TRUE)) = true);
assert(eval(ORELSE(TRUE, FALSE)) = true);
assert(eval(IMPLY(TRUE, FALSE)) = false);
assert(eval(IMPLY(TRUE, TRUE)) = true);
assert(eval(IMPLY(FALSE, FALSE)) = true);
assert(eval(IMPLY(FALSE, TRUE)) = true);

(* Question 2 Test*)
assert(checkMetro(AREA("a", STATION "a")) = true);
assert(checkMetro(AREA("a", AREA("b", STATION("a")))) = true);
assert(checkMetro(AREA("a", AREA("a", STATION("a")))) = true);
assert(checkMetro(AREA("a", AREA("b", CONNECT(STATION "a", STATION "b")))) = true);
assert(checkMetro(AREA("a", AREA("c", CONNECT(STATION "a", STATION "b")))) = false);
assert(checkMetro(CONNECT(STATION "a", STATION "b")) = false);
assert(checkMetro(STATION "a") = false);

(* Question 3 Test*)
val lazy_list = makeLazy([1, 2, 3, 4, 5]);
val lazy_list = infSeq(3);
val lazy_list = seq(3, 5);
val l = firstN(lazy_list, 2);
val l = firstN(lazy_list, 5);
val x = Nth(lazy_list, 0);
val x = Nth(lazy_list, 1);
val x = Nth(lazy_list, 3);
val x = Nth(lazy_list, 4);


(* Question 4 Test*)
val filtered = filterMultiples(seq(3, 9), 3);
val filtered = firstN(filtered, 100);