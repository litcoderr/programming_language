(* Question 1 *)
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

fun eval (f: formula): bool =
    let fun value(e: expr): int =
        case e of
        NUM(x) => x
        | PLUS(x, y) => value(x) + value(y)
        | MINUS(x, y) => value(x) - value(y)
    in case f of
    TRUE => true
    | FALSE => false
    | NOT(x) => not (eval(x))
    | ANDALSO(x, y) => eval(x) andalso eval(y)
    | ORELSE(x, y) => eval(x) orelse eval(y)
    | IMPLY(x, y) => not (eval(x)) orelse eval(y)
    | LESS(x, y) => value(x) < value(y)
    end

(* Question 2 *)
type name = string
datatype metro = STATION of name
                    | AREA of name * metro
                    | CONNECT of metro * metro

fun checkMetro (m: metro): bool =
    let
        fun isinlist(s: string, s_list: string list): bool =
            case s_list of
            [] => false
            | temp::sub_list => (s = temp) orelse isinlist(s, sub_list);

        fun helper(m: metro, area_names: string list): bool =
            case m of
            AREA(area_name, m) => helper(m, area_name::area_names)
            | CONNECT(m1, m2) => helper(m1, area_names) andalso helper(m2, area_names)
            | STATION(station_name) => isinlist(station_name, area_names);
    in
        helper(m, [])
    end

(* Question 3 *)
datatype 'a lazyList = nullList
                        | cons of 'a * (unit -> 'a lazyList)

fun makeLazy(l: 'a list): 'a lazyList =
    case l of
    [] => nullList
    | temp::temp_l => cons(temp, fn()=>makeLazy(temp_l))

fun seq(first: int, last: int): int lazyList =
    (* inclusive *)
    let fun seq_helper(first: int, last: int, list: int lazyList): int lazyList =
            if first > last then list
            else cons(first, fn()=>seq_helper(first+1, last, list));
    in
        seq_helper(first, last, nullList)
    end

fun infSeq(first: int): int lazyList =
    cons(first, fn()=>infSeq(first+1))

fun firstN(lazyListVal: 'a lazyList, n: int): 'a list =
    let fun helper(list: 'a lazyList, n: int, res: 'a list): 'a list =
        if n <= 0 then res
        else
            case list of
            nullList => res
            | cons(x, f) => x::helper(f(), n-1, res);
    in
        helper(lazyListVal, n, [])
    end

fun Nth(lazyListVal: 'a lazyList, n: int): 'a option =
    case lazyListVal of
    nullList => NONE
    | cons(x, f) =>
        if n = 0 then NONE
        else if n = 1 then SOME x
        else Nth(f(), n-1)

fun filterMultiples(lazyListVal: int lazyList, n: int): int lazyList =
    case lazyListVal of
    nullList => nullList
    | cons(x, f) =>
        if (Int.mod(x, n) = 0) then filterMultiples(f(), n)
        else cons(x, fn()=>filterMultiples(f(), n))

