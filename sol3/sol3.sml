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

fun check_pat(p: pattern): bool = 
    let
        fun str_exist(s: string, s_list: string list): bool =
            List.exists (fn x => x = s) s_list;

        fun helper(p: pattern, s_list: string list): (bool * string list) =
            case p of
            Variable(s) => (not (str_exist(s, s_list)), s::s_list)
            | ConstructorP(s, p_) => let
                                        val (sub_res, updated) = helper(p_, s::s_list);
                                     in
                                        (((not (str_exist(s, s_list))) andalso sub_res), updated)
                                     end
            | TupleP(p_::[]) => helper(p_, s_list)
            | TupleP(p_::p_list) => let
                                        val (res, first_updated) = helper(p_, s_list);
                                        val (sub_res, final_updated) = helper(TupleP(p_list), first_updated);
                                    in
                                        ((res andalso sub_res), final_updated)
                                    end
            | _ => (true, s_list);
    in
        #1 (helper(p, []))
    end

fun match(v: valu, p: pattern): (string * valu) list option =
    let
        fun length(xs: 'a list): int =
            let
                fun helper(xs: 'a list, acc: int): int =
                    case xs of
                    [] => acc
                    | x::[] => acc+1
                    | x::xs_ => helper(xs_, acc+1);
            in
                helper(xs, 0)
            end
        val is_valid = check_pat(p);
    in
        if not is_valid then NONE
        else
            case p of
            Wildcard => SOME([])
            | Variable(s) => SOME([(s, v)])
            | UnitP => 
                let in 
                    case v of
                    Unit => SOME([])
                    | _ => NONE
                end
            | ConstP(c_p) => 
                let in
                    case v of
                    Const(c_v) => if c_v = c_p then SOME([])
                                else NONE
                    | _ => NONE
                end
            | TupleP(p_list) =>
                let in
                    case v of
                    Tuple(v_list) =>
                    let in
                        if length(p_list) = length(v_list) then
                        let in
                            case p_list of
                            [] => SOME([]) 
                            | p_::[] => match((hd v_list), p_)
                            | p_::p_list_ =>
                            let in
                                case match((hd v_list), p_) of
                                SOME(cur_list) =>
                                let in
                                    case match(Tuple(tl v_list), TupleP(p_list_)) of
                                    SOME(sub_list) => SOME(cur_list@sub_list)
                                    | NONE => NONE
                                end
                                | NONE => NONE
                            end
                        end
                        else NONE
                    end
                    | _ => NONE
                end
            | ConstructorP(s_p, p_) =>
            let in
                case v of
                Constructor(s_v, v_) =>
                let in
                    if s_p = s_v then match(v_, p_)
                    else NONE
                end
                | _ => NONE
            end
    end

type name = string;

datatype RSP =
    ROCK
    | SCISSORS
    | PAPER

datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)

datatype tournament = PLAYER of name * (RSP strategy ref)
                    | MATCH of tournament * tournament

fun next(strategyRef) =
    let val Cons(rsp, func) = !strategyRef in
        strategyRef := func();
        rsp
    end

fun onlyOne(one:RSP) = Cons(one, fn() => onlyOne(one));
fun alterTwo(one:RSP, two:RSP) = Cons(one, fn() => alterTwo(two, one));
fun alterThree(one:RSP, two:RSP, three:RSP) = 
    Cons(one, fn() => alterThree(two, three, one));

val r = onlyOne(ROCK);
val s = onlyOne(SCISSORS);
val p = onlyOne(PAPER);
val rp = alterTwo(ROCK, PAPER);
val sr = alterTwo(SCISSORS, ROCK);
val ps = alterTwo(PAPER, SCISSORS);
val srp = alterThree(SCISSORS, ROCK, PAPER);

fun whosWinner(t: tournament): tournament =
    let
        fun winner(PLAYER(p1, p1_strat), PLAYER(p2, p2_strat)) =
            let
                val p1_hand = next(p1_strat);
                val p2_hand = next(p2_strat);
            in
                case p1_hand of
                ROCK =>
                let in
                    case p2_hand of
                    ROCK => winner(PLAYER(p1, p1_strat), PLAYER(p2, p2_strat))
                    | SCISSORS => PLAYER(p1, p1_strat)
                    | PAPER => PLAYER(p2, p2_strat)
                end
                | SCISSORS =>
                let in
                    case p2_hand of
                    ROCK => PLAYER(p2, p2_strat)
                    | SCISSORS => winner(PLAYER(p1, p1_strat), PLAYER(p2, p2_strat))
                    | PAPER => PLAYER(p1, p1_strat)
                end
                | PAPER =>
                let in
                    case p2_hand of
                    ROCK => PLAYER(p1, p1_strat)
                    | SCISSORS => PLAYER(p2, p2_strat)
                    | PAPER => winner(PLAYER(p1, p1_strat), PLAYER(p2, p2_strat))
                end
            end

        fun helper(t: tournament): tournament =
            case t of
            PLAYER(name, strat) => PLAYER(name, strat)
            | MATCH(PLAYER(p1, p1_strat), PLAYER(p2, p2_strat)) =>
                winner(PLAYER(p1, p1_strat), PLAYER(p2, p2_strat))
            | MATCH(m1, m2) => helper(MATCH(helper(m1), helper(m2)))
    in
        helper(t)
    end