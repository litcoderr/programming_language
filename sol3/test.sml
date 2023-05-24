use "sol3.sml";

val p = TupleP([Variable("hello"), Variable("hello2"), 
        ConstructorP("hello3", TupleP([
            UnitP, ConstP(2), Wildcard, Variable("sup")
        ]))
]);
val result = check_pat(p);

val p = TupleP([Variable("a"), UnitP, Wildcard, ConstructorP("b", Variable("c"))]);
val v = Tuple([Const(1), Unit, Const(2), Constructor("b", Unit)]);
val result = match(v, p);

val t = MATCH(PLAYER("s", ref s), MATCH(PLAYER("rp", ref rp), PLAYER("r", ref r)));
val res = whosWinner(t);
