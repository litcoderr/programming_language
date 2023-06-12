signature Animal =
sig
    val id: unit -> int
end

structure Dog :> Animal =
struct
(* Unbounded. cannot be called from outside*)
    fun add(a:int, b:int) = a+b;
(* bounded. *)
    fun id() = add(1,0);
end