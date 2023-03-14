use "sol1.sml";

(* Question 1 Test*)
val merge_result = merge([1,4,5], [2,6,7]);

(*Question 2 Test*)
val reverse_result = reverse([1,5,4]);
val reverse_result = reverse([1,5]);
val reverse_result = reverse([1]);
val reverse_result = reverse([]);

(* Question 3 Test*)
fun sample_func(a: int): int =
    a
val pi_result = pi(1, 3, sample_func);

(* Question 4 Test*)
val digits_result = digits(10);

(* Question 5 Test*)
val ap = additivePersistence(12349);
val root = digitalRoot(12349);