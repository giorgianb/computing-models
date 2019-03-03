use "sets.sml";
fun implies x y = not x orelse y;
fun unique [] = []
  | unique (X::XS) = if member X XS then unique XS else X::unique XS;

fun domain R = map (fn (a, b) => a) R;
fun codomain R = map (fn (a, b) => b) R;
fun set R = union (domain R) (codomain R);

fun relation (a, b) R = member (a, b) R;

fun compose R1 R2 =
  let
    val p = (fn (a, b, c) => relation (a, b) R1 andalso relation (b, c) R2)
    val tp = product3 (domain R1) (domain R2) (codomain R2)
    val c = map (fn (a, b, c) => (a, c)) (List.filter p tp)
  in
    unique c
  end
and product3 L1 L2 L3 = map (fn ((a, b), c) => (a, b, c)) (product (product L1 L2) L3);

fun reflective R =
  let
    val p = (fn (a, b) => relation (a, a) R andalso relation (b, b) R)
  in
    List.all p R
  end;

fun symmetric R =
  let
    val p = (fn (a, b) => relation (b, a) R)
  in
    List.all p R
  end;

fun transitive R =
  let
    val p = (fn (a, b) => List.all (fn c => implies (relation (b, c) R) (relation (a, c) R)) (set R))
  in
    List.all p R
  end;

fun reflective_closure R = union R (map (fn (a) => (a, a)) (set R));
fun symmetric_closure R = union R (map (fn (a, b) => (b, a)) R);
fun transitive_closure R = 
  let 
    val nelems = length (set R)
    val n = ceil (Math.ln (Real.fromInt nelems) / Math.ln 2.0)
  in
    repeat n (fn (e) => union e (compose e e)) R
  end
and repeat 1 f x = f x
  | repeat n f x = repeat (n - 1) f (f x);

