(* NDFA to implement (aba|aaa|abb)* *)
val FSM = (
  [#"a", #"b"],
  ["", "aba:a", "aaa:a", "abb:a", "aba:ab", "aaa:aa", "abb:ab"],
  [""],
  [
    (("", #"a"), "aba:a"),
    (("", #"a"), "aaa:a"),
    (("", #"a"), "abb:a"),
    (("aba:a", #"b"), "aba:ab"),
    (("aaa:a", #"a"), "aaa:aa"),
    (("abb:a", #"b"), "abb:ab"),
    (("aba:ab", #"a"), ""),
    (("aaa:aa", #"a"), ""),
    (("abb:ab", #"b"), "")
  ],
  [""]
);
fun member E [] = false
  | member E (X::XS) = if E = X then true else member E XS;

fun subset A [] = false
  | subset [] B = true
  | subset (X::XS) B = member X B andalso subset XS B;

fun union [] = []
  | union (X::XS) = X @ union XS;

fun evaluate F X =
  let
    val second = (fn (a, b) => b)
    val p = (fn (d, cd) => d = X)
    val s = map second (List.filter p F);
  in
    map second (List.filter p F)
  end;

fun states (SIGMA, S, S0, DELTA, F) = S0;
fun accepting_states (SIGMA, S, S0, DELTA, F) = F;

fun step t (SIGMA, S, S0, DELTA, F) =
  let
    val n = next_states t S0 (SIGMA, S, S0, DELTA, F)
  in
    (SIGMA, S, n, DELTA, F)
  end
and next_states t s (SIGMA, S, S0, DELTA, F) =
  if not (member t SIGMA) then
    raise Fail "Not a valid symbol."
  else if not (subset s S) then
    raise Fail "Not a valid set of states."
   else
     let
       val p = (fn (d, cd) => member d F)
       val potential_transitions = map (fn s => (s, t)) s
       val transitions = List.filter p potential_transitions
     in
       union (map (fn (s) => evaluate DELTA (s, t)) s)
     end;
