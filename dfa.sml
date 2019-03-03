val EOFSM = (
  [#"a", #"b"],
  ["ab", "aB", "Ab", "AB"],
  "ab",
  [
    (("ab", #"a"), "Ab"),
    (("ab", #"b"), "aB"),
    (("aB", #"a"), "AB"),
    (("aB", #"b"), "ab"),
    (("Ab", #"a"), "ab"),
    (("Ab", #"b"), "AB"),
    (("AB", #"a"), "aB"),
    (("AB", #"b"), "Ab")
  ],
  ["ab"]
);

fun member E [] = false
  | member E (X::XS) = if E = X then true else member E XS;

fun evaluate F X =
  let
    fun second (a, b) = b
    fun p (d, cd) = d = X
  in
    hd (map second (List.filter p F))
  end;

fun state (SIGMA, S, S0, DELTA, F) = S0;
fun accepting_states (SIGMA, S, S0, DELTA, F) = F;

fun step t (SIGMA, S, S0, DELTA, F) =
  let
    val n = next_state t S0 (SIGMA, S, S0, DELTA, F)
  in
    (SIGMA, S, n, DELTA, F)
  end
and next_state t s (SIGMA, S, S0, DELTA, F) =
  if not (member t SIGMA) then
    raise Fail "Not a valid symbol."
  else if not (member s S) then
    raise Fail "Not a valid state."
   else
     evaluate DELTA (s, t);

fun in_language [] FSM = member (state FSM) (accepting_states FSM)
  | in_language (X::XS) FSM = in_language XS (step X FSM);

