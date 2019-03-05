val ndfa = (
  ["a", "b"],
  ["q0", "q1", "q2", "q3"],
  "q0",
  (
  [(("q0", "a"), "q1"),
  (("q1", "a"), "q3"),
  (("q1", "b"), "q2"),
  (("q2", "a"), "q1"),
  (("q2", "a"), "q0"),
  (("q2", "b"), "q2"),
  (("q3", "a"), "q3"),
  (("q3", "b"), "q1")
  ],
  []
  ),
  ["q3"]
  );

use "relations.sml";
fun alphabet (SIGMA, S, S0, DELTA, F) = SIGMA;
fun states (SIGMA, S, S0, DELTA, F) = S;
fun start_state (SIGMA, S, S0, DELTA, F) = S0;
fun transitions (SIGMA, S, S0, DELTA, F) = DELTA;
fun empty_transitions (SIGMA, S, S0, (DELTA, EDELTA), F) = EDELTA;
fun symbol_transitions (SIGMA, S, S0, (DELTA, EDELTA), F) = DELTA
fun accepting_states (SIGMA, S, S0, DELTA, F) = F;

fun evaluate F X =
  let
    fun second (a, b) = b
    fun p (d, cd) = d = X
    val s = map second (List.filter p F);
  in
    map second (List.filter p F)
  end;

fun next_states t s (SIGMA, S, S0, (DELTA, EDELTA), F) =
  if not (member t SIGMA) then
    raise Fail "Not a valid symbol."
  else if not (subset s S) then
    raise Fail "Not a valid set of states."
   else
     let
       (* transitive closure of relations of empty transitions *)
       val edelta = transitive_closure EDELTA
       (* optionally take first empty string transition *)
       val fstates = union s (unionl (map (evaluate edelta) s))
       (* take character transition *)
       val cstates = unionl (map (fn (s) => evaluate DELTA (s, t)) fstates)
       (* again, optionally take empty string transition *)
       val sstates = union cstates (unionl (map (evaluate edelta) cstates))
     in
       sstates
     end
and unionl L = foldl (fn (a, b) => union a b) [] L;

fun in_language S FSM = intersection (run_ndfa [start_state FSM] S FSM) (accepting_states FSM) <> []
and run_ndfa s [] FSM = s
  | run_ndfa s (X::XS) FSM = run_ndfa (next_states X s FSM) XS FSM;

fun printList L = print (String.concat [(String.concatWith ", " L), "\n"]);

fun to_dfa FSM = to_dfa_accumulator [[start_state FSM]] [[start_state FSM]] [] FSM
and to_dfa_accumulator new seen delta FSM =
  let
    val trans = product new (alphabet FSM)
    val next_states = map (fn (s, t) => next_states t s FSM) trans
    val ndelta = ListPair.zip (trans, next_states)
    val nseen = difference next_states seen
  in
    if nseen = [] then
      (alphabet FSM,
      seen,
      [start_state FSM],
      delta,
      (List.filter (fn s => intersection s (accepting_states FSM) <> []) seen))
    else
      to_dfa_accumulator nseen (nseen @ seen) (ndelta @ delta) FSM
  end;
