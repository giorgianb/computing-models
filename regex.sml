use "relations.sml";

fun printList L = print (String.concat ["[", (String.concatWith ", " (map
  Int.toString L)), "]", "\n"]);
(* NDFA Implementation *)
val EMPTY = #"\000";
fun alphabet (SIGMA, S, S0, DELTA, F) = SIGMA;
fun states (SIGMA, S, S0, DELTA, F) = S;
fun start_state (SIGMA, S, S0, DELTA, F) = S0;
fun transitions (SIGMA, S, S0, DELTA, F) = DELTA;
fun accepting_states (SIGMA, S, S0, DELTA, F) = F;

fun evaluate F X =
  let
    val second = (fn (a, b) => b)
    val p = (fn (d, cd) => d = X)
    val s = map second (List.filter p F);
  in
    map second (List.filter p F)
  end;

fun next_states t s (SIGMA, S, S0, DELTA, F) =
  if not (member t SIGMA) then
    raise Fail "Not a valid symbol."
  else if not (subset s S) then
    raise Fail "Not a valid set of states."
   else
     let
       (* empty transitions *)
       val et = List.filter (fn ((d, t), cd) => t = EMPTY) DELTA
       (* transitive closure of relations of empty transitions *)
       val tet = transitive_closure (map (fn ((d, t), cd) => (d, cd)) et)
       val delta = union DELTA (map (fn (d, cd) => ((d, EMPTY), cd)) tet)
       (* optionally take first empty string transition *)
       val fstates = union s (unionl (map (fn (s) => evaluate delta (s, EMPTY)) s))
       (* take character transition *)
       val cstates = unionl (map (fn (s) => evaluate delta (s, t)) fstates)
       (* again, optionally take empty string transition *)
       val sstates = union cstates (unionl (map (fn (s) => evaluate delta (s, EMPTY)) cstates))
     in
       sstates
     end
and unionl L = foldl (fn (a, b) => union a b) [] L;

fun in_language S FSM = intersection (run_ndfa [start_state FSM] S FSM) (accepting_states FSM) <> []
and run_ndfa s [] FSM = s
  | run_ndfa s (X::XS) FSM = run_ndfa (next_states X s FSM) XS FSM;

 (* Parser *)
fun next_token [] = (EMPTY, [])
  | next_token (X::XS) = (X, XS)

val cstate = ref 0;
fun new_state () =
  let
    val nstate = !cstate
  in
    cstate := nstate + 1;
    nstate
  end;

fun parse SIGMA E =
  let
    val (left, ltoks) = parse_union SIGMA E
  in
    if ltoks = [] orelse (hd ltoks) = #")" orelse (hd ltoks) = #"|" then
      (left, ltoks)
    else
      let
        val (right, rtoks) = parse SIGMA ltoks
        val ntrans = map (fn (s) => ((s, EMPTY), start_state right)) (accepting_states left)
        val fsm = (
          SIGMA,
          (states left) @ (states right),
          start_state left,
          ntrans @ transitions left @ transitions right,
          accepting_states right
        )
      in
        (fsm, rtoks)
    end
  end
and parse_union SIGMA E =
  let
    val (left, ltoks) = parse_klein SIGMA E
    val (nt, rtoks) = next_token ltoks
  in
    if nt = #"|" then
      let
        val (right, rtoks) = parse_klein SIGMA rtoks
        val start = new_state ()
        val ntrans = [((start, EMPTY), start_state left), ((start, EMPTY), start_state right)] 
        val fsm = (
          SIGMA,
          (start::states left) @ states right,
          start,
          ntrans @ transitions left @ transitions right,
          accepting_states left @ accepting_states right
          )
      in
        (fsm, rtoks)
      end
    else
      (left, ltoks)
  end
and parse_klein SIGMA E =
  let
    val (left, ltoks) = parse_base SIGMA E
    val (nt, rtoks) = next_token ltoks
  in
    if nt = #"*" then
      let
        val start = new_state ()
        val ntrans = map (fn (s) => ((s, EMPTY), start_state left)) (accepting_states left)
        val fsm = (
          SIGMA,
          start::states left,
          start,
          [((start, EMPTY), start_state left)] @ ntrans @ transitions left,
          start::accepting_states left
          )
      in
        (fsm, rtoks)
      end
    else
      (left, ltoks)
  end
and parse_base SIGMA (#"("::XS) = 
  let
    val (e, toks) = parse SIGMA XS
    val (nt, rtoks) = next_token toks
  in
    if nt <> #")" then
      raise Fail "Missing parens"
    else
      (e, rtoks)
  end
  | parse_base SIGMA (X::XS) =
  let
    val start_state = new_state ()
    val end_state = new_state ()
    val fsm =  (
      SIGMA,
      [start_state, end_state],
      start_state,
      [
        ((start_state, X), end_state)
      ],
      [end_state]
      )
  in
    if member X SIGMA then
      (fsm, XS)
    else 
      raise Fail "Character not in alphabet"
  end;

val SIGMA = (explode "abcdefghijklmnopqrstuvwxyz");
val (f, n) = parse SIGMA (explode "(a|(ab))*");
