val EMPTY = #"\000";
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

fun member E [] = false
  | member E (X::XS) = if E = X then true else member E XS;

fun alphabet (SIGMA, S, S0, DELTA, F) = SIGMA;
fun states (SIGMA, S, S0, DELTA, F) = S;
fun start_state (SIGMA, S, S0, DELTA, F) = S0;
fun transitions (SIGMA, S, S0, DELTA, F) = DELTA;
fun accepting_states (SIGMA, S, S0, DELTA, F) = F;

fun printList L = print (String.concat [(String.concatWith ", " (map Char.toString L)), "\n"]);

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

val SIGMA = [#"a", #"b"];
val (f, n) = parse SIGMA (explode "(a|(ab))*");
