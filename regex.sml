use "ndfa.sml";

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

fun parse SIGMA E =
  let
    val (left, ltoks) = parse_union SIGMA E
  in
    if ltoks = [] orelse (hd ltoks) = #")" orelse (hd ltoks) = #"|" then
      (left, ltoks)
    else
      let
        val (right, rtoks) = parse SIGMA ltoks
        val ntrans = map (fn (s) => (s, start_state right)) (accepting_states left)
        val fsm = (
          SIGMA,
          (states left) @ (states right),
          start_state left,
          (
            symbol_transitions left @ symbol_transitions right,
            ntrans @ empty_transitions left @ empty_transitions right
          ),
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
        val ntrans = [(start, start_state left), (start, start_state right)] 
        val fsm = (
          SIGMA,
          (start::states left) @ states right,
          start,
          (
            symbol_transitions left @ symbol_transitions right,
            ntrans @ empty_transitions left @ empty_transitions right
          ),
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
        val ntrans = map (fn (s) => (s, start_state left)) (accepting_states left)
        val fsm = (
          SIGMA,
          start::states left,
          start,
          (
            symbol_transitions left,
            (start, start_state left)::ntrans @ empty_transitions left
          ),
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
      (
        [((start_state, X), end_state)],
        []
      ),
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
