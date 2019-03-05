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

fun parse SIGMA REGEX =
  let
    val (NDFA, TOKS) = parse_expression SIGMA (explode REGEX)
  in
    NDFA
  end
and parse_expression SIGMA E =
  let
    val (left, ltoks) = parse_union SIGMA E
  in
    if ltoks = [] orelse (hd ltoks) = #")" orelse (hd ltoks) = #"|" then
      (left, ltoks)
    else
      let
        val (right, rtoks) = parse_expression SIGMA ltoks
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
        val (right, rtoks) = parse_union SIGMA rtoks
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
    val (e, toks) = parse_expression SIGMA XS
    val (nt, rtoks) = next_token toks
  in
    if nt <> #")" then
      raise Fail "Missing parens"
    else
      (e, rtoks)
  end
  | parse_base SIGMA TOKS =
  let
    val (syms, rest) = take_while (fn (x) => member x SIGMA) TOKS
    val start_state = new_state ()
    val end_state = new_state ();
    val new_states = start_state::(map (fn s => new_state ()) (tl syms)) @ [end_state]
    val utrans = pair (new_states @ [end_state])
    val trans = map (fn ((d, cd), t) => ((d, t), cd)) (ListPair.zip (utrans, syms))
    val fsm =  (
      SIGMA,
      new_states,
      start_state,
      (
        trans,
        []
      ),
      [end_state]
      )
  in
    if syms <> [] then
      (fsm, rest)
    else
      raise Fail "Symbol not in alphabet."
  end
and take_while F [] = ([], [])
  | take_while F (X::XS) =
  let
    val (beg, rest) = take_while F XS
  in
    if F X then
      (X::beg, rest)
    else
      ([], X::XS)
  end
and pair [] = []
  | pair [X] = []
  | pair (X1::X2::XS) = (X1, X2)::pair (X2::XS);
