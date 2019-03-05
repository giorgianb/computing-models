fun quicksort [] = []
  | quicksort [X] = [X]
  | quicksort (X::XS) =
  let
    val (left, right) = List.partition (fn e => e < X) XS;
  in
    quicksort left @ [X] @ quicksort right
  end;

fun mergesort [] = []
  | mergesort [X] = [X]
  | mergesort L =
  let
    val left = take L
    val right = skip L
  in
    merge (mergesort left) (mergesort right)
  end
and take [] = []
  | take [X] = [X]
  | take (X1::X2::XS) = X1::take XS
and skip [] = []
  | skip [X] = []
  | skip (X1::X2::XS) = X2::skip XS
and merge [] [] = []
  | merge L1 [] = L1
  | merge [] L2 = L2
  | merge (X1::X1S) (X2::X2S) =
  if X1 < X2 then
    X1::merge X1S (X2::X2S)
  else
    X2::merge (X1::X1S) X2S;
