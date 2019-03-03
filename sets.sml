fun member E [] = false
  | member E (X::XS) = if E = X then true else member E XS;

fun union L [] = L
  | union [] L = L
  | union (X::XS) L = if member X L  then union XS L  else X::union XS L;

fun intersection L [] = []
  | intersection [] L = []
  | intersection (X::XS) L = 
  if member X L then 
    X::intersection XS L 
  else
    intersection XS L;

fun subset [] B = true
  | subset A [] = false
  | subset (X::XS) B = if member X B then subset XS B else false;

fun equal L1 L2 = subset L1 L2  andalso subset L2 L1;

fun minus L [] = L
  | minus [] L = []
  | minus (X::XS) L = if member X L then minus XS L else X::minus XS L;

fun product L [] = []
  | product [] L = []
  | product (X::XS) L = single_product X L @ product XS L
and single_product E [] = []
  | single_product E (X::XS) = (E, X)::single_product E XS;

fun powerset L = powerset_accumulator [] L
and powerset_accumulator S [] = [S]
  | powerset_accumulator S (X::XS) = powerset_accumulator (X::S) XS @ powerset_accumulator S XS;
