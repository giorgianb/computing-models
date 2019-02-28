import Data.List (nub)

implies x y = not x || y

member :: (Eq a) => a -> [a] -> Bool
member e [] = False;
member e (x:xs)
    | e == x = True
    | otherwise = member e xs

union :: (Eq a) => [a] -> [a] -> [a]
union l [] = l
union [] l = l
union (x:xs) l
    | member x l = union xs l
    | otherwise = x:union xs l

intersection :: (Eq a) => [a] -> [a] -> [a]
intersection l [] = []
intersection [] l = []
intersection (x:xs) l
    | member x l = x:intersection xs l
    | otherwise = intersection xs l

subset :: (Eq a) => [a] -> [a] -> Bool
subset [] l = True
subset l [] = False
subset (x:xs) l
    | member x l = subset xs l
    | otherwise = False

equal :: (Eq a) => [a] -> [a] -> Bool
equal s1 s2 = subset s1 s2 && subset s2 s1

minus :: (Eq a) => [a] -> [a] -> [a]
minus l [] = l
minus [] l = []
minus (x:xs) l 
    | member x l = minus xs l
    | otherwise = x:minus xs l

cproduct :: [a] -> [b] -> [(a, b)]
cproduct l [] = []
cproduct [] l = []
cproduct (x:xs) l = single_product x l ++ cproduct xs l
    where
        single_product e [] = []
        single_product e (x:xs) = (e, x):single_product e xs

cproduct3 :: (Eq a, Eq b, Eq c) => [a] -> [b] -> [c] -> [(a, b, c)]
cproduct3 r1 r2 r3 = nub . map (\(a, (b, c)) -> (a, b, c)) . cproduct r1 . cproduct r2 $ r3

powerset :: [a] -> [[a]]
powerset l = powerset_accumulator [] l
    where
        powerset_accumulator s [] = [s]
        powerset_accumulator s (x:xs) = powerset_accumulator (x:s) xs ++ powerset_accumulator s xs

relation :: (Eq a, Eq b) => (a, b) -> [(a, b)] -> Bool
relation = member

domain :: [(a, b)] -> [a]
domain = map fst

codomain :: [(a, b)] -> [b]
codomain = map snd

set l = union (domain l) (codomain l)

compose :: (Eq a, Eq b, Eq c) => [(a, b)] -> [(b, c)] -> [(a, c)]
compose r1 r2 = map (\(a, b, c) -> (a, c)) 
    $ filter p $ cproduct3 (domain r1) (domain r2) (codomain r2)
    where p = (\(a, b, c) -> relation (a, b) r1 && relation (b, c) r2)

reflective :: (Eq a) => [(a, a)] -> Bool
reflective l = all p l
    where p = (\(a, b) -> relation (a, a) l && relation (b, b) l)

symmetric :: (Eq a) => [(a, a)] -> Bool
symmetric l = all p l
    where p = (\(a, b) -> relation (b, a) l)

transitive :: (Eq a) => [(a, a)] -> Bool
transitive l = all p l
    where p = (\(a, b) -> all (\c -> relation (b, c) l `implies` relation (a, c) l) $ set l)

reflective_closure :: (Eq a) => [(a, a)] -> [(a, a)]
reflective_closure l = union l . map (\a -> (a, a)) $ set l

symmetric_closure :: (Eq a) => [(a, a)] -> [(a, a)]
symmetric_closure l = union l . map (\(a, b) -> (b, a)) $ l

transitive_closure :: (Eq a) => [(a, a)] -> [(a, a)]
transitive_closure l = iterate (\e -> union e . compose e $ e) l !! (length . set $ l)
