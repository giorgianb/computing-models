import Debug.Trace
-- returns the transitive closure of the relation defined by m
make_transitive m
    | length m <= 2 = union (compose m m) m
    | otherwise = cm
	where
	    (aa', ba, ab, bb') = split4 m
	    aa = make_transitive aa'
	    bb = make_transitive bb'
	    -- ab with new valid start points
	    ab_s = union ab $ compose ab aa
	    -- ab_s with new valid end points
	    ab_se = union ab_s $ compose bb ab_s
	    -- ba with new valid start points
	    ba_s = union ba $ compose ba bb
	    -- ba_s with new valid end points
	    ba_se = union ba_s $ compose bb ba_s

	    aa_f = make_transitive . union aa $ compose ba_se ab_se
	    bb_f = make_transitive . union bb $ compose ab_se ba_se
	    ab_sf = union ab . compose ab $ aa_f
	    ab_sef = union ab_sf $ compose bb_f ab_sf
	    ba_sf = union ba $ compose ba bb_f
	    ba_sef = union ba_sf $ compose aa_f ba_sf
	    
	    cm = combine4 (aa_f, ba_sef, ab_sef, bb_f)

make_transitive_slow m = iterate (\e -> union e . compose e $ e) m !! times
	where times = 1 + (ceiling $ logBase (fromIntegral . length $ m) 2)

relations nelems = 
    map (\n -> group nelems . map ($ n) $ c) [0..2^(nelems^2)-1]
    where c = counter (nelems^2)

is_transitive r = r == (union r $ compose r r)

-- given a matrix, it splits it into 4 quadrants:
-- | q1 q2 |
-- | q3 q4 |
-- and returns those 4 quadrants
split4 :: [[a]] -> ([[a]], [[a]], [[a]], [[a]])
split4 l = (
	    take nrows $ map (take ncolumns) l, 
	    take nrows $ map (drop ncolumns) l,
	    drop nrows $ map (take ncolumns) l,
	    drop nrows $ map (drop ncolumns) l
	    )
    where 
	nrows = div (length l) 2
	ncolumns = div (length $ head l) 2

group _ [] = []
group n l
    | n > 0 = (take n l) : (group n (drop n l))
    | otherwise = error "Negative or zero n"

-- given the matrices m1 and m2, returns the matrix [ m1 | m2 ]
merge_left_right m1 m2 = zipWith (++) m1 m2
-- given the matrices m1 and m2, returns the matrix 
-- | m1 |
-- | m2 |
merge_top_bottom m1 m2 = m1 ++ m2
-- given matrices m1, m2, m3, m4 returns:
-- | m1 m2 |
-- | m3 m4 |
-- basically, undoes the result of a split4 operator
combine4 (m1, m2, m3, m4) = merge_top_bottom (merge_left_right m1 m2) (merge_left_right m3 m4)

-- returns the rows of a matrix
rows l = l
-- returns the columns of a matrix
columns l =  map (\n -> map (\r -> r !! n) l) [0..(length $ head l) - 1]

bool_multiply a b = if a == 1 && b == 1 then 1 else 0
bool_add a b = if a == 1 || b == 1 then 1 else 0

-- takes the dot product of two vectors
dot_product v1 v2 = foldl (bool_add) 0 $ zipWith (bool_multiply) v1 v2
-- computes Av, where A is a matrix and v is a vector
matrix_vector_multiply m v = map (\r -> dot_product r v) (rows m)
-- matrix multiplication of m1 and m2
compose m1 m2 = columns (map (\c -> matrix_vector_multiply m1 c) (columns m2))
-- adds two vectors
vector_add v1 v2 = zipWith (bool_add) v1 v2
-- adds to matrices
union m1 m2 = zipWith (vector_add) (rows m1) (rows m2)

make_flipper n = \x -> mod (div x n) 2
counter nbits = map make_flipper . map (2^) $ [0..(nbits-1)]

