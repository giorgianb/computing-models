signature TREE = sig
  type tree
  type element
  val empty : tree
  val insert : tree -> element -> tree
  val insertAll : tree -> element list -> tree
  val contains : tree -> element -> bool
  val containsAll : tree -> element list -> bool
  val visit : tree -> (element -> unit) -> unit
end

signature ORDER = sig
  type element
  val compare : element * element -> order
end

functor BinarySearchTree (O : ORDER) : TREE =
  struct 
    datatype tree = empty | node of O.element * tree * tree
    type element = O.element
    val empty = empty;
    fun insert empty elem = node (elem, empty, empty)
      | insert (node (e, left, right)) elem =
      let
        val c = O.compare (elem, e)
      in
        if c = LESS then 
          node (e, insert left elem, right)
        else if c = EQUAL then
          node (e, left, right)
         else
           node (e, left, insert right elem)
      end;
    fun insertAll tree [] = tree
      | insertAll tree (X::XS) = insertAll (insert tree X) XS;
    fun contains empty elem = false
      | contains (node (e, left, right)) elem =
      let
        val c = O.compare (elem, e)
      in
        if c = LESS then
          contains left elem
        else if c = EQUAL then
          true
        else
          contains right elem
      end;
    fun containsAll tree [] = true
      | containsAll tree (X::XS) = contains tree X andalso containsAll tree XS;
    fun visit empty f = ()
      | visit (node (e, left, right)) f = (visit left f; f e; visit right f);
  end;

structure IntCmp = struct
  type element = int
  val compare = Int.compare
end;

structure RealCmp = struct
  type element = real
  val compare = Real.compare
end

structure IntBST = BinarySearchTree (IntCmp);
structure RealBST = BinarySearchTree (RealCmp);
val nextInt = Random.randRange (~1000, 1000);
val r = Random.rand (1, 1);
val ints = List.tabulate (100, fn x => nextInt r);
val tree = IntBST.insertAll IntBST.empty ints;
fun printInt x = print (String.concat [Int.toString x, " "]);
IntBST.visit tree printInt;
