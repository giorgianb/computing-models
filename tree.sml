signature TREE = sig
  type 'a tree
  val empty : 'a tree
  val insert : 'a tree -> (('a * 'a) -> order)-> 'a -> 'a tree
  val visit : 'a tree -> ('a -> unit) -> unit
end

structure OBTree :> TREE = struct
  datatype 'a tree = empty | node of 'a * 'a tree * 'a tree
  val empty = empty;
  fun insert empty compare elem = node (elem, empty, empty)
    | insert (node (e, left, right)) compare elem =
      if compare (elem, e) = LESS then 
        node (e, insert left compare elem, right)
      else if compare (elem, e) = EQUAL then
        node (e, left, right)
      else
        node (e, left, insert right compare elem);
  fun visit empty f = ()
    | visit (node (e, left, right)) f = (visit left f; f e; visit right f);
end
