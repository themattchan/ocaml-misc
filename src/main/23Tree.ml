module Tree = sig
  module type TREE =
    functor ( Key : OrderedType ) ->
    sig
      type 'v tree
      val empty  : 'v tree
      val insert : Key.t -> 'v -> 'v tree -> 'v tree
      val lookup : Key.t -> 'v tree -> 'v option
      val delete : Key.t -> 'v tree -> ('v * 'v tree) option
    end

  (* Example: AVL Trees *)

  module AVL ( Key : OrderedType ) : TREE =
    struct

      type 'v tree = Empty
                   | Node of (Key.t * 'v) * ('v tree) * ('v tree)

      let empty = Empty

      let rec height = function
        | Empty = 0
        | Node (_, l, r) -> 1 + max (height l) (height r)

      let rec balanceFactor = function
        | Empty = 0
        | Node (_, l, r) -> height l - height r

      let rec isBal = function
        | Empty = true
        | Node (_, l, r) -> isBal l && isBal r && abs (height l - height r) <= 2

      let rotateL t = match t with
        | Empty -> Empty
        | Node (kv, l, r) ->
           match r with
           | Empty -> t
           | Node (kv1, l1, r1) -> Node (kv1, Node (kv, l, l1), r1)

      let rotateR t = match t with
        | Empty -> Empty
        | Node (kv, l, r) ->
           match l with
           | Empty -> t
           | Node (kv1, l1, r1) -> Node (kv1, l1, Node (kv, r1, r))

      let balance t = match t with
        | Empty -> Empty
        | Node (kv, l, r) ->
           let n = balanceFactor t in
           (* left-right *)
           if n > 1 && (balanceFactor l) < 0 then
             rotateR (Node (kv, rotateL l, r))
                     (* right-left *)
           else if n < -1 && (balanceFactor r) > 0 then
             rotateL (Node (kv, l, rotateR r))
                     (* left-left *)
           else if n > 1 then
             rotateR t
                     (* right-right *)
           else if n < -1 then
             rotateL t
           else
             t

      let insert k v t =
        let rec insert1 k v t = match t with
          | Empty -> Node ((k,v), Empty, Empty)
          | Node ((k1,v1), l,r) ->
             if k < k1 then Node ((k1,v1), insert1 k v l, r)
             else if k > k1 then Node ((k1,v1), l, insert1 k v r)
             else t
        in
        balance (insert1 k v t)

      let rec lookup k = function
        | Empty -> None
        | Node ((k1,v1), l,r) ->
           if k < k1 then lookup k l
           else if k > k1 then lookup k r
           else Some v1

      let rec deleteMin = function
        | Empty -> None
        | Node (kv, Empty, r) ->
           Some (kv, r)
        | Node (kv, l, r) ->
           match deleteMin l with
           | Some (ret, l') -> Some (ret, Node (kv, l', r))
           | _ -> error "impossible"

      let rec delete k = function
        | Empty -> None
        | Node ((k1,v1), l, r) ->
           if k < k1 then match delete k l with
                          | None -> None
                          | Some (ret, l') -> Some (ret, Node ((k1,v1), l', r))
           else if k > k1 then match delete k r with
                               | None -> None
                               | Some (ret, r') -> Some (ret, Node ((k1,v1), l, r'))
           else match l,r with
                | Empty, _ -> r
                | _, Empty -> l
                | _, _     ->
                   match deleteMin r with
                   | None          -> Some ((k1,v1), Node (kv, l, Empty))
                   | Some (kv, r') -> Some ((k1,v1), Node (kv, l, r'))

    end

  (* Example: 2-3 Trees

  A really cool data structure: the structure of the tree itself guarantees balance!

  These generalise to 2-3 *finger* trees, which are extremely efficient
  functional data structures that are widely used as backing structures for
  other tree types.

  http://www.staff.city.ac.uk/~ross/papers/FingerTree.html

   *)
  module TwoThree ( Key : OrderedType ) : TREE =
    struct

      type 'v tree =
        Empty
      (* 2-Nodes store one datum and have 2 subtrees L R
       * INVARIANT: forall kL in L, k > kL /\ forall kR in R, k < kR
       *            L R have equal height
       *)
      | Two of (Key.t * 'v) *
                 ('v tree) * ('v tree)
      (* 3-Nodes store two datums and have 3 subtrees L M R
       * INVARIANT: k1 < k2 /\
       *            forall kL in L, k1 > kL /\ forall kM in M, k1 < kM /\
       *            forall kM in M, k2 > kM /\ forall kR in R, k2 < kR
       *            L M R have equal height
       *)
      | Three of (Key.t * 'v) * (Key.t * 'v) *
                   ('v tree) * ('v tree) * ('v tree)

      let empty = Empty

      let is_empty = function
        | Empty -> true
        | _     -> false

      let is_two = function
        | Two _ -> true
        | _     -> false

      let is_three = function
        | Three _ -> true
        | _       -> false

      let tree_of_list kvs = List.fold_left (fun t (k,v) -> insert k v t) empty kvs

      let rec insert ki vi t =
        match t with
        | Empty -> Two ((ki, vi), Empty, Empty)

        (* TWO NODES *)
        (* Don't insert *)
        | Two ((k,v), l, r) as t when ki = k -> t
        (* Leaves *)
        | Two ((k,v), Empty, r) as t when ki < k ->
           Three ((ki,vi),(k,v), Empty, Empty, r)
        | Two ((k,v), l, Empty) as t when ki > k ->
           Three ((k,v),(ki,vi),l, Empty, Empty)
        (* Nodes, keep going *)
        | Two ((k,v), l, r) as t ->
           if ki < k then Two ((k,v), insert ki vi l, r)
           else Two ((k,v), l, insert ki vi r)

        (* THREE NODES *)
        (* Don't insert *)
        | Three ((k1,v1), (k2,v2), l, m, r) as t when ki = k1 || ki = k2 -> t
        (* Leaves *)
        | Three ((k1,v1), (k2,v2), Empty, m, r) as t when ki < k1 ->

        | Three ((k1,v1), (k2,v2), l, Empty, r) as t when k1 < ki && ki < k2 ->
        | Three ((k1,v1), (k2,v2), l, m, Empty) as t when k1 > k2 ->

        (* Nodes, keep going *)
        | Three ((k1,v1), (k2,v2), l, m, r) as t  ->
           if ki < k1 then Three ((k1,v1), (k2,v2), insert ki vi l, m,r)
           else if k1 < ki && ki < k2 then Three ((k1,v1), (k2,v2), l, insert ki vi m, r)
           else Three ((k1,v1), (k2,v2), l, m, insert ki vi r)


      let rec lookup x = function
        | Empty -> None

        | Two ((k,v), l, r) ->
           if k = x      then Some v
           else if x > k then lookup x r
           else (* x < k *)   lookup x l

        | Three ((k1,v1), (k2,v2), l, m, r) ->
           if k1 = x      then Some v1
           else if k2 = x then Some v2

           else if x < k1 then lookup x l
           else if k1 < x && x < k2 then lookup x m
           else (* x > k2 *)   lookup r

      let rec delete ki = function
        | Empty -> Empty
        | Two ((k,v), l, r) ->
        | Three ((k1,v1), (k2,v2), l, m, r) ->

    end
end
