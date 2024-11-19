module type Set = sig
  type elt
  type t

  val empty : t
  val insert : elt -> t -> t
  val find : elt -> t -> elt option
  val delete : elt -> t -> t
  val map : (elt -> elt) -> t -> t
  val fold_left : ('b -> elt -> 'b) -> 'b -> t -> 'b
  val fold_right : (elt -> 'b -> 'b) -> t -> 'b -> 'b
  val filter : (elt -> bool) -> t -> t
  val concate : t -> t -> t
  val to_list : t -> elt list
end

module AvlSet (Ord : sig
    type t

    val compare : t -> t -> int
  end) : Set with type elt = Ord.t = struct
  type elt = Ord.t

  type avltree =
    | Empty
    | Node of int * avltree * elt * avltree

  type t = avltree

  let empty = Empty

  let height = function
    | Empty -> 0
    | Node (h, _, _, _) -> h
  ;;

  let create l v r =
    let h = 1 + max (height l) (height r) in
    Node (h, l, v, r)
  ;;

  let rotate_left = function
    | Node (_, l, v, Node (_, rl, rv, rr)) ->
      let nl = create l v rl in
      create nl rv rr
    | node -> node
  ;;

  let rotate_right = function
    | Node (_, Node (_, ll, lv, lr), v, r) ->
      let nr = create lr v r in
      create ll lv nr
    | node -> node
  ;;

  let double_rotate_left = function
    | Node (_, l, v, r) ->
      let nr = rotate_right r in
      let nnode = create l v nr in
      rotate_left nnode
    | node -> node
  ;;

  let double_rotate_right = function
    | Node (_, l, v, r) ->
      let nl = rotate_left l in
      let nnode = create nl v r in
      rotate_right nnode
    | node -> node
  ;;

  let balance_factor = function
    | Empty -> 0
    | Node (_, l, _, r) -> height l - height r
  ;;

  let balance = function
    | Node (_, _, _, r) as node when balance_factor node <= -2 ->
      if balance_factor r <= -1 then rotate_left node else double_rotate_left node
    | Node (_, l, _, _) as node when balance_factor node >= 2 ->
      if balance_factor l >= 1 then rotate_right node else double_rotate_right node
    | node -> node
  ;;

  let rec insert v = function
    | Empty -> Node (1, Empty, v, Empty)
    | Node (_, l, x, r) as node ->
      if Ord.compare v x = 0
      then node
      else if Ord.compare v x < 0
      then (
        let l' = insert v l in
        create l' x r |> balance)
      else (
        let r' = insert v r in
        create l x r' |> balance)
  ;;

  let rec find v = function
    | Empty -> None
    | Node (_, l, x, r) -> if Ord.compare v x = 0 then Some x else if Ord.compare v x < 0 then find v l else find v r
  ;;

  let rec find_min = function
    | Empty -> raise Not_found
    | Node (_, Empty, v, _) -> v
    | Node (_, l, _, _) -> find_min l
  ;;

  let rec remove_min = function
    | Empty -> Empty
    | Node (_, Empty, _, r) -> r
    | Node (_, l, v, r) -> create (remove_min l) v r |> balance
  ;;

  let rec delete v = function
    | Empty -> Empty
    | Node (_, l, x, r) ->
      if Ord.compare v x < 0
      then create (delete v l) x r |> balance
      else if Ord.compare v x > 0
      then create l x (delete v r) |> balance
      else (
        match l, r with
        | Empty, _ -> r
        | _, Empty -> l
        | _ ->
          let min_val = find_min r in
          create l min_val (remove_min r) |> balance)
  ;;

  let rec map f = function
    | Empty -> Empty
    | Node (_, l, v, r) ->
      let lm = map f l in
      let rm = map f r in
      create lm (f v) rm
  ;;

  let rec fold_left f acc = function
    | Empty -> acc
    | Node (_, l, v, r) ->
      let acc = fold_left f acc l in
      let acc = f acc v in
      fold_left f acc r
  ;;

  let rec fold_right f tree acc =
    match tree with
    | Empty -> acc
    | Node (_, l, v, r) ->
      let acc = fold_right f r acc in
      let acc = f v acc in
      fold_right f l acc
  ;;

  let rec concate t1 t2 =
    match t1, t2 with
    | Empty, t | t, Empty -> t
    | Node (_, l, v, r), _ ->
      let left_union = concate l t2 in
      let right_union = concate r left_union in
      insert v right_union
  ;;

  let rec filter pred = function
    | Empty -> Empty
    | Node (_, l, v, r) ->
      let l_filter = filter pred l in
      let r_filter = filter pred r in
      if pred v then create l_filter v r_filter |> balance else concate l_filter r_filter |> balance
  ;;

  let rec to_list = function
    | Empty -> []
    | Node (_, l, v, r) -> to_list l @ [ v ] @ to_list r
  ;;
end

module type Map = sig
  type elt1
  type elt2
  type t1
  type t2

  val map : (elt1 -> elt2) -> t1 -> t2
end

module MakeMap (Ord1 : Set) (Ord2 : Set) :
  Map with type elt1 = Ord1.elt and type elt2 = Ord2.elt and type t1 = Ord1.t and type t2 = Ord2.t = struct
  type elt1 = Ord1.elt
  type elt2 = Ord2.elt
  type t1 = Ord1.t
  type t2 = Ord2.t

  let map f tree = Ord1.fold_left (fun acc x -> Ord2.insert (f x) acc) Ord2.empty tree
end
