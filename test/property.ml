open QCheck
open Avlset
module IntAvlSet = AvlSet (Int)

let gen_avl_set =
  let open Gen in
  small_list int >|= List.fold_left (fun acc x -> IntAvlSet.insert x acc) IntAvlSet.empty
;;

let arb_avl_set = make gen_avl_set

let test_associativity =
  Test.make ~name:"associativity of union" ~count:1000 (triple arb_avl_set arb_avl_set arb_avl_set) (fun (t1, t2, t3) ->
    let union1 = IntAvlSet.concate t1 (IntAvlSet.concate t2 t3) in
    let union2 = IntAvlSet.concate (IntAvlSet.concate t1 t2) t3 in
    IntAvlSet.to_list union1 = IntAvlSet.to_list union2)
;;

let test_neutral_element =
  Test.make ~name:"neutral element of union" ~count:1000 arb_avl_set (fun tree ->
    let union_with_empty = IntAvlSet.concate tree IntAvlSet.empty in
    let union_empty_with = IntAvlSet.concate IntAvlSet.empty tree in
    let eq1 = IntAvlSet.to_list tree = IntAvlSet.to_list union_with_empty in
    let eq2 = IntAvlSet.to_list tree = IntAvlSet.to_list union_empty_with in
    eq1 && eq2)
;;

let test_twice_inserting =
  Test.make ~name:"Immuting equals element of insert" ~count:1000 (pair small_int arb_avl_set) (fun (x, set) ->
    let set_with_x = IntAvlSet.insert x set in
    let set_with_x_twice = IntAvlSet.insert x set_with_x in
    IntAvlSet.to_list set_with_x = IntAvlSet.to_list set_with_x_twice)
;;

let tests = [ test_associativity; test_neutral_element; test_twice_inserting ]
