open OUnit2
open Avlset
module IntSet = AvlSet (Int)
module StringSet = AvlSet (String)
module IntToString = MakeMap (IntSet) (StringSet)
module StringToInt = MakeMap (StringSet) (IntSet)

let test_int_empty _ = assert_equal [] (IntSet.to_list IntSet.empty)

let test_int_insert _ =
  let tree =
    IntSet.(
      empty |> insert 30 |> insert 10 |> insert 60 |> insert 40 |> insert 50 |> insert 20 |> insert 70 |> insert 80)
  in
  assert_equal [ 10; 20; 30; 40; 50; 60; 70; 80 ] (IntSet.to_list tree)
;;

let test_int_find _ =
  let tree = IntSet.(empty |> insert 3 |> insert 1 |> insert 2) in
  assert_equal (Some 1) (IntSet.find 1 tree);
  assert_equal None (IntSet.find 4 tree)
;;

let test_int_delete _ =
  let tree = IntSet.(empty |> insert 3 |> insert 1 |> insert 2 |> delete 1) in
  assert_equal [ 2; 3 ] (IntSet.to_list tree);
  let tree = IntSet.(tree |> delete 3) in
  assert_equal [ 2 ] (IntSet.to_list tree)
;;

let test_int_map _ =
  let tree = IntSet.(empty |> insert 1 |> insert 2 |> insert 3) in
  let mapped_tree = IntSet.map (( * ) 2) tree in
  assert_equal [ 2; 4; 6 ] (IntSet.to_list mapped_tree)
;;

let test_int_fold_left _ =
  let tree = IntSet.(empty |> insert 1 |> insert 2 |> insert 3) in
  let sum = IntSet.fold_left ( + ) 0 tree in
  assert_equal 6 sum
;;

let test_int_fold_right _ =
  let tree = IntSet.(empty |> insert 1 |> insert 2 |> insert 3) in
  let concat = IntSet.fold_right (fun x acc -> string_of_int x ^ acc) tree "" in
  assert_equal "123" concat
;;

let test_int_filter _ =
  let tree = IntSet.(empty |> insert 1 |> insert 2 |> insert 3 |> insert 4) in
  let filtered_tree = IntSet.filter (fun x -> x mod 2 = 0) tree in
  assert_equal [ 2; 4 ] (IntSet.to_list filtered_tree)
;;

let test_set _ =
  let tree = IntSet.(empty |> insert 1 |> insert 2 |> insert 3 |> insert 3) in
  assert_equal [ 1; 2; 3 ] (IntSet.to_list tree)
;;

(* Тесты для StringSet *)
let test_string_empty _ = assert_equal [] (StringSet.to_list StringSet.empty)

let test_string_insert _ =
  let tree = StringSet.(empty |> insert "c" |> insert "a" |> insert "b") in
  assert_equal [ "a"; "b"; "c" ] (StringSet.to_list tree)
;;

let test_string_find _ =
  let tree = StringSet.(empty |> insert "c" |> insert "a" |> insert "b") in
  assert_equal (Some "a") (StringSet.find "a" tree);
  assert_equal None (StringSet.find "z" tree)
;;

let test_string_delete _ =
  let tree = StringSet.(empty |> insert "c" |> insert "a" |> insert "b" |> delete "a") in
  assert_equal [ "b"; "c" ] (StringSet.to_list tree);
  let tree = StringSet.(tree |> delete "c") in
  assert_equal [ "b" ] (StringSet.to_list tree)
;;

let test_string_map _ =
  let tree = StringSet.(empty |> insert "a" |> insert "b" |> insert "c") in
  let mapped_tree = StringSet.map String.uppercase_ascii tree in
  assert_equal [ "A"; "B"; "C" ] (StringSet.to_list mapped_tree)
;;

let test_string_fold_left _ =
  let tree = StringSet.(empty |> insert "a" |> insert "b" |> insert "c") in
  let concat = StringSet.fold_left ( ^ ) "" tree in
  assert_equal "abc" concat
;;

let test_string_fold_right _ =
  let tree = StringSet.(empty |> insert "a" |> insert "b" |> insert "c") in
  let concat = StringSet.fold_right ( ^ ) tree "" in
  assert_equal "abc" concat
;;

let test_string_filter _ =
  let tree = StringSet.(empty |> insert "apple" |> insert "banana" |> insert "cherry") in
  let filtered_tree = StringSet.filter (fun x -> String.length x > 5) tree in
  assert_equal [ "banana"; "cherry" ] (StringSet.to_list filtered_tree)
;;

let test_map_types _ =
  let int_set = IntSet.(empty |> insert 1 |> insert 2 |> insert 3) in
  let int_to_string x = string_of_int x in
  let string_set = IntToString.map int_to_string int_set in
  let expected_string_set = StringSet.(empty |> insert "1" |> insert "2" |> insert "3") in
  assert_equal (StringSet.to_list expected_string_set) (StringSet.to_list string_set)
;;

let int_suite =
  "IntSet tests"
  >::: [ "test_empty" >:: test_int_empty
       ; "test_insert" >:: test_int_insert
       ; "test_find" >:: test_int_find
       ; "test_delete" >:: test_int_delete
       ; "test_map" >:: test_int_map
       ; "test_fold_left" >:: test_int_fold_left
       ; "test_fold_right" >:: test_int_fold_right
       ; "test_filter" >:: test_int_filter
       ; "test_set" >:: test_set
       ]
;;

let string_suite =
  "StringSet tests"
  >::: [ "test_empty" >:: test_string_empty
       ; "test_insert" >:: test_string_insert
       ; "test_find" >:: test_string_find
       ; "test_delete" >:: test_string_delete
       ; "test_map" >:: test_string_map
       ; "test_fold_left" >:: test_string_fold_left
       ; "test_fold_right" >:: test_string_fold_right
       ; "test_filter" >:: test_string_filter
       ; "test_map_types" >:: test_map_types
       ]
;;

let () =
  let unit_tests = "All tests" >::: [ int_suite; string_suite ] in
  let _ = run_test_tt_main unit_tests in
  QCheck_runner.run_tests_main Property.tests
;;
