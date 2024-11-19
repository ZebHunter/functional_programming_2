# Лабораторная работа 2. OCaml

- Вариант: AvlSet

- Автор: Рогачев Михаил Сергеевич P34082

- Цель: освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing), а также разделением интерфейса и особенностей реализации.

## Условие задания

В рамках лабораторной работы вам предлагается реализовать одну из предложенных классических структур данных (список, дерево, бинарное дерево, hashmap, граф...).
Требования:

Функции:

 - добавление и удаление элементов;
 - фильтрация;
 - отображение (map);
 - свертки (левая и правая);
 - структура должна быть моноидом.


1) Структуры данных должны быть неизменяемыми.
2) Библиотека должна быть протестирована в рамках unit testing.
3) Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
4) Структура должна быть полиморфной.

## Реализация

### Код

Для имплементации AvlSet был описан module type Set:
``` Ocaml
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
```
Структура дерева
``` Ocaml
module AvlSet (Ord : sig
    type t

    val compare : t -> t -> int
  end) : Set with type elt = Ord.t = struct
  type elt = Ord.t

  type avltree =
    | Empty
    | Node of int * avltree * elt * avltree

  type t = avltree
```

Добавление элемента в AvlSet:
``` Ocaml
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
```

Удаление элемента из множества:
``` Ocaml
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
```

### Тесты

### Unit-тестирование
В рамках unit-тестирования были протестированы функции API

Пример

### Property-based тестирование
В рамках pbt были протестированы свойства моноида и соответсвие Set

Тест ассоциативности:
``` Ocaml
let test_associativity =
  Test.make ~name:"associativity of union" ~count:1000 (triple arb_avl_set arb_avl_set arb_avl_set) (fun (t1, t2, t3) ->
    let union1 = IntAvlSet.concate t1 (IntAvlSet.concate t2 t3) in
    let union2 = IntAvlSet.concate (IntAvlSet.concate t1 t2) t3 in
    IntAvlSet.to_list union1 = IntAvlSet.to_list union2)
;;
```

Тест нейтрального элемента:
``` Ocaml
let test_neutral_element =
  Test.make ~name:"neutral element of union" ~count:1000 arb_avl_set (fun tree ->
    let union_with_empty = IntAvlSet.concate tree IntAvlSet.empty in
    let union_empty_with = IntAvlSet.concate IntAvlSet.empty tree in
    let eq1 = IntAvlSet.to_list tree = IntAvlSet.to_list union_with_empty in
    let eq2 = IntAvlSet.to_list tree = IntAvlSet.to_list union_empty_with in
    eq1 && eq2)
;;
```

Проверка вставки одинаковых элементов:
``` Ocaml
let test_twice_inserting =
  Test.make ~name:"Immuting equals element of insert" ~count:1000 (pair small_int arb_avl_set) (fun (x, set) ->
    let set_with_x = IntAvlSet.insert x set in
    let set_with_x_twice = IntAvlSet.insert x set_with_x in
    IntAvlSet.to_list set_with_x = IntAvlSet.to_list set_with_x_twice)
;;
```


