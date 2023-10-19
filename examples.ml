(* implementing operators *)
(* Option.map *)
let ( >>| ) opt f =
  match opt with
  | None -> None
  | Some x -> Some (f x)

(* Option.bind *)
let ( >>= ) opt f =
  match opt with
  | None -> None
  | Some x -> f x

(* polymorphism, applying sigs (i.e. interfaces) to modules *)
module type Fact = sig
  (* [fact n] is [n] factorial *)
  val fact : int -> int
end

module FactImpl : Fact = struct
  let rec fact n =
    if n = 0 then 1 else
      n * fact (n - 1)
end

module TailFact : Fact = struct
  let rec fact' acc n =
    if n = 0 then acc else
      fact' (n * acc) (n - 1)

  let fact n = fact' 1 n
end

(**
    Collections
**)

(* Collection: Binary Tree*)
module BTree = struct
  type 'a btree =
  | Leaf
  | Node of 'a * 'a btree * 'a btree

  let rec map f = function
  | Leaf -> Leaf
  | Node (v, l, r) -> Node (f v, map f l, map f r)

  let rec fold acc f = function
  | Leaf -> acc
  | Node (v, l, r) -> f v (fold acc f l) (fold acc f r)
end

module type StackSig = sig
  type 'a t
  val empty : 'a t
  val push : 'a -> 'a t -> 'a t
  val peek : 'a t -> 'a
  val pop : 'a t -> 'a t
end

(* Collection: Stack *)
module MyStack = struct
  type 'a stack = 'a list

  let empty = []

  let peek = function
  | [] -> failwith "Empty"
  | x :: _ -> x

  let push x stk = x :: stk

  let pop = function
  | [] -> failwith "Empty"
  | _ :: s -> s
end

(* Collection: Single-linked Queue *)
module MyQueue = struct
  type 'a queue = 'a list
  let empty = []

  let peek = function
  | [] -> None
  | x :: _ -> Some x

  let enqueue x q = q @ x

  let dequeue = function
  | [] -> None
  | _ :: q -> Some q
end

(* Collection: Deque implementation, allows constant time enqueues *)
module TwoListQueue = struct
  (* [{front = [a; b]; back = [e; d; c]}]
     represents the queue a,b,c,d,e

     If [front] is empty, then [back] must also be empty,
     to guarantee that the first element of the queue is always the head of [front].
  *)
  type 'a deque = {
    front : 'a list;
    back : 'a list;
  }
  let empty = {front = []; back = [];}

  let peek = function
  | {front = []} -> None
  | {front = x :: _} -> Some x

  let enqueue x = function
  | {front = []} -> {front = [x]; back = []}
  | q -> {q with back = x :: q.back}

  let dequeue = function
  | {front = []} -> None
  | {front = _ :: []; back} -> Some {front = List.rev back; back = []}
  | {front = _ :: t; back} -> Some {front = t; back}
end

(*  Collection: Set
    with "data abstraction" *)
module type Set = sig
  type 'a t
  val empty : 'a t
  (* val size : 'a t -> int *)
  val insert : 'a -> 'a t -> 'a t
  val mem : 'a -> 'a t -> bool
  (* val union : 'a t -> 'a t -> 'a t
  val string : ('a -> string) -> 'a t -> string *)
end

let interior string_of_elt h t =
  t
  |> List.map string_of_elt
  |> List.fold_left (fun acc elt -> acc ^ ", " ^ elt) (string_of_elt h)

let string_of_list string_of_elt = function
| [] -> "{}"
| h :: t -> "{" ^ interior string_of_elt h t ^ "}"

let dedupe lst = List.sort_uniq Stdlib.compare lst

module ListSetNoDupes : Set = struct
  (* duplicates are handled during [add] and [union] so that the backing structure
     actually doesn't have duplicates in it, making [size] simple and true.
     [add] => O(n) | [union] => O(n*m) | [size] => O(1) | [mem] => O(n) *)
  type 'a t = 'a list
  let empty = []
  let size = List.length
  let mem = List.mem
  let insert x s = if mem x s then s else x :: s
  let union s1 s2 = s1 @ s2 |> dedupe
  let string s = s |> string_of_list
end

module ListSetDupes : Set = struct
  (* duplicates are handled during [size] and [union], this means the backing structure
     may actually have duplicates in it.
     [insert] => O(1) | [union] => O(1) | [size] => O(n) | [mem] => O(n) *)
  type 'a t = 'a list
  let empty = []
  let size s = s |> dedupe |> List.length
  let mem = List.mem
  let insert = List.cons
  let union = List.append
  let string f s = s |> dedupe |> (string_of_list f)
end

module BstSet : Set = struct
  type 'a t = Leaf | Node of 'a t * 'a * 'a t

  let empty = Leaf

  let rec mem x = function (* O(n) i.e. not O(log n) due to potential worst-case of an inbalanaced tree *)
  | Leaf -> false
  | Node (l, v, r) ->
    if x < v then mem x l
    else if x > v then mem x r
    else true

  let rec insert x = function (* O(n) i.e. not O(log n) due to potential worst-case of an inbalanaced tree *)
  | Leaf -> Node (Leaf, x, Leaf)
  | Node (l, v, r) as n ->
    if x < v then insert x l
    else if x > v then Node (l, v, insert x r)
    else if x < v then Node (insert x l, v, r)
    else n
end

module RedBlackSet : Set = struct
  type color = Red | Blk
  type 'a t = Leaf | Node of color * 'a t * 'a * 'a t

  let empty = Leaf

  let rec mem x = function (* O(log n) *)
  | Leaf -> false
  | Node (_, l, v, r) ->
    if x < v then mem x l
    else if x > v then mem x r
    else true

  let balance = function
  | (Blk, Node (Red, Node (Red, a, x, b), y, c), z, d) (* 1 *)
  | (Blk, Node (Red, a, x, Node (Red, b, y, c)), z, d) (* 2 *)
  | (Blk, a, x, Node (Red, Node (Red, b, y, c), z, d)) (* 3 *)
  | (Blk, a, x, Node (Red, b, y, Node (Red, c, z, d))) (* 4 *)
    -> Node (Red, Node (Blk, a, x, b), y, Node (Blk, c, z, d))
  | (c, l, v, r) -> Node (c, l, v, r)

  let rec insert' x = function (* O(log n) *)
  | Leaf -> Node (Red, Leaf, x, Leaf)
  | Node (c, l, v, r) as n ->
    if x < v then balance (c, l, v, insert' x r)
    else if x > v then balance (c, l, v, insert' x r)
    else n

  let rec insert x s = (* O(log n) *)
    match insert' x s with
    | Leaf -> failwith "impossible"
    | Node (_, l, v, r) -> Node (Blk, l, v, r)
end

(*  Collection: Map *)
module type Map = sig
  type ('k, 'v) t
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val find : 'k -> ('k, 'v) t -> 'v option
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
  val empty : ('k, 'v) t
  val of_list : ('k * 'v) list -> ('k, 'v) t
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module AssociationListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list

  let insert k v m = (k, v) :: m (* O(1) *)

  let find = List.assoc_opt (* O(n) *)

  let remove k m = List.filter (fun (k', _) -> k <> k') m (* O(n) *)

  let empty = []

  let of_list lst = lst (* O(1) *)

  let keys m = m |> List.map fst |> List.sort_uniq Stdlib.compare (* O(n log n) *)

  let binding m k = (k, List.assoc k m) (* O(n) *)

  let bindings m = List.map (binding m) (keys m) (* O(n log n) linearithmic + O(n) linear + O(n) linear = O(n^2) quadratic *)
end

module type DirectAddressMap = sig
  type 'v t
  val insert : int -> 'v -> 'v t -> unit
  val find : int -> 'v t -> 'v option
  val remove : int -> 'v t -> unit
  val create : int -> 'v t
  val of_list : int -> (int * 'v) list -> 'v t
  val bindings : 'v t -> (int * 'v) list
end

module ArrayMap : DirectAddressMap = struct
  type 'v t = 'v option array

  let insert k v a = a.(k) <- Some v (* O(1) *)

  let find k a = a.(k) (* O(1) *)

  let remove k a = a.(k) <- None (* O(1) *)

  let create cap = Array.make cap None (* O(cap) *)

  let of_list cap lst = (* O(cap) *)
    let a = create cap in (* O(cap) *)
    List.iter (fun (k, v) -> insert k v a) lst; (* O(n) *)
    a

  let bindings a = (* O(cap) *)
    let b = ref [] in
    for k = 0 to Array.length a do
      match a.(k) with
      | None -> ()
      | Some v -> b := (k, v) :: !b
    done;
    !b
end

module type TableMap = sig
  type ('k, 'v) t

  val insert : 'k -> 'v -> ('k, 'v) t -> unit
  val find : 'k -> ('k, 'v) t -> 'v option
  val remove : 'k -> ('k, 'v) t -> unit
  val create : ('k -> int) -> int -> ('k, 'v) t
end

module HashMap : TableMap = struct
  type ('k, 'v) t = {
    hash : 'k -> int;
    mutable size : int;
    mutable buckets : ('k * 'v) list array;
  }

  let capacity tab = Array.length tab.buckets (* O(1) *)

  let index k tab = tab.hash k mod (capacity tab) (* O(1) *)

  let insert_no_resize k v tab = (* O(L) *)
    let b = index k tab in
    let old = tab.buckets.(b) in
    tab.buckets.(b) <- (k, v) :: List.remove_assoc k old;
    if not (List.mem_assoc k old) then
      tab.size <- tab.size + 1;
    ()

  let load_factor tab =
    float_of_int tab.size /. float_of_int (capacity tab)

  let rehash tab new_cap = (* O(n:# of bindings) *)
    let rehash_binding (k, v) =
      insert_no_resize k v tab
    in
    let rehash_bucket b =
      List.iter rehash_binding b
    in
    let old = tab.buckets in
    tab.buckets <- Array.make new_cap [];
    tab.size <- 0;
    Array.iter rehash_bucket old

  let resize tab =
    let lf = load_factor tab in
    if lf > 2.0 then
      rehash tab (capacity tab * 2)
    else if lf < 0.5 then
      rehash tab (capacity tab / 2)
    else ()

  let insert k v tab = (* O(n) *)
    insert_no_resize k v tab;
    resize tab

  let find k tab =
    List.assoc_opt k tab.buckets.(index k tab)

  let remove_no_resize k tab = (* O(L) *)
    let b = index k tab in
    let old = tab.buckets.(b) in
    tab.buckets.(b) <- List.remove_assoc k tab.buckets.(b);
    if List.mem_assoc k old then
      tab.size <- tab.size - 1;
    ()

  let remove k tab = (* O(n) *)
    remove_no_resize k tab;
    resize tab

  let create hash cap = { (* O(cap) *)
    hash;
    size = 0;
    buckets = Array.make cap [];
  }
end

(**
    Functors
**)

module type X = sig
  val x : int
end

module A = struct
  (* as long as A's shape matches X, A can be used in a functor that applies X, e.g. `module B = IncrementX(A)` *)
  let x = 0
end

module IncrementX = functor (M : X) -> struct
  let x = M.x + 1
end

(* syntactic sugar for the above, kinda like `function` *)
module IncrementX' (M : X) = struct
  let x = M.x + 1
end

(* using functors *)
type day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

let int_of_day = function
| Mon -> 1
| Tue -> 2
| Wed -> 3
| Thu -> 4
| Fri -> 5
| Sat -> 6
| Sun -> 7

module DayKey = struct
  type t = day
  let compare day1 day2 = int_of_day day1 - int_of_day day2
end

module DayMap = Map.Make(DayKey)

let m =
  let open DayMap in
  empty
  |> add Mon "Monday"
  |> add Tue "Tuesday"
  |> add Wed "Wednesday"
  |> add Thu "Thursday"
  |> add Fri "Friday"
  |> add Sat "Saturday"
  |> add Sun "Sunday"
