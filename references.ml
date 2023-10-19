(* references, pointers to typed locations in memory *)
let y = ref 1;;
y;; (* int ref *)
!y;; (* int 1 *)
y := 2;;
!y;; (* int 2 *)

(* aliasing *)
let x = ref 2;;
let z = x;;
x := 3;;
!z;; (* int 3 *)

(* equality *)
let r1 = ref 'a';;
let r2 = ref 'a';;
r1 == r2;; (* false, different pointers, physical equality, `!=` is inverse *)
r1 = r2;; (* true, same contents, structural equality, `<>` is inverse *)

(**
  Using references
**)

let next () =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter
;;

(* mutating records *)
type point = {x : int; y : int; mutable c : string}

let p = {x = 0; y = 0; c = "red"};;
p.c <- "white";;

(* mutable single-linked list *)
type 'a node = {
  value : 'a;
  mutable next : 'a node option;
}

type 'a mlist = {
  mutable first : 'a node option;
}

let create_node v = {
  next = None;
  value = v;
}

let empty () = {
  first = None
}

let singleton v = {
  first = Some (create_node v)
}

let insert_first lst v =
  match lst.first with
  | None -> lst.first <- Some (create_node v)
  | old_first ->
    let new_first = create_node v in
    new_first.next <- old_first;
    lst.first <- Some new_first
;;

(**
  Arrays
**)
let a = [|1; 2; 3|];;
a.(1);; (* int 2 *)
a.(2) <- 4;;
a.(0);; (* int 5 *)

(* vectors *)
type vec = float array

let print_vec v =
  for i = 0 to Array.length v - 1 do
    print_float v.(i); print_newline ()
  done

let print_vec' v =
  let print_elt n =
    print_float n; print_newline ()
  in
  Array.iter print_elt v

let print_vec'' = Array.iter (Printf.printf "%F\n")

let vec_add v1 v2 =
  let len1, len2 = Array.length v1, Array.length v2 in
  let v3 = Array.make len1 0. in
  for i = 0 to len1 - 1 do
    v3.(i) <- v1.(i) +. v2.(i)
  done;
  v3

let vec_add' v1 v2 =
  let len1, len2 = Array.length v1, Array.length v2 in
  let elt i = v1.(i) +. v2.(i) in
  Array.init len1 elt

let vec_add'' = Array.map2 ( +. )