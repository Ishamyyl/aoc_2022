(*  Checks if the input char is a blankspace character via membership of the list.
    Uses `Fun.flip` to turn `List.mem ch lst` into `List.mem lst ch` for partial application / currying. *)
let is_space = Fun.flip List.mem [' '; '\t'; '\n'; '\r'; '\012']

(*  Python's `join()` *)
let rec join sep = function
| [] -> ""
| [t] -> t
| h :: t -> h ^ sep ^ join sep t

(* single-let filter with default argument. this allows caller to provide inital accumulator, which will have weird effects due to `List.rev` *)
(* let rec filter op ?(acc=[]) = function
| [] -> List.rev acc
| h :: t -> filter op ~acc:(if op h then h :: acc else acc) t *)

(* double-let filter with the accumulator hidden as an implementation detail *)
let filter op =
  let rec filter' op acc = function
  | [] -> List.rev acc
  | h :: t -> filter' op (if op h then h :: acc else acc) t
  in filter' op []
