(**
  Given: List of integers, one on each line, forming a sequence relating to a particular unit. Multiple sequences are seperated by an empty line.
  sum each sequence of numbers, and return: part 1) the largest sequence total, part 2) the largets 3 sequence totals
**)

(*
    bubble up the max total of sequences of numbers.
    recursively sum up the sequence line by line.
    at the end of each sequence, which is delimited by "", check if the total is bigger than the current max.
    exit the recursion on [] and return the highest sequence total
*)
(* let biggest_sequence lines =
   let rec biggest_sequence' highest count lines =
     match lines with
     | [] -> highest
     | "" :: rest -> biggest_sequence' (max highest count) 0 rest
     | line :: rest ->
         biggest_sequence' highest (count + int_of_string line) rest
   in
   biggest_sequence' 0 0 lines *)

(* recursively build a list of sum totals of sequences of numbers in a list.
   then sort biggest-to-smallest and return the list
*)
(* let sum_sequences lines =
   let rec sum_sequences' totals total lines =
     match lines with
     | [] -> totals
     | "" :: rest -> sum_sequences' (total :: totals) 0 rest
     | line :: rest -> sum_sequences' totals (total + int_of_string line) rest
   in
   sum_sequences' [] 0 lines
   |> List.sort (fun x y -> -Int.compare x y)
*)

let sum_sequences_iter ic =
  let rec sum_sequences_iter' totals total =
    match In_channel.input_line ic with
    | None -> totals
    | Some "" -> sum_sequences_iter' (total :: totals) 0
    | Some line -> sum_sequences_iter' totals (total + int_of_string line)
  in
  sum_sequences_iter' [] 0 |> List.sort (fun x y -> -Int.compare x y)

(* let read_lines ?(line_ending = '\n') filename =
   In_channel.with_open_text filename In_channel.input_all
   |> String.split_on_char line_ending
*)

let () =
  let sums = In_channel.with_open_text "input.txt" @@ sum_sequences_iter in
  let sums' =
    match sums with
    | [] -> 0
    | [ a ] -> a
    | [ a; b ] -> a + b
    | [ a; b; c ] -> a + b + c
    | a :: b :: c :: _ -> a + b + c
  in
  let r = string_of_int sums' in
  print_endline r

(* let () =
   let sums = sum_sequences @@ read_lines "input.txt" in
   let sums' =
     match sums with
     | [] -> 0
     | [ a ] -> a
     | [ a; b ] -> a + b
     | [ a; b; c ] -> a + b + c
     | a :: b :: c :: _ -> a + b + c
   in
   let r = string_of_int sums' in
   print_endline r *)
