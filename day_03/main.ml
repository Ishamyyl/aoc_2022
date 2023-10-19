(**
    Rucksack Reorganization

    Container of 2 boxes.
    Types of objs go in either one or the other box.
    Excatly one obj type in the container is errant, which broke this either-or rule and are in both boxes.
    Obj types are any single char.
    Each obj type has a priority that matches it's alphabetical order, a-z|A-Z == 1-26|27-52
    Input is in the form of `(.{0 : len / 2})(.{len / 2 : len})`
      where $1 is the obj types in the first box and $2 is the types in the second.
    Each sequence of 3 containers has exactly one common type between them, called a badge.

    Part 1) Given the input, what is the sum of the priorities of the errant types?
    Part 2) Given the input, what is the sum of the priorities of the badges?
**)

module CharSet = Set.Make (Char)

let fold_lines_1 ic ~init ~f =
  let rec loop acc =
    match In_channel.input_line ic with
    | None -> acc
    | Some line -> loop (f acc line)
  in
  loop init

let fold_lines_2 ic ~init ~f =
  let rec loop acc three_lines =
    match In_channel.input_line ic with
    | None -> acc
    | Some line ->
        if List.length three_lines == 2 then
          loop (f acc (line :: three_lines)) []
        else loop acc (line :: three_lines)
  in
  loop init []

let parse_line fn acc line =
  let a = String.sub line 0 (String.length line / 2) in
  let b = String.sub line (String.length line / 2) (String.length line / 2) in
  acc
  + fn
      (a |> String.to_seq |> CharSet.of_seq)
      (b |> String.to_seq |> CharSet.of_seq)

let parse_three_lines fn acc three_lines =
  acc
  + fn (List.map (fun l -> l |> String.to_seq |> CharSet.of_seq) three_lines)

let uppercase_offset = Char.code 'A' - 1
let lowercase_offset = Char.code 'a' - 1

let _print_charset s =
  CharSet.iter print_char s;
  print_newline ()

let priority_of_char c =
  let code = Char.code c in
  if code > lowercase_offset then code - lowercase_offset
  else code - uppercase_offset + 26

let badge_priority three_lines =
  let inter =
    List.fold_left CharSet.inter (List.hd three_lines) (List.tl three_lines)
  in
  let c = List.nth (CharSet.elements inter) 0 in
  priority_of_char c

let shared_type_priority a b =
  let inter = CharSet.inter a b in
  let c = List.nth (CharSet.elements inter) 0 in
  priority_of_char c

let () =
  In_channel.with_open_text "input.txt" (fun ic ->
      print_endline @@ string_of_int
      @@ fold_lines_1 ic ~init:0 ~f:(parse_line shared_type_priority));
  In_channel.with_open_text "input.txt" (fun ic ->
      print_endline @@ string_of_int
      @@ fold_lines_2 ic ~init:0 ~f:(parse_three_lines badge_priority));
  ()
