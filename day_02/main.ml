(**
    RPS
    Rock Paper Scissors, game between 2 players, game has many rounds. Each round is scored, summed for a total score:
    - Rock: +1, Paper: +2, Scissors: +3
    - Win: +6
    Highest total score wins the game

    Rock beats Scissors
    Scissors beats Paper
    Paper beats Rock
    Same is a draw

    Input is in the form `(\d) (\d)`
    where $1 is your opponent's play (one of A|B|C) and $2 is your play (one of X|Y|Z), each standing for Rock|Paper|Scissors

    A beats Y
    B beats Z
    C beats X

    Part 1) What is *your* expected total score, given the input?
    Part 2) Input $2 is actually what the result of the round should be! One of X|Y|Z, standing for Lose|Draw|Win.
            With the same input, what is *your* new expected total score?
**)

let fold_lines ic ~init ~f =
  let rec loop acc =
    match In_channel.input_line ic with
    | None -> acc
    | Some line -> loop (f acc line)
  in
  loop init

type play = Rock | Paper | Scissors
type outcome = Win | Lose | Draw

let win_lose_score = function
  | Rock, Scissors | Scissors, Paper | Paper, Rock -> 0
  | Rock, Rock | Scissors, Scissors | Paper, Paper -> 3
  | _ -> 6

let play_score = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let _calc_score_1 a b =
  let opp =
    match a with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith "unknown part 1 opp strategy"
  in
  let me =
    match b with
    | "X" -> Rock
    | "Y" -> Paper
    | "Z" -> Scissors
    | _ -> failwith "unknown me strategy"
  in
  win_lose_score (opp, me) + play_score me

let calc_score_2 a b =
  let opp =
    match a with
    | "A" -> Rock
    | "B" -> Paper
    | "C" -> Scissors
    | _ -> failwith "unknown part 2 opp strategy"
  in
  let out =
    match b with
    | "X" -> Lose
    | "Y" -> Draw
    | "Z" -> Win
    | _ -> failwith "unknown outcome strategy"
  in
  let me =
    match (opp, out) with
    | Rock, Draw | Paper, Lose | Scissors, Win -> Rock
    | Rock, Win | Paper, Draw | Scissors, Lose -> Paper
    | Rock, Lose | Paper, Win | Scissors, Draw -> Scissors
  in
  win_lose_score (opp, me) + play_score me

let parse_line fn acc line =
  match String.split_on_char ' ' line with [ a; b ] -> acc + fn a b | _ -> 0

let () =
  (* In_channel.with_open_text "input.txt" (fun ic ->
      print_endline @@ string_of_int
      (* partial application of `calc_score` onto `parse_line` *)
      @@ fold_lines ic ~init:0 ~f:(parse_line calc_score_1)); *)
  In_channel.with_open_text "input.txt" (fun ic ->
      print_endline @@ string_of_int
      (* partial application of `calc_score` onto `parse_line` *)
      @@ fold_lines ic ~init:0 ~f:(parse_line calc_score_2));
  ()
