(* let option = None | Some of 'a *)

let expect_option err = function
  | None -> failwith err
  | Some x -> x

let unwrap_option_or default = function
  | None -> default
  | Some x -> x

let unwrap_option = expect_option "??"

let result_of_option err = function
  | Some x -> Ok x
  | None -> Error err

(* let result = Error of 'e | Ok of 'a *)

let expect_result err = function
  | Error e -> failwith (e ^ ": " ^ err)
  | Ok x -> x

let unwrap_result_or default = function
  | Error _ -> default
  | Ok x -> x

let unwrap_result = expect_result "??"

let option_of_err_result = function
  | Error e -> Some e
  | Ok x -> None

let option_of_result = function
  | Error e -> None
  | Ok x -> Some x
