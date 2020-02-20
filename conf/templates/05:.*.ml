open Batteries
open BatPrintf

let scan fmt f =
  BatScanf.sscanf (read_line ()) fmt f

let scan_list cnv =
  read_line ()
  |> BatString.split_on_char ' '
  |> List.map cnv

let rec zip xs ys =
  match xs, ys with
  | [], _ -> []
  | _, [] -> []
  | x::xs, y::ys ->
    (x, y) :: zip xs ys

let () =
  $0
