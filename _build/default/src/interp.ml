open Geo

(* Code de la Section 4 du projet. *)

type instruction =
  Move of transformation
| Repeat of int * program
| Either of program * program 
and program = instruction list

let rec is_deterministic (prog : program) : bool =
  match prog with
  | [] -> true  
  | Move _ :: reste -> is_deterministic reste  
  | Repeat (_, subprog) :: reste -> is_deterministic subprog && is_deterministic reste  
  | Either _ :: _ -> false  

let unfold_repeat (prog : program) : program =
  failwith "À compléter"

let run_det (prog : program) (p : point) : point list =
  failwith "À compléter"

let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  failwith "À compléter"
  
let run (prog : program) (p : point) : point list =
  failwith "À compléter"

let all_choices (prog : program) : program list =
  failwith "À compléter"

let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  failwith "À compléter"
