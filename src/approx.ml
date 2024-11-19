open Geo
open Interp

(* Code de la Section 5 du projet. *)

let sample (rect : rectangle) : point =
  (* failwith "À compléter" *)
  {
    x = Random.float (rect.x_max -. rect.x_min) +. rect.x_min;
    y = Random.float (rect.y_max -. rect.y_min) +. rect.y_min
  }
let transform_rect (t : transformation) (r : rectangle) : rectangle =
  failwith "À compléter"


let run_rect (prog : program) (r : rectangle) : rectangle list =
  failwith "À compléter"

let inclusion (r : rectangle) (t : rectangle) : bool =
  (* failwith "À compléter" *)
  if (r.x_min >= t.x_min && r.x_max <= t.x_max && r.y_min >= t.y_min && r.y_max <= t.y_max) then true
  else if (r.x_min <= t.x_min && r.x_max >= t.x_max && r.y_min <= t.y_min && r.y_max >= t.y_max) then true
  else false

let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"

let run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
  failwith "À compléter"

let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  failwith "À compléter"

let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"
