open Geo
open Interp

(* Code de la Section 5 du projet. *)

let sample (rect : rectangle) : point =
  {
    x = Random.float (rect.x_max -. rect.x_min) +. rect.x_min;
    y = Random.float (rect.y_max -. rect.y_min) +. rect.y_min
  }

let transform_rect (t : transformation) (r : rectangle) : rectangle =

  let corner_rec = corners r 
  in let transform_corner = List.map (transform t ) corner_rec
  in rectangle_of_list transform_corner 
  (* match t with
  | Translate _ -> rectangle_of_list transform_corner 
  | Rotate (point, angle) -> rectangle_of_list transform_corner *)
      
                      
let rec run_rect (prog : program) (r : rectangle) : rectangle list =
  List.fold_left                                        
  (fun rect_liste instr ->                       
    let current_rect = List.hd (List.rev rect_liste) in
      rect_liste @ execute_prog current_rect instr)       
    [r]                                                 
    ( unfold_repeat prog )                             
  
  and execute_prog (r : rectangle) (i : instruction) : rectangle list = 
    match i with                                                
      | Move t -> [transform_rect t r]                                 (* dans geo.ml *)
      | Repeat (n, sousprog) -> run_rect (repeat_to_flat n sousprog ) r    
      | Either (p1, p2) ->                                          
        let choose_prog = if Random.bool () then p1 else p2 in
      run_rect (choose_prog) r 
        
      (* A REVOIR CAR DUPLICATION DE LA FONCTION DANS CE FICHIER ET INTERP *)
      and repeat_to_flat (n : int) (sousprog : program) : program =   
      let flat_sousprog = unfold_repeat sousprog in                
        List.init n (fun _ -> flat_sousprog)                        
        |> List.flatten                                             


let inclusion (r : rectangle) (t : rectangle) : bool =
  if (r.x_min >= t.x_min && r.x_max <= t.x_max && r.y_min >= t.y_min && r.y_max <= t.y_max) then true
  else if (r.x_min <= t.x_min && r.x_max >= t.x_max && r.y_min <= t.y_min && r.y_max >= t.y_max) then true
  else false

let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"
  (* let list_prog = all_choices prog in
  let rec verif_rect rect = 
    match rect with
    | [] -> true
    | last_rect :: _ -> inclusion last_rect target *)

let run_polymorphe (transform : transformation -> 'a -> 'a) (prog : program) (i : 'a) : 'a list =
  failwith "À compléter"

let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  failwith "À compléter"

let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"
