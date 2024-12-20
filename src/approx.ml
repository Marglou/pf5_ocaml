open Geo
open Interp

(* Code de la Section 5 du projet. *)

let sample (rect : rectangle) : point =
  {
    x = Random.float (rect.x_max -. rect.x_min) +. rect.x_min;
    y = Random.float (rect.y_max -. rect.y_min) +. rect.y_min
  }

let transform_rect (t : transformation) (r : rectangle) : rectangle =

  let corner_rec = corners r                                  (*sommets du rectangle *)
  in let transform_corner = List.map (transform t) corner_rec (*applique transformation t a chaque point*)
  in rectangle_of_list transform_corner                       (*crée rectangle avec points transformés*)
      
                      
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
                                        


let inclusion (r : rectangle) (t : rectangle) : bool =
  if (r.x_min >= t.x_min && r.x_max <= t.x_max && r.y_min >= t.y_min && r.y_max <= t.y_max) then true
  (* else if (r.x_min <= t.x_min && r.x_max >= t.x_max && r.y_min <= t.y_min && r.y_max >= t.y_max) then true *)
  else false

let target_reached_rect (prog : program) (r : rectangle) (target : rectangle) : bool =
  let list_prog = all_choices prog in
  let rect_visited prog = 
    let rect_visited = run_rect prog r in
    match List.rev rect_visited with
    | [] -> false
    | last_rect :: _ -> inclusion last_rect target
  in   List.for_all rect_visited list_prog (* apply notre sous fonction check_program a notre liste de possibilités all_prog_choices *)

let rec run_polymorphe (apply_transform : transformation -> 'a -> 'a) (prog : program) (state : 'a) : 'a list =
    List.fold_left
    (fun positions instr ->
      let current_state = List.hd (List.rev positions) in
      let new_states = execute_prog apply_transform current_state instr in
      positions @ new_states
    )
    [state]
    (unfold_repeat prog)

    and execute_prog (apply_transform : transformation -> 'a -> 'a) (state : 'a) (instr : instruction) : 'a list =
      match instr with
      | Move t -> [apply_transform t state] (* Applique une seule transformation *)
      | Repeat (n, subprog) ->
        let repeated_prog = repeat_to_flat n subprog in
        run_polymorphe apply_transform repeated_prog state
        | Either (p1, p2) ->
          let chosen_prog = if Random.bool () then p1 else p2 in
          run_polymorphe apply_transform chosen_prog state
      (* renvoie le plus petit rectangle contenant les 2 rectangles de base  *)
let union_rectangle (r1 : rectangle) (r2 : rectangle) : rectangle = 
  {
    x_min = min r1.x_min r2.x_min;
    y_min = min r1.y_min r2.y_min;
    x_max = max r1.x_max r2.x_max;
    y_max = max r1.y_max r2.y_max;
  }


let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  (* Déroule le programme avec unfold_repeat pour transformer Repeat et Either *)
  let unfolded_prog = unfold_repeat prog in
  List.fold_left
    (fun current_rect instr -> execute_instr current_rect instr)
    r
    unfolded_prog

  and execute_instr (r : rectangle) (instr : instruction) : rectangle =
    match instr with
    | Move t -> transform_rect t r
    | Repeat (_, _) -> failwith "Unfold_repeat devrait déjà avoir transformé Repeat."
    | Either (p1, p2) -> 
        let rect_p1 = over_approximate p1 r in
        let rect_p2 = over_approximate p2 r in
        union_rectangle rect_p1 rect_p2
        
let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  let rect_target = over_approximate prog r in
  inclusion  rect_target target
