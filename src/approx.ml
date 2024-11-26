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
  r.x_min >= t.x_min && r.x_max <= t.x_max 
  && r.y_min >= t.y_min && r.y_max <= t.y_max
  

  let target_reached_rect (prog : program) (initial_rect : rectangle) (target_rect : rectangle) : bool =
    let all_prog_choices = all_choices prog in  (* tous les programmes déterministes de prog*)
    let points_to_test = corners initial_rect in  (* sommets = points de départ *)
    List.for_all
      (fun p_start ->
         List.for_all
           (fun prog_det ->
              let visited_points = run_det prog_det p_start in  (* Obtenir tous les points visités *)
              match List.rev visited_points with
              | [] -> false                                         (* Aucun point visité = pb *)
              | last_pos :: _ -> in_rectangle target_rect last_pos  (*dernière position in target? *)
           ) all_prog_choices
      ) points_to_test
  


let rec run_polymorphe (apply_transform : transformation -> 'a -> 'a) (prog : program) (state : 'a) : 'a list =
    match prog with
    | [] -> [state]  
    | Move t :: reste ->
    let new_state = apply_transform t state in
    state :: run_polymorphe apply_transform reste new_state

    | Repeat (n, sousprog) :: reste ->
    let rec repeat_states n current_state =
      if n <= 0 then [current_state]
      else
        let repeated_states = run_polymorphe apply_transform sousprog current_state 
        in match List.rev repeated_states with
        | [] -> []
        | last_state :: _ ->
            List.rev repeated_states @ repeat_states (n - 1) last_state
            in let repeated_result = repeat_states n state 
            in repeated_result @ run_polymorphe apply_transform reste (List.hd (List.rev repeated_result))

    | Either (p1, p2) :: _ ->
    failwith "error, non-det"


let rec over_approximate (prog : program) (r : rectangle) : rectangle =
  failwith "À compléter"

let feasible_target_reached (prog : program) (r : rectangle) (target : rectangle) : bool =
  failwith "À compléter"
