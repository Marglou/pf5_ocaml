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
  | Move _ :: reste -> is_deterministic reste       (*recursion pour regarder reste de instructions dans prog*)
  | Repeat (_, subprog) :: reste -> is_deterministic subprog && is_deterministic reste  (*recursion pour regarder reste de instructions dans prog et dans les sous programme dans repeat*)
  | Either _ :: _ -> false                          (*false dès qu'il y a un repeat*)



  exception NonDeterministicProgram

  let rec unfold_repeat (prog : program) : program =
    List.flatten (                                            (*combines sous listes en une liste "homogène"*)
      List.map (function
        | Move t -> [Move t]                                  (* move reste pareil, on l'ajoute tel quel au nvx prog *)
        | Repeat (n, sousprog) -> repeat_to_flat n sousprog   (* fonction auxiliaire pour derouler  *)
        | Either _ -> raise NonDeterministicProgram           (*on lève une exception pour either car doit être deterministe *)
      ) prog                                                  (*applique function a prog avec map*)
    )
  
  and repeat_to_flat (n : int) (sousprog : program) : program =
    let flat_sousprog = unfold_repeat sousprog in
      List.init n (fun _ -> flat_sousprog) 
      |> List.flatten


(*EXERCICE 4.2*)

let rec run_det (prog : program) (p : point) : point list =
  List.fold_left                                        (*itère chaque instruction dans prog*) (*forme: List.fold_left (fonction) [liste points] programme*)
    (fun positions instr ->                       
      let current_pos = List.hd (List.rev positions) in (*La position courrante est le dernier point ajouté a la liste de points soit head du revers de la liste*)
      positions @ execute_prog current_pos instr)       (*on ajoute a positions le nouveau point visité *)
    [p]                                                 (*on apply le programme depuis le premier point p*)
    prog                                                (*instructions qui vont être traitées par les fonctions auxiliaires depuis p pour avoir les autres points*)

  and execute_prog (p : point) (i : instruction) : point list = (*interprète instruction sur un point*)
    match i with                                                (*regarde quelle instruction on traite*)
    | Move t -> [transform t p]                                 (* dans geo.ml *)
    | Repeat (n, sousprog) -> prog_repeat n sousprog p          (*traitement en boucle pour Repeat*)
    | Either _ -> raise NonDeterministicProgram                 (* Lève une exception si Either est rencontré *)

    and prog_repeat (n : int) (sousprog : program) (start_position : point) : point list =
      let rec repeat n current_pos tmp =                         (*fonction auxiliaire pour répéter n fois le sous programme*) 
        if n <= 0 then tmp                                        (*si n <= 0, on retourne la liste des positions -> tmp*)                     
        else
          let new_positions = List.fold_left (fun tmp instr ->  (*on itère sur chaque instruction dans le sous programme*)
            let new_pos = execute_prog current_pos instr in     (*on execute chaque instruction sur la position courante*)
            match new_pos with                                  (*on regarde si on a des nouveaux points*)
            | [] -> tmp                                         (*si pas de nouveaux points, on retourne la liste des positions -> tmp*)
            | last_pos :: _ -> tmp @ new_pos) [] sousprog in  (*on ajoute les nouveaux points à la liste des positions*)
          repeat (n - 1) (List.hd (List.rev new_positions)) (tmp @ new_positions)   (*on continue la boucle avec la dernière position*)
      in
      repeat n start_position []
    


(* 4.3 : premier rendu *)
let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  let points_visited = run_det prog p in  (* Obtenir la liste des points visités avec la fonction précédente *)
  match List.rev points_visited with    (* Prendre les points visités dans l'ordre inverse pour avoir la dernière position *)
  | [] -> false
  | last_pos :: _ -> in_rectangle target last_pos (* Vérifier si la dernière position est dans le rectangle cible *)
  
  

  
let run (prog : program) (p : point) : point list =
  let rec execute_prog prog p visited_positions =
    match prog with
    | [] -> visited_positions  (* Retourner les positions visitées *)
    | Move t :: rest ->  (* Déplacer le point *)
        let new_pos = transform t p in
        execute_prog rest new_pos (visited_positions @ [new_pos])  (* Ajouter la nouvelle position à la fin *)

    | Repeat (n, sousprog) :: rest ->  (* Répéter le sous-programme *)
      let rec repeat n current_pos tmp =
          if n <= 0 then tmp  (* Si n <= 0, retourner la liste tmp qui stocke les points de repeat *)
          else
            let new_positions = List.fold_left (fun tmp instr ->
              let new_pos = execute_prog [instr] current_pos [] in  (* Exécuter chaque instruction *)
              match new_pos with
              | [] -> tmp
              | last_pos :: _ -> tmp @ new_pos) [] sousprog in
            repeat (n - 1) (List.hd (List.rev new_positions)) (tmp @ new_positions)
        in execute_prog rest p (visited_positions @ repeat n p [])  (* Ajouter les points visités par repeat à la liste *)

    | Either (p1, p2) :: rest ->  (* Exécuter l'un des deux programmes *)
        let choose_prog = if Random.bool () then p1 else p2 in
        execute_prog (choose_prog @ rest) p visited_positions  (* Ajouter le programme choisi à la suite *)
  in execute_prog prog p [p]
  




let all_choices (prog : program) : program list =
  failwith "À compléter"

let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  (* failwith "À compléter" *)
  let points_visited = all_choices prog in
  let rec verif_points point = 
    match point with
    | [] -> true
    | point :: reste -> if in_rectangle r point then verif_point reste else false
