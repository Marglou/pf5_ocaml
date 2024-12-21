open Geo

(* Code de la Section 4 du projet. *)

type instruction =
  Move of transformation
| Repeat of int * program
| Either of program * program 
and program = instruction list (* and -> pour définition récursive mutuelle *)


let rec is_deterministic (prog : program) : bool =
  match prog with
  | [] -> true  
  | Move _ :: reste -> is_deterministic reste       (*recursion pour regarder reste de instructions dans prog*)
  | Repeat (_, subprog) :: reste -> is_deterministic subprog && is_deterministic reste  (*recursion pour regarder reste de instructions dans prog et dans les sous programme dans repeat*)
  | Either _ :: _ -> false                          (*false dès qu'il y a un repeat*)
  exception NonDeterministicProgram  (* init exception pour raise *)

  let rec unfold_repeat (prog : program) : program =
    List.flatten (                                            (*combines sous listes en une liste "homogène"*)
      List.map (function
        | Move t -> [Move t]                                  (* move reste pareil, on l'ajoute tel quel au nvx prog *)
        | Repeat (n, sousprog) -> repeat_to_flat n sousprog   (* fonction auxiliaire pour derouler  *)
        (* | Either _ -> raise NonDeterministicProgram           on lève une exception pour either car doit être deterministe *)
        | Either (p1, p2) -> [Either (p1, p2)]                  

      ) prog                                                  (*applique function a prog avec map*)
    )
  
  and repeat_to_flat (n : int) (sousprog : program) : program =   (* fonction aux pour dérouler le repeat*)
    let flat_sousprog = unfold_repeat sousprog in                 (* déroule une fois le repeat *)
      List.init n (fun _ -> flat_sousprog)                        (* copie de n fois flat_sousprog <=> effectue le repeat | fun_ -> fonction anonyme *)
      |> List.flatten                                             (* aplatie la liste dans le cas de repeat (repeat) ou plusieurs prog *)


(*EXERCICE 4.2*)

let rec run_det (prog : program) (p : point) : point list =
  List.fold_left                                        (*itère chaque instruction dans prog*) 
    (fun positions instr ->                       
      let current_pos = List.hd (List.rev positions) in (*La position courrante est le dernier point ajouté a la liste de points soit head du revers de la liste *)
      positions @ execute_prog current_pos instr)       (*on ajoute a positions le nouveau point visité *)
    [p]                                                 (*on apply le programme depuis le premier point p*)
    (unfold_repeat prog  )                              (* on unfold le programme doonné *)

  and execute_prog (p : point) (i : instruction) : point list = (*interprète instruction sur un point*)
    match i with                                                (*regarde quelle instruction on traite*)
    | Move t -> [transform t p]                                 (* dans geo.ml *)
    | Repeat (n, sousprog) -> run_det (repeat_to_flat n sousprog ) p     (*traitement en boucle pour Repeat*)
    | Either _ -> raise NonDeterministicProgram                 (*Lève une exception si Either est rencontré*)
    

let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  let points_visited = run_det prog p in  (* Obtenir la liste des points visités avec la fonction précédente *)
  match List.rev points_visited with      (* Prendre les points visités dans l'ordre inverse pour avoir la dernière position *)
  | [] -> false
  | last_pos :: _ -> in_rectangle target last_pos (* Vérifier si la dernière position est dans le rectangle cible *)
  
  
let rec unfold_repeat_nonDet (prog : program) : program =
  List.flatten (                                            (*combines sous listes en une liste "homogène"*)
    List.map (function
      | Move t -> [Move t]                                  (* move reste pareil, on l'ajoute tel quel au nvx prog *)
      | Repeat (n, sousprog) -> repeat_to_flat n sousprog   (* fonction auxiliaire pour derouler  *)
      | Either (p1, p2) -> [Either (p1, p2)]                  
      ) prog                                                 
  )
  
(* Exercice 4.5 *)
let rec run (prog : program) (p : point) : point list =
  List.fold_left                                        (*itère chaque instruction dans prog*) 
    (fun positions instr ->                       
      let current_pos = List.hd (List.rev positions) in (*La position courrante est le dernier point ajouté a la liste de points soit head du revers de la liste *)
        positions @ execute_prog current_pos instr)       (*on ajoute a positions le nouveau point visité *)
      [p]                                                 (*on apply le programme depuis le premier point p*)
      ( unfold_repeat prog )                              (* on unfold le programme doonné *)
    
  and execute_prog (p : point) (i : instruction) : point list = (*interprète instruction sur un point*)
    match i with                                                (*regarde quelle instruction on traite*)
      | Move t -> [transform t p]                                 (* dans geo.ml *)
      | Repeat (n, sousprog) -> run (repeat_to_flat n sousprog ) p     (*traitement en boucle pour Repeat*)
      | Either (p1, p2) ->                                          (* Exécuter l'un des deux programmes *)
        let choose_prog = if Random.bool () then p1 else p2 in
      run (choose_prog) p                                     (* Ajouter le programme choisi à la suite *)


(*Exercice 4.6*)
let rec all_choices (prog : program) : program list = (*fait tous les choix/possibilités de programes deterministes à partir d'un prog non-deterministe*)
  match prog with
  | [] -> [[]]            
  (* Cas de base --> programme vide est deterministe *)

  | Move t :: reste ->
      List.map (fun choice -> Move t :: choice) (all_choices reste)
    (* Move --> move est deterministe donc on l'ajoute avant chaque possible execution du reste (Move t :: choice)
            --> reste est traité récursivement et va donner une liste des possibilités du programme (fun choice avec fun= all_choices reste) *)

  | Repeat (n, subprog) :: reste ->
      let unfolded = repeat_to_flat n subprog in
      let choices_for_rest = all_choices reste in
      let choices_for_unfolded = all_choices unfolded in
      List.flatten (
        List.map
          (fun choice_unfolded ->
              List.map (fun choice_rest -> choice_unfolded @ choice_rest) choices_for_rest
          ) choices_for_unfolded
      )
      
    (* Repeat --> est non deterministe donc on va derouler repeat pour le rendre deterministe (unfolded=repeat_to_flat ET all_choices unfolded)
              --> combine avec chaque possibilité du reste (map)  *)
    (*Maps imbriqués --> pour chaque possibilité du unfolded on va ajouter à chaque possibiilté du reste*)

  | Either (p1, p2) :: reste ->
      let choices_p1 = all_choices p1 in
      let choices_p2 = all_choices p2 in
      let choices_rest = all_choices reste in
      List.flatten (
        List.map
          (fun choice_p1 ->
              List.map (fun choice_rest -> choice_p1 @ choice_rest) choices_rest
          ) choices_p1
      )
      @
      List.flatten (
        List.map
          (fun choice_p2 ->
              List.map (fun choice_rest -> choice_p2 @ choice_rest) choices_rest
          ) choices_p2
      )
      (*Either --> all_choices pour les deux options et on combine avec all_choices du reste*)
      
(*Exercice 4.7*)
let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  let all_prog_choices = all_choices prog in    (* Genere la liste de toutes les possibilitées deterministes du programme *)
  let check_program program =                   (* Definie sous fonction pour check chaque possibilité *)
    let visited_points = run_det program p in   (* Genere tous les points visités quand on exec le programme p*)
    match List.rev visited_points with
    | [] -> false                               (* Cas de base: si aucun point est visité alors le target est pas atteint *)
    | last_pos :: _ -> in_rectangle r last_pos  (* Regarde si la dernière position apres avoir exec le programme est in target *)
  in
  List.for_all check_program all_prog_choices  (* apply notre sous fonction check_program a notre liste de possibilités all_prog_choices *)
            
