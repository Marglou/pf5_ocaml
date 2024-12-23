open Graphics
open Geo
open Interp
open Approx

(* Options *)
let size = ref (1000, 800) (* ref == variable gloable *)
let width = ref 1000
let height = ref 800
let bg_color = ref (255, 255, 255)
let fg_color = ref (0, 25, 0)
let rect_color = ref (0, 255, 0)
let point_color = ref (0, 0, 255)
let cr_mode = ref true
let abs_mode = ref false
let arg = ref 1
let rect_target = ref { Geo.x_min = 0.; Geo.x_max = 100.; Geo.y_min = 0.; Geo.y_max = 35. }
let rect_approx = ref { Geo.x_min = 0.; Geo.x_max = 0.; Geo.y_min = 0.; Geo.y_max = 0. }

(* Options supplémentaires *)
let step_by_step = ref false (* Executer un programme etape apr etape *)
let print_prog = ref false (* Affiche la ligne de cmd executée *)


(*------------------ PROGRAMMES DISPONIBLES ------------------------------------------------------- *)
let prog1 = [Move (Translate {x = 10.; y = 0.})]

let prog2 = [
  Move (Translate {x = 100.; y = 0.});
  Repeat (3, [Move (Rotate ({x = 50.; y = 50.}, 90.))])
]

let prog3 = [
  Either (
    [Move (Translate {x = 50.; y = 0.})],
    [Move (Translate {x = 0.; y = 50.})]
  );
  Either (
    [Move (Translate {x = 50.; y = 0.})],
    [Move (Translate {x = 0.; y = 50.})]
  )
]


(* --------------------------- DRAW ---------------------------------------- *)

  (* Centrer les coordonées sur l'axe tracé  *)
let scale_graphic_point { Geo.x; Geo.y } =
  {
    Geo.x = x +. float_of_int (!width / 2);
    Geo.y = y +. float_of_int (!height / 2);
  }

let scale_graphic_rect rect =
  let width, height = !size in
  {
    Geo.x_min = rect.Geo.x_min +. float_of_int (width / 2);
    Geo.x_max = rect.Geo.x_max +. float_of_int (width / 2);
    Geo.y_min = rect.Geo.y_min +. float_of_int (height / 2);
    Geo.y_max = rect.Geo.y_max +. float_of_int (height / 2);
  }

(* Afficher un point *)
let draw_point point =
  let { Geo.x; Geo.y } = scale_graphic_point point in
  let r, g, b = !point_color in
  set_color (rgb r g b);
  fill_rect (int_of_float x - 3) (int_of_float y - 3) 6 6


(* Afficher un rectangle *)
let draw_rectangle rect =
  let rect = scale_graphic_rect rect in
  let r, g, b = !rect_color in
  set_color (rgb r g b);
  let x = int_of_float rect.x_min in
  let y = int_of_float rect.y_min in
  let width = int_of_float (rect.x_max -. rect.x_min) in
  let height = int_of_float (rect.y_max -. rect.y_min) in
  draw_rect x y width height


(* Fonction pour configurer la fenêtre graphique *)
let set_window () =
  let width, height = !size in
  open_graph (Printf.sprintf " %dx%d" width height);
  let r, g, b = !bg_color in
  set_color (rgb r g b);
  fill_rect 0 0 width height


(* DESSIN DES AXES ET GRADUATIONS *)
(* Dessiner les axes principaux *)
let draw_axes width height =
  set_color (rgb 0 0 0);
  moveto (width / 2) 0;
  lineto (width / 2) height;
  moveto 0 (height / 2);
  lineto width (height / 2)

(* Dessiner les graduations sur l'axe X *)
let draw_x_graduations width height step grad_length =
  set_color (rgb 0 0 0);
  for x = -(width / 2) to (width / 2) do
    if x mod step = 0 then begin
      let gx = x + (width / 2) in

      (* Dessiner la graduation *)
      moveto gx (height / 2 - grad_length / 2);
      lineto gx (height / 2 + grad_length / 2);

      (* Afficher le numéro *)
      if x <> 0 then begin
        moveto gx (height / 2 + grad_length + 5);
        draw_string (string_of_int (x / step)); (* Numérotation ajustée supprimer le step pour avoir une graduation = pixel *)
      end
    end
  done


(* Dessiner les graduations sur l'axe Y *)
let draw_y_graduations width height step grad_length =
  set_color (rgb 0 0 0);
  for y = -(height / 2) to (height / 2) do
    if y mod step = 0 then begin
      let gy = y + (height / 2) in

      (* Dessiner la graduation *)
      moveto (width / 2 - grad_length / 2) gy;
      lineto (width / 2 + grad_length / 2) gy;

      (* Afficher le numéro *)
      if y <> 0 then begin
        moveto (width / 2 + grad_length + 5) gy;
        draw_string (string_of_int (y / step)); (* Numérotation ajustée *)
      end
    end
  done

(* Dessiner des lignes de division avec graduations et numérotation *)
let draw_lines () =
  let width, height = !size in
  let step = 50 in  (* Espacement des graduations augmenté *)
  let grad_length = 10 in
  draw_axes width height;
  draw_x_graduations width height step grad_length;
  draw_y_graduations width height step grad_length


(* Afficher du texte sur la fenetre graphique *)
let draw_text x y text =
  set_color (rgb 0 0 0);
  moveto x y;
  draw_string text

(* AFFICHE LA CIBLE DONNEE *)
let draw_target_rectangle rect_ref =
  let rect = scale_graphic_rect !rect_ref in (* Déréférencement avec ! *)
  let x = int_of_float rect.x_min in
  let y = int_of_float rect.y_min in
  let width = int_of_float (rect.x_max -. rect.x_min) in
  let height = int_of_float (rect.y_max -. rect.y_min) in
  (* Couleur de remplissage avec une opacité simulée *)
  set_color (rgb 255 200 200); (* Rouge clair pour simuler une transparence *)
  fill_rect x y width height (* Remplir le rectangle *)


let string_of_transform transform =
  match transform with
  | Translate { x; y } -> Printf.sprintf "Translate(x = %.2f, y = %.2f)" x y
  | Rotate ({ x; y }, angle) -> Printf.sprintf "Rotate(center = (%.2f, %.2f), angle = %.2f)" x y angle


(* --------------------------- PARSE ARGUMENT ---------------------------------------- *)
(* Vérifie que le rectangle approx soit correct dès le début *)
let abs_correct rect =
    if not (rect.Geo.x_min <= 0. && rect.Geo.x_max >= 0. && rect.Geo.y_min <= 0. && rect.Geo.y_max >= 0.) then
      failwith (Printf.sprintf "Le rectangle spécifié (x_min=%.2f, x_max=%.2f, y_min=%.2f, y_max=%.2f) ne contient pas l'origine (0, 0)." rect.Geo.x_min rect.Geo.x_max rect.Geo.y_min rect.Geo.y_max)
  

(* Fonction pour analyser les arguments *)
let parse_arguments () =
  let args = Array.to_list Sys.argv in
  let parse_size s =
    try
      match String.split_on_char 'x' s with
      | [h; w] -> (int_of_string h, int_of_string w)
      | _ -> raise Not_found
    with _ ->
      failwith (Printf.sprintf "Format incorrect: %s.  HauteurxLargeur (e.g., 800x600)." s)

  in
  let parse_color c =
    match String.split_on_char ',' c with
    | [r; g; b] -> (int_of_string r, int_of_string g, int_of_string b)
    | _ -> failwith "Format couleur invalide.  R,G,B (e.g., 255,0,0)."
  in
  
  let rec parse_options = function
    | [] -> ()
    | "-size" :: value :: rest -> size := parse_size value; parse_options rest
    | "-bc" :: value :: rest -> bg_color := parse_color value; parse_options rest
    | "-fc" :: value :: rest -> fg_color := parse_color value; parse_options rest
    | "-rc" :: value :: rest -> rect_color := parse_color value; parse_options rest
    | "-pc" :: value :: rest -> point_color := parse_color value; parse_options rest
    | "-cr" :: rest -> cr_mode := true; parse_options rest
    | "-abs" :: x_min :: y_min :: x_max :: y_max :: rest ->
        let rect = {
          Geo.x_min = float_of_string x_min;
          Geo.x_max = float_of_string x_max;
          Geo.y_min = float_of_string y_min;
          Geo.y_max = float_of_string y_max;
        } in
        abs_correct rect;     (* Vérifier si le rectangle contient l'origine *)
        (* rect_target := rect;  Mettre à jour la cible *)
        abs_mode := true; (* Activer le mode rectangle *)
        cr_mode := false; (* Désactiver le mode point *)
        rect_approx := rect; (* Mettre à jour le rectangle approximatif *)
        parse_options rest
    | "-print" :: rest -> print_prog := true; parse_options rest
    | "-steps" :: rest -> step_by_step := true; parse_options rest
    | [last] -> arg := int_of_string last
    | _ -> failwith "Options invalides."
  in
  parse_options (List.tl args)


 

(* ------------------------------------------------------------------------------------------------ *)

(* Effacer l'écran *)
let clear_screen () =
  let width, height = !size in
  let r, g, b = !bg_color in
  set_color (rgb r g b);
  fill_rect 0 0 width height

let delay seconds =
  let start_time = Sys.time () in
  let rec loop () =
    if Sys.time () -. start_time < seconds then
      loop ()
  in
  loop ()

  (* Fonction qui attend un événement et exécute une action en fonction de la touche *)
let handle_key_event () =
  match wait_next_event [Key_pressed] with
  | { key = 'p'; _ } -> `StepForward  (* Avancer *)
  | { key = 'q'; _ } -> `StepBackward (* Reculer *)
  | { key = 'e'; _ } -> `Exit         (* Quitter *)
  | _ -> `NoAction                     (* Aucune action *)

  let wait_for_exit () =
    let rec loop () =
      match wait_next_event [Key_pressed] with
      | { key = 'e'; _ } -> close_graph () (* Fermer la fenêtre quand 'e' est pressé *)
      | _ -> loop () (* Continuer à attendre *)
    in
    loop ()


let rec string_of_instruction instr =
  match instr with
  | Move t -> Printf.sprintf "Move: %s" (string_of_transform t)
  | Repeat (n, prog) -> 
      let prog_str = String.concat "; " (List.map string_of_instruction prog) in
      Printf.sprintf "Repeat %d times: [%s]" n prog_str
  | Either (prog1, prog2) ->
      let prog1_str = String.concat "; " (List.map string_of_instruction prog1) in
      let prog2_str = String.concat "; " (List.map string_of_instruction prog2) in
      Printf.sprintf "Either: [%s] OR [%s]" prog1_str prog2_str


(* Fonction pour accéder à l'instruction à une position donnée *)
let instruction_at_step n prog =
  if n >= 1 && n <= List.length prog then
    Some (List.nth prog (n - 1))
  else
    None

(* ---------------------------------  EXECUTER LE PROGRAMME AVEC DESSIN -------------------------------------------------------------- *)

(* Approximation du rectangle *)
let rec run_rect_approx (prog : program) (r : rectangle) : rectangle list =
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
        let rects_p1 = run_rect_approx p1 r in
        let rects_p2 = run_rect_approx p2 r in
        let combined_rects = List.map2 union_rectangle rects_p1 rects_p2 in
        combined_rects


type execution_mode =
  | PointOnly
  | RectOnly
  | PointAndRect
  
(* Fonction pour initialiser l'affichage *)
let init_window rect_target =
  clear_screen ();
  draw_target_rectangle rect_target;
  if !step_by_step then
    draw_text 10 (!height - 20) "Appuyez sur 'p' pour avancer, 'q' pour reculer, 'e' pour quitter."
  else
    draw_text 10 (!height - 20) "Appuyez sur 'e' pour quitter."
  
(* Fonction pour dessiner un état *)
let draw_state state draw_form =
  draw_form state;
  draw_lines () (* Dessin des lignes de la grille ou autres éléments visuels *)

let draw_step_info current_step instruction_opt =
  let message = match instruction_opt with
    | Some step_instruction ->
        let step_string = string_of_instruction step_instruction in
        Printf.sprintf "Step %d: %s" current_step step_string
    | None -> 
        Printf.sprintf "Step %d: Non trouvé" current_step
  in
  draw_text 10 10 message


(* Fonction pour gérer le mode Step-by-Step *)
let option_step_by_step states draw_form rect_target prog check =
  let rec loop current_step =
    let step = max 0 (min current_step (List.length states )) in
    if step < (List.length states)  then
      let state = List.nth states step in
      init_window rect_target;
      draw_state state draw_form;

      if !print_prog then
        draw_step_info current_step (instruction_at_step current_step prog);
      
      (* Printf.printf "Point %d: %s\n" current_step; *)
      
      if step = List.length states then
        if check then
          draw_text 10 (!height - 50) "Cible atteinte."
        else
          draw_text 10 (!height - 50) "Cible non atteinte.";

    match handle_key_event () with
    | `StepForward -> 
        if current_step < List.length states - 1 then loop (current_step + 1)
        else draw_text 10 (!height - 70) "Derniere etape atteinte."
    | `StepBackward -> 
        if current_step > 1 then loop (current_step - 1)
        (* else draw_text 10 (!height - 70) "Première étape atteinte." *)
    | `Exit -> ()
    | `NoAction -> loop current_step
    
  in
  loop 0

(* Fonction pour traiter le mode sans Step-by-Step (Exécution complète) *)
let execute_auto states draw_form rect_target =
  List.iter (fun state ->
    init_window rect_target;
    draw_state state draw_form;
    (* if !print_prog then
      draw_text 10 10 (string_of_instruction state); *)
    delay 0.2;
  ) states

(* Fonction principale pour exécuter le programme en fonction du mode *)

(* Fonction pour gérer l'exécution de la boucle (automatique ou pas) *)
let execute_step_or_auto states draw_form rect_target prog check_target =
  if !step_by_step then
    option_step_by_step states draw_form rect_target prog check_target
  else
    execute_auto states draw_form rect_target;
    if check_target  then
      draw_text 10 (!height - 50) "Cible atteinte."
    else
      draw_text 10 (!height - 50) "Cible non atteinte."

(* Fonction pour exécuter le programme pour PointOnly, RectOnly ou PointAndRect *)
let execute_prog_polymorphe mode prog point_state rect_state rect_target =
  match mode with
  | PointOnly ->
    let states = run_polymorphe transform prog point_state in
    let draw_form state = draw_point state in
    let check_target = target_reached prog point_state !rect_target in
    execute_step_or_auto states draw_form rect_target prog check_target

  | RectOnly ->
    let states = run_rect_approx prog rect_state in
    let draw_form state = draw_rectangle state in
    let check_target = target_reached_rect prog rect_state !rect_target in
    execute_step_or_auto states draw_form rect_target prog check_target

  | PointAndRect ->
    let point_states = run_polymorphe transform prog point_state in
    let rect_states = run_rect_approx prog rect_state in
    let states = List.combine point_states rect_states in
    let draw_form (point, rect) =
      draw_point point;
      draw_rectangle rect
    in
    let check_target = target_reached prog point_state !rect_target in
    execute_step_or_auto states draw_form rect_target prog check_target



(* ---------------------------------------------------  MAIN ------------------------------------------------------------------------------ *)

let run () =
  (* Analyse des arguments et initialisation de la fenêtre *)
  parse_arguments ();
  set_window ();

  (* Sélectionner le programme en fonction de l'argument fourni *)
  let program = match !arg with
    | 1 -> prog1
    | 2 -> prog2
    | 3 -> prog3
    | _ -> failwith "Choisissez un programme valide."
  in

  let initial_point = { Geo.x = 0.; Geo.y = 0. } in
  let initial_rect =
    if !abs_mode then
      !rect_approx
    else
      { Geo.x_min = -5.; Geo.x_max = 5.; Geo.y_min = -5.; Geo.y_max = 5. }
  in
  
  (* Déterminer le mode d'exécution en fonction des arguments *)
  if not !cr_mode && not !abs_mode then
    failwith "Au moins un mode (point ou rectangle) doit être activé.";
    
  let mode = 
    if !cr_mode && not !abs_mode then
      PointOnly
    else if !abs_mode && not !cr_mode then
      RectOnly
    else
      PointAndRect
  in

  (* Exécuter le programme en fonction du mode *)
  execute_prog_polymorphe mode program initial_point initial_rect rect_target;

  (* Attente de la fermeture par l'utilisateur *)
  wait_for_exit ()
