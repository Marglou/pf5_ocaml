open Graphics
open Geo
open Interp
open Approx


(* Options pour la configuration *)
let size = ref (800, 600)  (* Taille par défaut de la fenêtre HxW *)
let bg_color = ref (255, 255, 255)  (* Couleur de fond par défaut *)
let fg_color = ref (0, 25, 0)  (* Couleur de dessin par défaut *)
let rect_color = ref (0, 255, 0)  (* Couleur du rectangle par défaut *)
let point_color = ref (0, 20, 0)  (* Couleur du point par défaut *)

(* ARG : programmes possibles *)
let arg = ref 1
let prog1 = [Move (Translate { x = 100.; y = 100. })]  (* Déplacer un point à (100, 100) *)
let prog2 = 2
let prog3 = 3


(* Fonction pour configurer la fenêtre graphique *)
let run () =
  let width, height = !size in
  open_graph (Printf.sprintf " %dx%d" width height);
  let r, g, b = !bg_color in
  set_color (rgb r g b);
  fill_rect 0 0 width height

(* Dessiner des lignes de division avec graduations *)
let draw_lines () =
  let width, height = !size in
  let step = 20 in (* Intervalle entre chaque graduation *)
  let grad_length = 10 in (* Longueur des graduations *)

  (* Fonction pour dessiner les graduations *)
  let draw_graduations () =
    set_color (rgb 0 0 0); (* Noir pour les graduations *)

    (* Graduations horizontales *)
    for x = 0 to width do
      if x mod step = 0 then begin
        moveto x (height / 2 - grad_length / 2);
        lineto x (height / 2 + grad_length / 2)
      end
    done;

    (* Graduations verticales *)
    for y = 0 to height do
      if y mod step = 0 then begin
        moveto (width / 2 - grad_length / 2) y;
        lineto (width / 2 + grad_length / 2) y
      end
    done
  in

  set_color (rgb 0 0 0); (* Noir pour les lignes *)

  (* Ligne verticale au centre *)
  moveto (width / 2) 0;
  lineto (width / 2) height;

  (* Ligne horizontale au centre *)
  moveto 0 (height / 2);
  lineto width (height / 2);

  (* Appeler la fonction pour dessiner les graduations *)
  draw_graduations ()

(* Fonction pour analyser les arguments *)
let parse_arguments () =
  let args = Sys.argv in
  let parse_size s =
    match String.split_on_char 'x' s with
    | [h; w] -> (int_of_string h, int_of_string w)
    | _ -> failwith "Invalid size format. Use HeightxWidth (e.g., 800x600)."
  in
  let parse_color c =
    match String.split_on_char ',' c with
    | [r; g; b] -> (int_of_string r, int_of_string g, int_of_string b)
    | _ -> failwith "Invalid color format. Use R,G,B (e.g., 255,0,0)."
  in
  Array.iteri (fun i arg ->
    match arg with
    | "-size" when i + 1 < Array.length args ->
        size := parse_size args.(i + 1)
    | "-bc" when i + 1 < Array.length args ->
        bg_color := parse_color args.(i + 1)
    | "-fc" when i + 1 < Array.length args ->
        fg_color := parse_color args.(i + 1)
    | "-rc" when i + 1 < Array.length args ->
        rect_color := parse_color args.(i + 1)
    | "-pc" when i + 1 < Array.length args ->
        point_color := parse_color args.(i + 1)
    | _ -> ()
  ) args

(* Affichage d'un point *)
let draw_point { Geo.x; Geo.y } =
  let r, g, b = !point_color in
  set_color (rgb r g b);
  fill_rect (int_of_float x - 3) (int_of_float y - 3) 6 6

(* Dessiner un rectangle à partir de coordonnées données *)
let draw_rectangle rect =
  let r, g, b = !rect_color in
  set_color (rgb r g b);
  let x = int_of_float rect.x_min in
  let y = int_of_float rect.y_min in
  let width = int_of_float (rect.x_max -. rect.x_min) in
  let height = int_of_float (rect.y_max -. rect.y_min) in
  draw_rect x y width height

    

(* Fonction pour exécuter un programme donné et afficher les résultats dans la fenêtre graphique *)
(* let execute_prog apply_transform prog state =
  let target = over_approximate prog state in  (* Calcul de l'approximation du programme *)
  
  (* Fonction récursive pour dessiner des rectangles à partir des résultats *)
  (* let rec draw_rectangles = function
    | [] -> ()
    | r :: rs -> 
        draw_rectangle (int_of_float r.x_min, int_of_float r.y_min) (int_of_float (r.x_max -. r.x_min), int_of_float (r.y_max -. r.y_min));  (* Dessiner un rectangle *)
        draw_rectangles rs  (* Appel récursif pour dessiner les autres rectangles *)
  in *)
  draw_rectangle (int_of_float target.x_min, int_of_float target.y_min) (int_of_float (target.x_max -. target.x_min), int_of_float (target.y_max -. target.y_min));  (* Dessiner un rectangle *)


  (* Point de départ (au centre de la fenêtre) *)
  let starting_point = (400, 300) in

  (* Appliquer les transformations et obtenir les positions *)
  let positions = run_polymorphe apply_transform prog state in

  (* Fonction récursive pour dessiner les points à partir des positions *)
  let rec draw_positions = function
    | [] -> ()  (* Si la liste est vide, ne rien faire *)
    | p :: ps -> 
        draw_point p;  (* Dessiner un point *)
        draw_positions ps  (* Appel récursif pour dessiner les autres points *)
  in

  (* Dessiner les positions calculées *)
  draw_positions positions *)


(* 
  run_polymorphe transform [Repeat (4, [Move ROT({0,0},90)])] {1,0}.
  run_polymorphe transform_rect [Move TRANS({2,1})] {0,1,0,1}
*)


(* Main *)
let () =
  (* try *)
    (* Analyser les arguments en ligne de commande *)
    parse_arguments ();
    (* Initialiser la fenêtre *)
    run ();
    (* Dessiner les lignes graduées et autres éléments *)
    draw_lines ();

    draw_rectangle { Geo.x_min = 100.; Geo.x_max = 150.; Geo.y_min = 100.; Geo.y_max = 150. };
    (* Executer le programme donné et dessiner la progression du point  *)
    (* let point = {x = 50.; y = 50.} in
    match !arg with
    | 1 -> execute_program transform prog1 point
    | 2 -> execute_program prog2
    | 3 -> execute_program prog3
    | _ -> failwith "Donner un numéro pour executer le programme. Use 1, 2, or 3." *)

    (* Attendre que l'utilisateur appuie sur une touche pour fermer la fenêtre *)
    ignore (read_key ());
    close_graph ()

  (* with *)
  (* | Failure msg -> eprintf "Error: %s\n" msg; exit 1 *)
  (* | Graphic_failure _ -> eprintf "Graphics error: Check your environment.\n"; exit 1 *)
