(* open Graphics;;

let width = 800;;
let height = 600;;

(* Ouvrir une fenêtre graphique de 800x600 pixels *)
let () =
  (* let width = 800 in
  let height = 600 in *)
  open_graph (Printf.sprintf " %dx%d" width height);
  
  (* Définir la couleur de fond (gris clair) *)
  set_color (rgb 255 0 0);
  fill_rect 0 0 width height;
  
  (* Définir la couleur des lignes *)
  set_color (rgb 0 0 0);
  
  (* Dessiner des lignes pour diviser la fenêtre *)
  moveto (width / 2) 0;
  lineto (width / 2) height;  (* Ligne verticale au centre *)
  
  moveto 0 (height / 2);
  lineto width (height / 2);  (* Ligne horizontale au centre *)

  (* Attendre qu'une touche soit pressée avant de fermer la fenêtre *)
  ignore (read_key ());
  
  (* Fermer la fenêtre graphique *)
  close_graph ();;
 *)

 open Graphics
open Printf

(* POUR COMPILER : ocamlc -I $(opam var lib)/graphics graphics.cma graphic.ml -o graphic  *)

(* Variables pour la configuration *)
let size = ref (800, 600) (* Taille par défaut de la fenêtre *)
let bg_color = ref (255, 0, 0) (* Couleur de fond par défaut *)

(* Fonction pour configurer la fenêtre graphique *)
let setup_window () =
  let width, height = !size in
  open_graph (Printf.sprintf " %dx%d" width height);
  let r, g, b = !bg_color in
  set_color (rgb r g b);
  fill_rect 0 0 width height

(* Dessiner des lignes de division *)
let draw_lines () =
  let width, height = !size in
  set_color (rgb 0 0 0); (* Noir pour les lignes *)
  (* Ligne verticale au centre *)
  moveto (width / 2) 0;
  lineto (width / 2) height;
  (* Ligne horizontale au centre *)
  moveto 0 (height / 2);
  lineto width (height / 2)

(* Fonction pour analyser les arguments *)
let parse_arguments () =
  let args = Sys.argv in
  let parse_size s =
    match String.split_on_char 'x' s with
    | [w; h] -> (int_of_string w, int_of_string h)
    | _ -> failwith "Invalid size format. Use WxH (e.g., 800x600)."
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
    | "-color" when i + 1 < Array.length args ->
        bg_color := parse_color args.(i + 1)
    | _ -> ()
  ) args

(* Main *)
let () =
  try
    parse_arguments ();
    setup_window ();
    draw_lines ();
    ignore (read_key ()); (* Attendre une touche *)
    close_graph ()
  with
  | Failure msg -> eprintf "Error: %s\n" msg; exit 1
  | Graphic_failure _ -> eprintf "Graphics error: Check your environment.\n"; exit 1
