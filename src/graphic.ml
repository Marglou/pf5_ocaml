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
let print_prog = ref false
let target = ref { Geo.x_min = 0.; Geo.x_max = 100.; Geo.y_min = 0.; Geo.y_max = 35. }


(* Programmes *)
let prog1 = [Move (Translate {x = 100.; y = 100.})]
let prog2 = [
  Move (Translate {x = 100.; y = 0.});
  Repeat (3, [Move (Rotate ({x = 50.; y = 50.}, 90.))])
]
let prog3 = [
  Either (
    [Move (Translate {x = 20.; y = 0.})],
    [Move (Translate {x = 0.; y = 20.})]
  );
  Repeat (5, [Move (Rotate ({x = 100.; y = 100.}, 45.))])
]



(* --------------------------- DRAW ---------------------------------------- *)

  (* Centrer les coordonées sur l'axe tracé  *)
let to_graphic_coords { Geo.x; Geo.y } =
  {
    Geo.x = x +. float_of_int (!width / 2);
    Geo.y = y +. float_of_int (!height / 2);
  }

let to_graphic_rect rect =
  let width, height = !size in
  {
    Geo.x_min = rect.Geo.x_min +. float_of_int (width / 2);
    Geo.x_max = rect.Geo.x_max +. float_of_int (width / 2);
    Geo.y_min = rect.Geo.y_min +. float_of_int (height / 2);
    Geo.y_max = rect.Geo.y_max +. float_of_int (height / 2);
  }

(* Afficher un point *)
let draw_point point =
  let { Geo.x; Geo.y } = to_graphic_coords point in
  let r, g, b = !point_color in
  set_color (rgb r g b);
  fill_rect (int_of_float x - 3) (int_of_float y - 3) 6 6


(* Afficher un rectangle *)
let draw_rectangle rect =
  let rect = to_graphic_rect rect in
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
        draw_string (string_of_int (x / step)); (* Numérotation ajustée *)
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
  moveto x y;
  draw_string text

(* AFFICHE LA CIBLE DONNE *)
let draw_target_rectangle rect_ref =
  let rect = to_graphic_rect !rect_ref in (* Dereferencement avec ! *)
  set_color (rgb 255 0 0); (* Couleur rouge pour la cible *)
  let x = int_of_float rect.x_min in
  let y = int_of_float rect.y_min in
  let width = int_of_float (rect.x_max -. rect.x_min) in
  let height = int_of_float (rect.y_max -. rect.y_min) in
  draw_rect x y width height

(* --------------------------- PARSE ARGUMENT ---------------------------------------- *)
(* Fonction pour analyser les arguments *)
let parse_arguments () =
  let args = Array.to_list Sys.argv in
  let parse_size s =
    try
      match String.split_on_char 'x' s with
      | [h; w] -> (int_of_string h, int_of_string w)
      | _ -> raise Not_found
    with _ ->
      failwith (Printf.sprintf "Invalid size format: %s. Use HeightxWidth (e.g., 800x600)." s)

  in
  let parse_color c =
    match String.split_on_char ',' c with
    | [r; g; b] -> (int_of_string r, int_of_string g, int_of_string b)
    | _ -> failwith "Invalid color format. Use R,G,B (e.g., 255,0,0)."
  in
  
  let rec parse_options = function
    | [] -> ()
    | "-size" :: value :: rest -> size := parse_size value; parse_options rest
    | "-bc" :: value :: rest -> bg_color := parse_color value; parse_options rest
    | "-fc" :: value :: rest -> fg_color := parse_color value; parse_options rest
    | "-rc" :: value :: rest -> rect_color := parse_color value; parse_options rest
    | "-pc" :: value :: rest -> point_color := parse_color value; parse_options rest
    | "-cr" :: rest -> cr_mode := true; parse_options rest
    | "-abs" :: rest -> cr_mode := false; parse_options rest
    | "-print" :: rest -> print_prog := true; parse_options rest
    | [last] -> arg := int_of_string last
    | _ -> failwith "Options invalides."
  in
  parse_options (List.tl args);
  let (w, h) = !size in
  width := w;  (* var déclarée avec ref := nv valeur *)
  height := h
 

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

(* Fonction pour lire une touche *)
let read_key () =
  let rec loop () =
    match wait_next_event [Key_pressed] with
    | { key = 'q'; _ } -> ()
    | _ -> loop ()
  in
  loop ()

(* ---------------------------------  EXECUTER LE PROGRAMME AVEC DESSIN -------------------------------------------------------------- *)

let execute_prog_point prog state =
  let states = run_polymorphe transform prog state in
  Printf.printf "Debug Point States:\n%!";
  List.iter (fun { Geo.x; Geo.y } ->
    Printf.printf "Point: x = %.2f, y = %.2f\n%!" x y;
    clear_screen ();
    draw_lines ();
    draw_point { Geo.x; Geo.y };
    delay 0.2;
  ) states

let execute_prog_rect prog state =
  let states = run_polymorphe transform_rect prog state in
  Printf.printf "Debug Rectangle States:\n%!";
  List.iter (fun { Geo.x_min; Geo.x_max; Geo.y_min; Geo.y_max } ->
    Printf.printf "Rectangle: x_min=%.2f, x_max=%.2f, y_min=%.2f, y_max=%.2f\n%!"
      x_min x_max y_min y_max;
    clear_screen ();
    draw_lines ();
    draw_rectangle { Geo.x_min; Geo.x_max; Geo.y_min; Geo.y_max };
    delay 0.2;
  ) states

let execute_prog_point_and_rect prog point_state rect_state =
  let point_states = run_polymorphe transform prog point_state in
  let rect_states = run_polymorphe transform_rect prog rect_state in
  let states = List.combine point_states rect_states in
  List.iter (fun ({ Geo.x; Geo.y }, { Geo.x_min; Geo.x_max; Geo.y_min; Geo.y_max }) ->
    clear_screen ();
    draw_lines ();
    draw_point { Geo.x; Geo.y };
    draw_rectangle { Geo.x_min; Geo.x_max; Geo.y_min; Geo.y_max };
    delay 0.2;
  ) states


(* --------------------- MAIN ------------------------------------------------------------------------------ *)

let run () =
  parse_arguments ();
  set_window ();
  draw_lines ();

  let program = match !arg with
    | 1 -> prog1
    | 2 -> prog2
    | 3 -> prog3
    | _ -> failwith "Choisissez un programme valide."
  in
  Printf.printf "Selected program: %d\n%!" !arg;

  (* Vérifier les modes et exécuter le programme *)
  if !cr_mode && not !abs_mode then
    (* Mode point uniquement *)
    (Printf.printf "Executing in absolute mode (points).\n%!";
     execute_prog_point program { Geo.x = 0.; Geo.y = 0. })
  else if !abs_mode && not !cr_mode then
    (* Mode rectangle uniquement *)
    (Printf.printf "Executing in rectangle mode (approximations).\n%!";
     execute_prog_rect program { Geo.x_min = 0.; Geo.x_max = 10.; Geo.y_min = 0.; Geo.y_max = 10. })
  else
    (* Les deux modes activés *)
    (Printf.printf "Executing in combined mode (points and rectangles).\n%!";
     execute_prog_point_and_rect program { Geo.x = 0.; Geo.y = 0. } { Geo.x_min = 0.; Geo.x_max = 10.; Geo.y_min = 0.; Geo.y_max = 10. });

  ignore (read_key ());
  close_graph ()

