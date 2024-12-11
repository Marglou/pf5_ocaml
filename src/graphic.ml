open Graphics
open Geo
open Interp
open Approx

(* Options *)
let size = ref (800, 600)
let bg_color = ref (255, 255, 255)
let fg_color = ref (0, 25, 0)
let rect_color = ref (0, 255, 0)
let point_color = ref (0, 0, 255)
let abs_mode = ref false
let arg = ref 1

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

(* Afficher un point *)
let draw_point { Geo.x; Geo.y } =
  let r, g, b = !point_color in
  set_color (rgb r g b);
  fill_rect (int_of_float x - 3) (int_of_float y - 3) 6 6

(* Afficher un rectangle *)
let draw_rectangle rect =
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

(* Dessiner des lignes de division avec graduations *)
let draw_lines () =
  let width, height = !size in
  let step = 20 in
  let grad_length = 10 in

  (* Fonction pour dessiner les graduations *)
  let draw_graduations () =
    set_color (rgb 0 0 0);
    for x = 0 to width do
      if x mod step = 0 then begin
        moveto x (height / 2 - grad_length / 2);
        lineto x (height / 2 + grad_length / 2)
      end
    done;
    for y = 0 to height do
      if y mod step = 0 then begin
        moveto (width / 2 - grad_length / 2) y;
        lineto (width / 2 + grad_length / 2) y
      end
    done
  in

  set_color (rgb 0 0 0);
  moveto (width / 2) 0;
  lineto (width / 2) height;
  moveto 0 (height / 2);
  lineto width (height / 2);
  draw_graduations ()

(* Fonction pour analyser les arguments *)
let parse_arguments () =
  let args = Array.to_list Sys.argv in
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
  let rec parse_options = function
    | [] -> ()
    | "-size" :: value :: rest -> size := parse_size value; parse_options rest
    | "-bc" :: value :: rest -> bg_color := parse_color value; parse_options rest
    | "-fc" :: value :: rest -> fg_color := parse_color value; parse_options rest
    | "-rc" :: value :: rest -> rect_color := parse_color value; parse_options rest
    | "-pc" :: value :: rest -> point_color := parse_color value; parse_options rest
    | "-abs" :: rest -> abs_mode := true; parse_options rest
    | [last] -> arg := int_of_string last
    | _ -> failwith "Invalid command-line options."
  in
  parse_options (List.tl args)

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

let run () =
  parse_arguments ();
  set_window ();
  draw_lines ();
  Printf.printf "Running program with abs_mode=%b, arg=%d\n%!" !abs_mode !arg;
  let program = match !arg with
    | 1 -> prog1
    | 2 -> prog2
    | 3 -> prog3
    | _ -> failwith "Choisissez un programme valide."
  in
  Printf.printf "Selected program: %d\n%!" !arg;
  Printf.printf "Initial state: %s\n%!"
    (if !abs_mode
     then Printf.sprintf "Point {x=0.00, y=0.00}"
     else Printf.sprintf "Rectangle {x_min=0.00, x_max=10.00, y_min=0.00, y_max=10.00}");
  if !abs_mode then
    (Printf.printf "Executing in absolute mode (points).\n%!";
     execute_prog_point program { Geo.x = 0.; Geo.y = 0. })
  else
    (Printf.printf "Executing in rectangle mode (approximations).\n%!";
     execute_prog_rect program { Geo.x_min = 0.; Geo.x_max = 10.; Geo.y_min = 0.; Geo.y_max = 10. });
  ignore (read_key ());
  close_graph ()
