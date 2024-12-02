(* graphic.mli *)
open Graphics

(* Fonction pour ouvrir la fenêtre graphique *)
val open_window : int -> int -> unit

(* Fonction pour fermer la fenêtre graphique *)
val close_window : unit -> unit

(* Fonction pour dessiner un rectangle *)
val draw_rectangle : int -> int -> int -> int -> unit

(* Fonction pour dessiner un cercle *)
val draw_circle : int -> int -> int -> unit

(* Fonction pour dessiner une ligne *)
val draw_line : int -> int -> int -> int -> unit

(* Fonction pour définir la couleur du dessin *)
val set_color : int -> int -> int -> unit

(* Fonction pour remplir la fenêtre avec une couleur *)
val fill_background : int -> int -> int -> unit
