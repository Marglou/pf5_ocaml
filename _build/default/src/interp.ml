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
  | Move _ :: reste -> is_deterministic reste  
  | Repeat (_, subprog) :: reste -> is_deterministic subprog && is_deterministic reste  
  | Either _ :: _ -> false  



  exception NonDeterministicProgram

  let rec unfold_repeat (prog : program) : program =
    List.flatten (
      List.map (function
        | Move t -> [Move t]                                  (* move reste pareil, on l'ajoute tel quel au nvx prog *)
        | Repeat (n, sousprog) -> repeat_to_flat n sousprog   (* fonction auxiliaire pour derouler  *)
        | Either _ -> raise NonDeterministicProgram           (*on lève une exception pour either car doit être deterministe *)
      ) prog          (*applique function a prog avec map*)
    )
  
  and repeat_to_flat (n : int) (sousprog : program) : program =
    let flat_sousprog = unfold_repeat sousprog in
      List.init n (fun _ -> flat_sousprog) 
      |> List.flatten



      let rec run_det (prog : program) (p : point) : point list =
        List.fold_left
          (fun positions instr ->
            let current_pos = List.hd (List.rev positions) in
            positions @ execute_prog current_pos instr)
          [p]
          prog
      
      and prog_repeat (n : int)(sousprog : program)(start_position : point) : point list=
        if n<=0 then []
        else
          let position = run_det sousprog start_position in
          match List.rev position with
          |[]->[]
          |last :: _ -> position @ prog_repeat (n-1) sousprog last
      
      and execute_prog (p : point) (i : instruction) : point list =
        match i with
        | Move t -> [transform t p]  (* d/ geo.ml *)
        | Repeat (n, sousprog) -> prog_repeat n sousprog p  
        | Either _ -> raise NonDeterministicProgram  (* Lève une exception si Either est rencontré *)


let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  failwith "À compléter"
  
let run (prog : program) (p : point) : point list =
  failwith "À compléter"

let all_choices (prog : program) : program list =
  failwith "À compléter"

let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  failwith "À compléter"
