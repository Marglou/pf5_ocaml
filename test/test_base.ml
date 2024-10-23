let add_tests, run_tests =
  let tests = ref [] in
  (fun id tsts -> tests := !tests @ [id, tsts]),
  (fun () -> Alcotest.run "pf5" !tests)

let add_tests_1 id fn_name fn pp testable tests =
  add_tests id @@
  List.map (fun (arg, exp) ->
    let descr = Fmt.str "%s %a" fn_name pp arg in
    let test () = Alcotest.check testable descr exp @@ fn arg in
    Alcotest.test_case descr `Quick test
  ) tests

let add_tests_2 id fn_name fn pp1 pp2 testable tests =
  add_tests id @@
  List.map (fun (arg1, arg2, exp) ->
    let descr = Fmt.str "%s %a %a" fn_name pp1 arg1 pp2 arg2 in
    let test () = Alcotest.check testable descr exp @@ fn arg1 arg2 in
    Alcotest.test_case descr `Quick test
  ) tests

let add_tests_3 id fn_name fn pp1 pp2 pp3 testable tests =
  add_tests id @@
  List.map (fun (arg1, arg2, arg3, exp) ->
    let descr = Fmt.str "%s %a %a %a" fn_name pp1 arg1 pp2 arg2 pp3 arg3 in
    let test () = Alcotest.check testable descr exp @@ fn arg1 arg2 arg3 in
    Alcotest.test_case descr `Quick test
  ) tests

let pp_list pp =
  Fmt.(brackets @@ list ~sep:(const char ';') pp)
