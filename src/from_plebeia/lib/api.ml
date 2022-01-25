open Plebeia
open Test_helper
open Cursor
open Node

let equal_check context n1 n2 =
  let res = Node_storage.Internal.equal_for_test context n1 n2 in
  match res with
  | Ok () -> ()
  | Error (n1',n2') ->
      prerr_endline "equality fails";
      Format.eprintf "DIFF@.  %a@.  %a@." Node_type.pp n1' Node_type.pp n2';
      Format.eprintf "ALL@.  %a@.  %a@." Node_type.pp n1 Node_type.pp n2;
      let n1 = Node_storage.read_node_fully ~reset_index:false context n1 in
      let n2 = Node_storage.read_node_fully ~reset_index:false context n2 in
      save_cursor_to_dot "debug_equal_check_1.dot" @@ _Cursor (_Top, n1, context, Info.empty);
      save_cursor_to_dot "debug_equal_check_2.dot" @@ _Cursor (_Top, n2, context, Info.empty);
      save_node_to_dot "debug_equal_check_detail1.dot" n1';
      save_node_to_dot "debug_equal_check_detail2.dot" n2';
      assert false

let commit_check c =
  let Cursor (_tr, n, context, _) as c, i, _ = from_Ok @@ Cursor_storage.write_top_cursor c in
  save_cursor_to_dot "commit_check.dot" c;
  let n' = View (Node_storage.read_node context i Not_Extender) in
  equal_check context n n';
  c

let test_segs_of_trail c seg =
  match access_gen c seg with
  | Ok (Reached (Cursor (trail, _, _, _), _v)) ->
      let path = path_of_trail trail in
      if not @@ Segment.equal (Segment.concat @@ Path.to_segments path) seg then begin
        failwith
          (Path.to_string path ^ " /= " ^ Segment.to_string seg)
      end
  | Ok (Middle_of_extender _) ->
      (* middle of extender *)
      dump_cursor c;
      assert false
  | Ok x ->
      let e = match error_access x with
        | Ok _ -> assert false
        | Error e -> e
      in
      dump_cursor c;
      Error.raise e

  | Error e ->
      (* no path ? *)
      dump_cursor c;
      Error.raise e

(* Add random leafs and subdirs to plebeia and dumb trees,
   checking the consistency between them.
   It returns, the final trees and a hashtbl of added leafs and subdirs.

   The input trees must be equivalent.
*)
let add_random rng sz c dumb =
  let bindings = Hashtbl.create 101 in

  let rec f c dumb i =
    if i = sz then (c, dumb)
    else
      let seg = Gen.(segment (int_range (3,13))) rng in
      let c, dumb =
        let s = Segment.to_string seg in
        let v = value (Segment.to_string seg ^ string_of_int (RS.int rng 5)) in

        (* get *)
        begin match get_value c seg, Dumb.get_value dumb seg with
          | Ok _, Ok _ -> ()
          | Error _, Error _ -> ()
          | _ -> assert false
        end;

        (* subtree *)
        begin match subtree c seg, Dumb.subtree dumb seg with
          | Ok _, Ok _ -> ()
          | Error _, Error _ -> ()
          | _ -> assert false
        end;

        (* removal (we do not accumulate) *)
        begin match
            delete c seg,
            Dumb.delete dumb seg
          with
          | Ok _, Ok _ -> ()
          | Error _, Error _ -> ()
          | Ok _, Error e ->
              Format.eprintf "dumb: %s (seg=%s)@." e s; assert false
          | Error e, Ok _ ->
              Format.eprintf "impl: %s (seg=%s)@." (Error.show e) s;
              Format.eprintf "%s@." @@ Debug.dot_of_cursor c;
              assert false
        end;

        match RS.int rng 3 with
        | 0 -> begin
            (* insert *)
            match
              insert c seg v,
              Dumb.insert dumb seg v
            with
            | Ok c, Ok dumb ->
                compare_trees dumb c;
                let Cursor (_, n, context, _) = c in
                (* check the invariants of the node *)
                Test_helper.validate_node context n;

                (* record the insertion *)
                Hashtbl.replace bindings (Segment.to_sides seg) (`Value v);
                test_segs_of_trail c seg;
                (c, dumb)
            | Error _, Error _ -> (c, dumb)
            | Ok _, Error e -> Format.eprintf "dumb: %s (seg=%s)@." e s; assert false
            | Error e, Ok _ ->
                Format.eprintf "impl: %s (seg=%s)@." (Error.show e) s;
                Format.eprintf "%s@." @@ Debug.dot_of_cursor c;
                assert false
          end
        | 1 -> begin
            (* upsert *)
            match
              upsert c seg v,
              Dumb.upsert dumb seg v
            with
            | Ok c, Ok dumb ->
                (* print_command (); *)
                compare_trees dumb c;
                let Cursor (_, n, context, _) = c in
                Test_helper.validate_node context n;
                Hashtbl.replace bindings (Segment.to_sides seg) (`Value v);
                test_segs_of_trail c seg;
                (c, dumb)
            | Error _, Error _ -> (c, dumb)
            | Ok _, Error e -> Format.eprintf "dumb: %s (seg=%s)@." e s; assert false
            | Error e, Ok _ ->
                Format.eprintf "impl: %s (seg=%s)@." (Error.show e) s;
                Format.eprintf "%s@." @@ Debug.dot_of_cursor c;
                assert false
          end
        | 2 -> begin
            (* create_subtree *)
            match
              create_subtree c seg,
              Dumb.create_subtree dumb seg
            with
            | Ok c, Ok dumb ->
                (* print_command (); *)
                compare_trees dumb c;
                let Cursor (_, n, context, _) = c in
                Test_helper.validate_node context n;
                Hashtbl.replace bindings (Segment.to_sides seg) `Subtree;
                (c, dumb)
            | Error _, Error _ -> (c, dumb)
            | Ok _, Error e -> Format.eprintf "dumb: %s (seg=%s)@." e s; assert false
            | Error e, Ok _ ->
                Format.eprintf "impl: %s (seg=%s)@." (Error.show e) s;
                Format.eprintf "%s@." @@ Debug.dot_of_cursor c;
                assert false
          end
        | _ -> assert false
      in
      f c dumb (i+1)
  in
  let c, dumb = f c dumb 0 in
  to_file ~file:"random_insertions.dot" @@ Debug.dot_of_cursor c;
  Hashtbl.iter (fun ss x ->
      let seg = Segment.of_sides ss in
      match x with
      | `Value v -> assert (snd @@ from_Ok @@ get_value c seg = v)
      | `Subtree -> assert (match subtree c seg with Ok _ -> true | _ -> false)
    ) bindings;

  (* hash and commit *)
  let c = if RS.int rng 10 = 0 then let c, _ = Cursor.compute_hash c in c else c in
  let c = if RS.int rng 10 = 0 then let c = commit_check c in c else c in
  c, dumb, bindings

let do_random_insertions rng sz =
  with_cursor @@ fun c ->

  let dumb = Dumb.empty () in

  let c, dumb, bindings = add_random rng sz c dumb in

  (* traversal: visit the leaf and bud and check all are covered *)
  let bindings' = Hashtbl.copy bindings in

  let () = Cursor.fold ~init:() c (fun () c ->
      let Cursor (trail, _, _, _) = c in
      let _, v = Cursor.view c in
      match v with
      | Node.Leaf _ ->
          let s = match Path.to_segments @@ path_of_trail trail with [s] -> s | _ -> assert false in
          (* Format.eprintf "value seg: %s@." @@ Segment.to_string s; *)
          begin match Hashtbl.find_opt bindings' (Segment.to_sides s) with
            | Some `Value _ -> Hashtbl.remove bindings' (Segment.to_sides s)
            | _ -> assert false
          end;
          `Continue, ()
      | Bud _ ->
          begin match Path.to_segments @@ path_of_trail trail with
            | [] -> ()
            | [s] ->
                begin
                  (* Format.eprintf "subtree seg: %s@." @@ Segment.to_string s; *)
                  match Hashtbl.find_opt bindings' (Segment.to_sides s) with
                  | Some `Subtree -> Hashtbl.remove bindings' (Segment.to_sides s)
                  | _ -> assert false
                end
            | _ -> assert false
          end;
          `Continue, ()
      | _ -> `Continue, ())
  in
  assert (Hashtbl.fold (fun k v acc -> (k,v)::acc) bindings' [] = []);

  (* deletions to the empty *)
  let bindings = Gen.shuffle (Hashtbl.fold (fun k v st -> (k,v)::st) bindings []) rng in
  let Cursor (_, n, context, _), _ =
    List.fold_left (fun (c, dumb) (ss, _) ->
        let seg = Segment.of_sides ss in
        let Cursor (_, n, context, _) as c = match delete c seg with
          | Ok c ->
              (* hash and commit *)
              let c = if RS.int rng 10 = 0 then let c, _ = Cursor.compute_hash c in c else c in
              let c = if RS.int rng 10 = 0 then let c = commit_check c in c else c in
              c
          | Error e ->
              to_file ~file:"deletion.dot" @@ Debug.dot_of_cursor c;
              Error.raise e
        in
        let dumb = from_Ok @@ Dumb.delete dumb seg in
        assert (Dumb.get_node dumb = Dumb.of_plebeia_node context n);
        Test_helper.validate_node context n;
        (c, dumb)) (c, dumb) bindings
  in
  (* once committed, the top node becomes Disk *)
  match Node_storage.read_node_fully ~reset_index:false context n with
  | View (Bud (None, _, _)) as n ->
      snd @@ Node.compute_hash context n
  | _ -> assert false
