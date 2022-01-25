open Plebeia
open Test_helper
open Cursor
open Node

module Debug = Debug

(* We cannot compare with Dumb *)

let gen_segs = gen_segs (1,5) 3

let do_random st sz c =
  let rev_ops = ref [] in
  let add_op o = rev_ops := o :: !rev_ops in
  let rec f c i =
    if i = sz then c
    else
      let c =
        let rec get_op () = match RS.int st 8 with
          | 0 -> `Insert (gen_segs st, Gen.value st)
          | 1 -> `Upsert (gen_segs st, Gen.value st)
          | 2 -> `Subtree (gen_segs st)
          | 3 -> `Commit
          | 4 ->
              begin match random_segs_to_bud_or_leaf st c with
                | None -> get_op ()
                | Some segs ->
                    let segs' = gen_segs st in
                    `Copy (segs, segs')
              end
          | _ ->
              match random_segs_to_bud_or_leaf st c with
              | None -> get_op ()
              | Some segs -> `Delete segs
        in
        let op = get_op () in
        match op with
        | `Insert (segs, v) ->
            (* Format.eprintf "Insert at %s@." @@ string_of_segs segs; *)
            begin match Deep.insert c segs v with
              | Ok c ->
                  add_op op;
                  check_cursor_is_top c;
                  c
              | Error _ -> c
            end
        | `Upsert (segs, v) ->
            (* Format.eprintf "Upsert at %s@." @@ string_of_segs segs; *)
            begin match Deep.upsert c segs v with
              | Ok c ->
                  add_op op;
                  check_cursor_is_top c;
                  c
              | Error _ -> c
            end
        | `Subtree segs ->
            (* Format.eprintf "Create_subtree at %s@." @@ string_of_segs segs; *)
            begin match Deep.create_subtree ~create_subtrees:true c segs with
              | Ok c ->
                  add_op op;
                  check_cursor_is_top c;
                  c
              | Error _ -> c
            end
        | `Delete segs ->
            (* Format.eprintf "Delete at %s@." @@ string_of_segs segs; *)
            begin match
                Deep.delete c segs
              with
              | Ok c ->
                  add_op op;
                  check_cursor_is_top c;
                  c
              | Error _ -> c
            end
        | `Copy (segs, segs') ->
            begin match Deep.copy ~create_subtrees:true c segs segs' with
              | Ok c ->
                  add_op op;
                  check_cursor_is_top c;
                  c
              | Error _ -> c
            end
        | `Commit ->
            let Cursor(_, _, context, info), i, _ = from_Ok @@ Cursor_storage.write_top_cursor c in
            let v = Node_storage.read_node context i Not_Extender in
            add_op op;
            _Cursor (_Top, View v, context, info)
            in
            f c (i+1)
        in
        let c = f c 0 in
        (c, List.rev !rev_ops)
