(* node storage test *)
open Plebeia
open Test_helper
open Debug
open Node

let set_uint32 buf x v = Cstruct.LE.set_uint32 buf x @@ Stdint.Uint32.to_int32 v
let set_index buf x v = set_uint32 buf x @@ Index.to_uint32 v

let write_string s buf off len =
  let slen = String.length s in
  if slen <> len then begin Format.eprintf "write_string: %d <> %d@." slen len; assert false end;
  Cstruct.blit_from_string s 0 buf off len

let parse_test context n =
  (* reload the node and compare *)
  try
    let n' = View (Node_storage.Internal.parse_cell context @@ from_Some @@ index n) in
    let n' = Node_storage.read_node_fully ~reset_index:false context n' in
    if not (equal_nodes ~ignore_hash:true n n') then begin
      prerr_endline "input:";
      prerr_endline @@ string_of_node n 2;
      prerr_endline "reparsed:";
      prerr_endline @@ string_of_node n' 2;
      assert false
    end
  with
  | e ->
      prerr_endline "Failed for";
      prerr_endline @@ string_of_node n 2;
      raise e

let random_write_parse st =
  ignore_lwt @@ with_cursor @@ fun (Cursor (_, _, context, _)) ->

  match RS.int st 4 with
  | 0 (* leaf *) ->
      let v = Gen.value st in
      let n = new_leaf v in
      let n, _i, _h = from_Ok @@ Node_storage.write_node ~clear:false context n in
      parse_test context n

  | 1 (* bud *) ->
      if RS.bool st then
        let n = new_bud None in
        let n, _, _ = from_Ok @@ Node_storage.write_node ~clear:false context n in
        parse_test context n
      else
        let v = Gen.value st in
        let n = new_bud (Some (new_extender (path "L") (new_leaf v))) in
        let n, _, _ = from_Ok @@ Node_storage.write_node ~clear:false context n in
        parse_test context n

  | 2 (* internal *) ->
      let right_referred = RS.bool st in
      let n1, _, _ =
        from_Ok @@ Node_storage.write_node ~clear:false context @@
        new_leaf @@ Gen.value st
      in
      let n2 =
        new_leaf @@ Gen.value st
      in
      let n =
        if right_referred then
          new_internal n2 n1
        else
          new_internal n1 n2
      in
      let n, _, _ = from_Ok @@ Node_storage.write_node ~clear:false context n in
      parse_test context n

  | 3 (* extender *) ->
      let seg = Gen.(segment @@ int Segment.max_length) st in (* may be a big segment! *)
      let n' = new_leaf @@ Gen.value st in
      let n = new_extender seg n' in
      let n, _, _ = from_Ok @@ Node_storage.write_node ~clear:false context n in
      parse_test context n

  | _ -> assert false
