open Plebeia
open Test_helper
open Cursor

module Debug = Debug

module Dumb = Dumb

let random_seg = Gen.(segment (int 3 >>| fun x -> x + 3))

let do_random st sz c dumb =
  let module Segs = Set.Make( struct type t = Segment.t let compare = compare end ) in
  let segs = ref Segs.empty in
  let add_seg seg = segs := Segs.add seg !segs in
  let del_seg seg = segs := Segs.remove seg !segs in
  let pick_seg st =
    let segs = Segs.elements !segs in
    let len = List.length segs in
    if len < 20 then random_seg st
    else
      let pos = RS.int st (List.length segs) in
      List.nth segs pos
  in
  let rev_ops = ref [] in
  let add_op o = rev_ops := o :: !rev_ops in
  let rec f c dumb i =
    if i = sz then (c, dumb)
    else
      let (c, dumb) =
        let op = match RS.int st 7 with
          | 0 -> `Insert (random_seg st, Gen.value st)
          | 1 -> `Upsert (random_seg st, Gen.value st)
          | 2 -> `Subtree (random_seg st)
          | _ -> `Delete (pick_seg st)
        in
        match op with
        | `Insert (seg, v) ->
            begin match
              insert c seg v,
              Dumb.insert dumb seg v
            with
            | Ok c, Ok dumb ->
                compare_trees dumb c;
                let Cursor (_, n, context, _) = c in
                (* check the invariants of the node *)
                validate_node context n;
                add_op op;
                add_seg seg;
                (c, dumb)
            | Error _, Error _ ->
                (c, dumb)
            | _ -> assert false
          end

        | `Upsert (seg, v) ->
            begin match
              upsert c seg v,
              Dumb.upsert dumb seg v
            with
            | Ok c, Ok dumb ->
                compare_trees dumb c;
                let Cursor (_, n, context, _) = c in
                (* check the invariants of the node *)
                validate_node context n;
                add_op op;
                add_seg seg;
                (c, dumb)
            | Error _, Error _ ->
                (c, dumb)
            | _ -> assert false
          end

        | `Subtree seg ->
            begin match
                create_subtree c seg,
                Dumb.create_subtree dumb seg
            with
            | Ok c, Ok dumb ->
                compare_trees dumb c;
                let Cursor (_, n, context, _) = c in
                (* check the invariants of the node *)
                validate_node context n;
                add_op op;
                add_seg seg;
                (c, dumb)
            | Error _, Error _ ->
                (c, dumb)
            | _ -> assert false
          end

        | `Delete seg ->
            begin match
                delete c seg,
                Dumb.delete dumb seg
            with
            | Ok c, Ok dumb ->
                compare_trees dumb c;
                let Cursor (_, n, context, _) = c in
                (* check the invariants of the node *)
                validate_node context n;
                add_op op;
                del_seg seg;
                (c, dumb)
            | Error _, Error _ ->
                (c, dumb)
            | _ -> assert false
          end

      in
      f c dumb (i+1)
  in
  let (c,_) = f c dumb 0 in
  (List.rev !rev_ops, Cursor_storage.read_fully ~reset_index:false c)
