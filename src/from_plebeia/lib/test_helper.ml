(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Plebeia
include Debug
include Utils

module RS = Random.State

let temp_file body ext =
  (* [Filename.temp_file] CREATEs a file.  It has a security reason to do so,
     but we do not like it for tests. *)
  let fn = Filename.temp_file body ext in
  Unix.unlink fn;
  fn

let with_tempdir f =
  (* temp_file creats a file! *)
  let d = temp_file "plebeia_test" ".dir" in
  Format.eprintf "Using tempdir %s@." d;
  Unix.mkdir d 0o700;
  f d

let context_confs =
  let open Context in
  [ { hash_func= `Blake2B; bytes_per_hash= 28; bytes_per_cell= 32 }
  ; { hash_func= `Blake2B; bytes_per_hash= 28; bytes_per_cell= 36 }
  ; { hash_func= `Blake2B; bytes_per_hash= 32; bytes_per_cell= 36 }
  ; { hash_func= `Blake2B; bytes_per_hash= 32; bytes_per_cell= 40 }
  ; { hash_func= `Blake3;  bytes_per_hash= 28; bytes_per_cell= 32 }
  ]

let with_context_conf f =
  List.map (fun conf -> conf, f conf) context_confs

let with_context_conf_lwt f =
  Lwt_list.map_s (fun conf -> let%lwt r = f conf in Lwt.return (conf, r)) context_confs

let resize_step_bytes = 100_000 (* 100KiB *)

let with_context f =
  Lwt_list.map_s (fun ({Context.hash_func; bytes_per_hash; bytes_per_cell} as conf) ->
      Lwt_fmt.eprintf "Testing with %a@." Context.pp_config conf;%lwt
      let tempfile = temp_file "plebeia" ".context" in
      let%lwt context = Context.create ~hash_func ~bytes_per_hash ~bytes_per_cell tempfile ~resize_step_bytes in
      let res = f context in
      Context.close context;%lwt
      Lwt.return (conf, res)) context_confs

let with_context_lwt f =
  Lwt_list.map_s (fun ({Context.hash_func; bytes_per_hash; bytes_per_cell} as conf) ->
      Lwt_fmt.eprintf "Testing with %a@." Context.pp_config conf;%lwt
      let tempfile = temp_file "plebeia" ".context" in
      let%lwt context = Context.create ~hash_func ~bytes_per_hash ~bytes_per_cell tempfile ~resize_step_bytes in
      let%lwt res = f context in
      Context.close context;%lwt
      Lwt.return (conf, res)) context_confs

let with_memory_only_context f =
  Lwt_list.map_s (fun ({Context.hash_func; bytes_per_hash; bytes_per_cell} as conf) ->
      Lwt_fmt.eprintf "Testing with %a@." Context.pp_config conf;%lwt
      let context = Context.memory_only ~hash_func ~bytes_per_hash ~bytes_per_cell () in
      let res = f context in
      Context.close context;%lwt
      Lwt.return (conf, res)) context_confs

let with_vc ?prefix f =
  (* The file created by [Filename.temp_file] to avoid file name crashes *)
  Lwt_list.map_s (fun ({Context.hash_func; bytes_per_hash; bytes_per_cell} as conf) ->
      Lwt_fmt.eprintf "Testing with %a@." Context.pp_config conf;%lwt
      let prefix =
        match prefix with
        | None -> Filename.temp_file "plebeia" "vc"
        | Some p -> Printf.sprintf "%s-%s" p (Context.config_name conf)
      in
      Lwt_fmt.eprintf "Using VC %s@." prefix;%lwt
      let%lwt vc = Vc.create ~bytes_per_cell ~hash_func ~bytes_per_hash ~resize_step_bytes prefix in
      let res = f vc in
      from_Ok_lwt @@ Vc.close vc;%lwt
      Lwt.return (conf, res)) context_confs

let with_cursor f =
  with_context (fun context ->
    let cursor = Cursor.empty context in
    f cursor)

let with_memory_only_cursor f =
  with_memory_only_context (fun context ->
    let cursor = Cursor.empty context in
    f cursor)

(* XXX This is very fragile.  If the test returns () by mistake
   we cannot detect any change... *)
let regression_by_hash name expected actual =
  let h = Hashtbl.hash actual in
  (* Some hashes are bad, indicating that this function was used for unit types *)
  match h with
  | 129913994 ->
      failwithf "%s: Regression error: returning ()?" name;
  | 609990030 ->
      (* [with_context @@ fun _ -> ()] *)
      failwithf "%s: Regression error: returning () with [with_context]?" name;
  | _ ->
      if expected <> h then
        failwithf "%s: Regression error: obtained %d" name h

let path_of_string s = from_Some @@ Segment.of_string s

let ok_or_fail = function
  | Ok x -> x
  | Error s -> Error.raise s

let must_fail = function
  | Ok _ -> failwith "must fail"
  | Error _ -> ()

let path = path_of_string
let value = Value.of_string

open Node

(* Normalize Segments *)
let rec normalize ~clear_hash n = match n with
  | Hash _ -> n
  | Disk _ -> n
  | View v -> View (normalize_view ~clear_hash v)

and normalize_view ~clear_hash v = match v with
  | Internal (n1, n2, i, h) ->
      let h = if clear_hash then Not_Hashed else h in
      _Internal (normalize ~clear_hash n1, normalize ~clear_hash n2, i, h)
  | Bud (None, i, h) ->
      let h = if clear_hash then Not_Hashed else h in
      _Bud (None, i, h)
  | Bud (Some n, i, h) ->
      let h = if clear_hash then Not_Hashed else h in
      _Bud (Some (normalize ~clear_hash n), i, h)
  | Leaf (a, b, h) ->
      let h = if clear_hash then Not_Hashed else h in
      _Leaf (a, b, h)
  | Extender (seg, n, i, h) ->
      let h = if clear_hash then Not_Hashed else h in
      _Extender (Segment.normalize seg, normalize ~clear_hash n, i, h)

let equal_nodes ~ignore_hash n1 n2 =
  let clear_hash = ignore_hash in
  normalize ~clear_hash n1 = normalize ~clear_hash n2

(* ls . *)
let all_children context node =
  let rec aux store = function
    | [] -> store
    | (segs_rev, node) :: tail ->
       match Node_storage.view context node with
       | Leaf _ ->
          let segment = List.rev segs_rev |> Segment.concat in
          aux ((segment, `File) :: store) tail
       | Bud (Some child, _, _) ->
          let segment = List.rev segs_rev |> Segment.concat in
          aux ((segment, `Directory child) :: store) tail
       | Bud (None, _, _) -> aux store tail
       | Extender (seg, node', _, _) ->
          aux store ((seg::segs_rev, node') :: tail)
       | Internal (l, r, _, _) ->
          aux store (
              (Segment.of_sides [Segment.Left] :: segs_rev, l)
              :: (Segment.of_sides [Segment.Right] :: segs_rev, r)
              :: tail)
  in
  aux [] [([], node)]

let xassert b =
  let open Printexc in
  if not b then begin
    prerr_endline ("*****************************************************\n" ^ raw_backtrace_to_string (get_callstack 10));
    assert false
  end

let reraise_after f x e =
  try
    f x
  with
    exn -> e exn; raise exn

let reraise_after_lwt (f : 'a -> 'b Lwt.t) (x : 'a) e =
  try%lwt
    f x
  with
    exn -> e exn; raise exn

let with_random f =
  let seed =
    let default () =
      let st_seed = RS.make_self_init() in
      RS.int st_seed @@ (1 lsl 30) - 1
    in
    match Sys.getenv "PLEBEIA_TEST_SEED" with
    | exception Not_found -> default ()
    | s ->
        try
          let i = int_of_string s in
          Format.eprintf "PLEBEIA_TEST_SEED=%d@." i;
          i
        with _ -> default ()
  in
  let st = RS.make [| seed |] in
  reraise_after f st @@ fun _exn ->
    Format.eprintf "Failed with seed: PLEBEIA_TEST_SEED=%d@." seed

let with_random_lwt f =
  let seed =
    let default () =
      let st_seed = RS.make_self_init() in
      RS.int st_seed @@ (1 lsl 30) - 1
    in
    match Sys.getenv "PLEBEIA_TEST_SEED" with
    | exception Not_found -> default ()
    | s ->
        try
          let i = int_of_string s in
          Format.eprintf "PLEBEIA_TEST_SEED=%d@." i;
          i
        with _ -> default ()
  in
  let st = RS.make [| seed |] in
  reraise_after_lwt f st @@ fun _exn ->
    Format.eprintf "Failed with seed: Random.State.make [| %d |]@." seed

(* XXX This STILL may break the invaraint *)
let forget_random_nodes rs prob n =
  let rec f n = match n with
    | Hash _ -> n
    | Disk _ -> n
    | View v ->
        if RS.float rs 1.0 < prob then
          match Node.may_forget n with
          | None -> n
          | Some n -> n
        else
          match v with
          | Leaf _ -> n
          | Bud (None, _, _) -> n
          | Bud (Some n', i, h) ->
              let n' = f n' in
              begin match _Bud (Some n', i, h) with
                | exception _ -> n
                | v -> View v
                end
          | Extender (seg, n', i, h) ->
              let n' = f n' in
              begin match _Extender (seg, n', i, h) with
                | exception _ -> n
                | v -> View v
                end
          | Internal (nl, nr, i, h) ->
              let nl' = f nl in
              let nr' = f nr in
              begin match _Internal (nl', nr', i, h) with
                | exception _ -> n
                | v -> View v
              end
    in
    f n

let leaves context node : Segment.t list list =
  let rec aux store = function
    | [] -> store
    | (pathes_rev, node) :: tail ->
       all_children context node
       |> List.map (function
              | (seg, `File) -> List.rev (seg :: pathes_rev) :: store
              | (seg, `Directory child) when Segment.is_empty seg ->
                 aux store ((pathes_rev, child) :: tail)
              | (seg, `Directory child) ->
                 aux store ((seg :: pathes_rev, child) :: tail))
       |> List.concat
  in
  aux [] [([], node)]

let choose_random_node stop_prob st ctxt n0 =
  let open Segment in
  let rec f segs n =
    let v = Node_storage.view ctxt n in
    if RS.float st 1.0 < stop_prob then segs
    else
      match v with
      | Leaf _ | Bud (None, _, _) -> f Segs.empty n0
      | Bud (Some n, _, _) -> f (Segs.push_bud segs) n
      | Internal (n1, n2, _, _) ->
          if RS.bool st then f (Segs.add_side segs Left) n1
          else f (Segs.add_side segs Right) n2
      | Extender (seg, n, _, _) ->
          f (Segs.append_seg segs seg) n
  in
  f Segs.empty n0

let make_resemble st ctxt n =
  let c =
    match Node_tools.count_nodes ~upto:1000 n with
    | `EQ i | `GE i -> i
  in
  let open Segment in

  (* remove some nodes *)
  let rec rm i n =
    if i = 0 then n
    else
      let (`EQ c | `GE c) = Node_tools.count_nodes ~upto:1000 n in
      let segs = choose_random_node 0.05 st ctxt n in
      let segs = Segs.finalize segs in
      if segs = [] then rm (i-1) n
      else
        let cur = Cursor.(_Cursor (_Top, n, ctxt, Info.empty)) in
        match Deep.delete' cur segs  with
        | Ok Cursor (_, n', _, _) ->
            let (`EQ c' | `GE c') = Node_tools.count_nodes ~upto:1000 n' in
            if c' * 2 < c then rm (i-1) n (* we do not remove too much *)
            else rm (i-1) n'
        | Error _ -> rm (i-1) n
  in
  let n = rm (if c <= 1000 then 2 else 4) n in

(*
  let (`EQ c' | `GE c') = Node_tools.count_nodes ~upto:1000 n in
  Format.eprintf "Resemble del: %d => %d@." c c';
*)

  let (`EQ c | `GE c) = Node_tools.count_nodes ~upto:1000 n in

  (* replace some nodes *)
  let rec add i n =
    if i = 0 then n
    else
      (* XXX segment selection is not really good *)
      let segs = choose_random_node 0.05 st ctxt n in
      let segs = Segs.append_seg segs @@ Gen.(segment (int 5)) st in
      let m =
        let open Gen in
        one_of
          [gen_leaf; gen_bud 8; gen_internal 8; gen_extender 8]
          st
      in
      let segs = Segs.finalize segs in
      match
        Deep.alter Cursor.(_Cursor (_Top, n, ctxt, Info.empty)) segs @@ function
        | Some _ -> Ok m
        | None ->
            (* This is possible.  If [segs] can point an empty bud,
               alter detects None there.
            *)
            Ok m
      with
      | exception _ ->
          (* This is arbitrary update therefore may break the invariant *)
          add (i-1) n
      | Ok (Cursor (_, n', _, _)) ->
          let (`EQ c' | `GE c') = Node_tools.count_nodes ~upto:1000 n' in
          if c' < c then add (i-1) n (* not to make the tree too small *)
          else add (i-1) n'
      | Error _ -> add (i-1) n
  in
  let n = add 32 n in

(*
  let (`EQ c' | `GE c') = Node_tools.count_nodes ~upto:1000 n in
  Format.eprintf "Resemble: => %d@." c';
*)

  n

module Dumb = Dumb

let copy_file fn fn' =
  let%lwt fd = Lwt_unix.openfile fn [O_RDONLY] 0o644 in
  let%lwt fd' = Lwt_unix.openfile fn' [O_CREAT; O_TRUNC; O_WRONLY] 0o644 in
  let buf = Bytes.create 4096 in
  let rec loop () =
    let%lwt nreads = Lwt_unix.read fd buf 0 4096 in
    if nreads = 0 then Lwt.return_unit
    else begin
      let%lwt nwrites = Lwt_unix.write fd' buf 0 nreads in
      assert (nreads = nwrites);
      loop ()
    end
  in
  loop ();%lwt
  Lwt_unix.close fd;%lwt
  Lwt_unix.close fd'

let count_nodes ctxt n =
  let cntr = ref 0 in
  Traverse.Iter.iter (fun t ->
      incr cntr;
      `Continue (Node_storage.view ctxt t)) n;
  !cntr

(* generate a node with enough depth and size *)
let gen_node ctxt rng ?min_nodes depth =
  match min_nodes with
  | None ->
      Node_type.gen_bud depth rng
  | Some min_nodes ->
      let rec aux (max_ns, m) = function
        | 0 ->
            Format.eprintf "Warning: could not build a node with size >= %d@." min_nodes;
            m
        | i ->
            let n = Node_type.gen_bud depth rng in
            let ns = count_nodes ctxt n in
            if min_nodes <= ns then begin
              Format.eprintf "Build a node with size %d@." ns;
              n
            end else
              let max_ns, m =
                if max_ns < ns then ns, n
                else max_ns, m
              in
              aux (max_ns, m) (i-1)
      in
      aux (1, Node_type.gen_leaf rng) 100

let validate_node ctxt n =
  Result.default (Debug.validate_node ctxt n) (fun e ->
      to_file ~file:"invalid.dot" @@ Debug.dot_of_node n;
      prerr_endline "Saved the current node to invalid.dot";
      failwith e)

let check_cursor_is_top c =
  match c with
  | Cursor.Cursor (Top, _, _, _) -> ()
  | _ -> xassert false

(* Pick an existing random segment points at Leaf or Bud *)
let random_segs_to_bud_or_leaf rng c =
  let open Cursor in
  let Cursor (_, n, context, _) = c in
  match Node_storage.view context n with
  | Internal _ | Extender _ | Leaf _ -> assert false
  | Bud (None, _, _) -> None (* unremovable *)
  | Bud (Some n, _, _) ->
      let rec choose_random_segs rev_segs rev_seg n =
        let v = Node_storage.view context n in
        match v with
        | Leaf _ -> List.rev (List.rev rev_seg :: rev_segs)
        | Bud (None, _, _) -> List.rev (List.rev rev_seg :: rev_segs)
        | Bud (Some n, _, _) ->
            let rev_segs = List.rev rev_seg :: rev_segs in
            if RS.int rng 2 = 0 then List.rev rev_segs
            else choose_random_segs rev_segs [] n
        | Internal (n1, n2, _, _) ->
            if RS.int rng 2 = 0 then
              let rev_seg = Segment.Left :: rev_seg in
              choose_random_segs rev_segs rev_seg n1
            else
              let rev_seg = Segment.Right :: rev_seg in
              choose_random_segs rev_segs rev_seg n2
        | Extender (seg, n, _, _) ->
            let rev_seg = List.rev_append (Segment.to_sides seg) rev_seg in
            choose_random_segs rev_segs rev_seg n
      in
      Some (List.map Segment.of_sides @@ choose_random_segs [] [] n)

let dump_cursor c =
  let Cursor.Cursor (_, n, context, _) = c in
  to_file ~file:"plebeia.dot" @@ Debug.dot_of_cursor c;
  to_file ~file:"plebeia_dumb.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n

(* compare the entire tree of Dumb and Cursor *)
let compare_trees dumb (Cursor.Cursor (_, n, context, _) as c) =
  if Dumb.get_node dumb <> Dumb.of_plebeia_node context n then begin
    to_file ~file:"dumb.dot" @@ Dumb.dot_of_cursor dumb;
    dump_cursor c;
    begin match Cursor.go_top c with
      | Error _ -> prerr_endline "no root dump"
      | Ok (Cursor (_, n, context, _)) ->
          to_file ~file:"plebeia_dumb_root.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n;
    end;
    assert false
  end

let gen_segs (a,b) c =
  Gen.(list (int_range (a,b)) (segment @@ return c))

(* Prepare a tree with path_length, nfiles, and vsize

   If path_length is too small for nfiles, the function never terminates.
*)
let prepare_tree rs c ~path_bits ~nfiles ~vbytes =
  let rec loop c segs = function
    | 0 -> c, segs
    | nfiles ->
        let seg = Gen.(segment (return path_bits)) rs in
        let v = Value.of_string @@ Gen.(string (return vbytes) char) rs in
        match Cursor.insert c seg v with
        | Error _ -> loop c segs nfiles
        | Ok c -> loop c (seg::segs) (nfiles - 1)
  in
  loop c [] nfiles

(* choose a random terminal node (Leaf or empty Bud) of [n]
   and returns its segments *)
let random_terminal ctxt n rng =
  begin match Node_storage.view ctxt n with
    | Bud _ -> ()
    | _ -> invalid_arg "node must be a Bud"
  end;
  let rec f segs n =
    match Node_storage.view ctxt n with
    | Leaf _ | Bud (None, _, _) ->
        Segment.Segs.finalize segs, n
    | Bud (Some n, _, _) ->
        f (Segment.Segs.push_bud segs) n
    | Internal (l, r, _, _) ->
        begin match Gen.bool rng with
        | true -> f (Segment.Segs.add_side segs Left) l
        | false -> f (Segment.Segs.add_side segs Right) r
        end
    | Extender (seg, n, _, _) ->
        f (Segment.Segs.append_seg segs seg) n
  in
  f Segment.Segs.empty n

let ignore_lwt v =
  Lwt.bind v (fun _ -> Lwt.return_unit)
  (* Lwt.map ignore *)

let run_lwt = Lwt_main.run

let run_ignore_lwt v =
  run_lwt (ignore_lwt v)

let exec_lwt f () = Lwt_main.run @@ f ()
