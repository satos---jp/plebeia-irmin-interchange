(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import
open Common

let root = Filename.concat "_build" "test-tree"

module Hash = Irmin.Hash.SHA1

type ('key, 'value) op =
  | Add of 'key * 'value
  | Del of 'key
  | Find of 'key
  | Find_tree of 'key

module Make (Conf : Irmin_pack.Conf.S) = struct
  module Store = struct
    module P = Irmin.Path.String_list
    module M = Irmin.Metadata.None
    module XNode = Irmin.Private.Node.Make
    module XCommit = Irmin.Private.Commit.Make

    include
      Irmin_pack.Make_ext (Irmin_pack.Version.V2) (Conf) (XNode) (XCommit) (M)
        (Irmin.Contents.String)
        (P)
        (Irmin.Branch.String)
        (Hash)
  end

  let config ?(readonly = false) ?(fresh = true) root =
    Irmin_pack.config ~readonly ?index_log_size ~fresh root

  let info () = Irmin.Info.empty

  module Tree = Store.Tree

  type context = { repo : Store.repo; tree : Store.tree }

  let persist_tree tree =
    let* repo = Store.Repo.v (config root) in
    let* store = Store.empty repo in
    let* () = Store.set_tree_exn ~info store [] tree in
    let+ tree = Store.tree store in
    { repo; tree }

  let close { repo; _ } = Store.Repo.close repo

  let fold ~order t ~init ~f =
    Tree.fold ~order ~force:`True ~cache:false ~uniq:`False
      ~contents:(fun k _v acc -> if k = [] then Lwt.return acc else f k acc)
      t init

  let init_bindings n =
    let zero = String.make 10 '0' in
    List.init n (fun n ->
        let h = Store.Contents.hash (string_of_int n) in
        let h = Irmin.Type.to_string Store.Hash.t h in
        ([ h ], zero))

  let init_tree bindings =
    let tree = Tree.empty () in
    let* tree =
      Lwt_list.fold_left_s (fun tree (k, v) -> Tree.add tree k v) tree bindings
    in
    persist_tree tree

  let find_tree tree k =
    let+ t = Tree.find_tree tree k in
    match t with None -> tree | Some t -> t

  let find tree k =
    let+ _ = Tree.find tree k in
    tree

  let run_one tree = function
    | Add (k, v) -> Tree.add tree k v
    | Del k -> Tree.remove tree k
    | Find k -> find tree k
    | Find_tree k -> find_tree tree k

  let run ops tree =
    let+ t = Lwt_list.fold_left_s run_one tree ops in
    (t, ())

  let proof_of_ops repo hash ops : _ Lwt.t =
    let+ t, () = Store.Tree.produce_proof repo hash (run ops) in
    t

  let tree_proof_t = Tree.Proof.t Tree.Proof.tree_t
  let bin_of_proof = Irmin.Type.(unstage (to_bin_string tree_proof_t))
  let proof_of_bin = Irmin.Type.(unstage (of_bin_string tree_proof_t))

  module Plebeia = Tree.Proof.Plebeia
end

(* module Default = Make (Conf) *)

module Binary = Make (struct
  let entries = 2
  let stable_hash = 2
  let inode_child_order = `Hash_bits
end)