open Lwt.Syntax

let create () =
  let open From_irmin.Binary in
  let tree = Tree.empty () in
  let* ctxt = persist_tree tree in
  Lwt.return ctxt

let produce_proof ctxt ops =
  let open From_irmin.Binary in
  let* proof =
    proof_of_ops ctxt.repo (`Node (Tree.hash ctxt.tree)) ops
  in
    Lwt.return proof

let get_start_hash proof =
  let open From_irmin.Binary in
  let kh = Tree.Proof.before proof in
  Tree.Proof.hash_to_hex kh
