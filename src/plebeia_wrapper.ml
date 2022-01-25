open Lwt.Syntax

let hash_func= `Blake2B
let bytes_per_hash= 28
let bytes_per_cell= 32

let create () =
  let open Plebeia in
  let open Test_utils in
  let tmp = temp_file "plebeia" ".context" in
  let* context = Context.create ~hash_func ~bytes_per_hash ~bytes_per_cell tmp ~resize_step_bytes in
  Lwt.return context

let produce_proof ctxt _ops =
  let open Plebeia in
  let open Test_utils in
  let rng = RS.make [| 0 |] in
  let node = Node_type.gen_bud_none rng in
  let proof, _res =
  Merkle_proof.make ctxt node []
    in Lwt.return proof


let get_start_hash proof =
  let open Plebeia in
  let hasher = Hash.Hasher.make ~bytes_per_cell ~hash_func ~bytes_per_hash in
  let h,_ = Merkle_proof.Tree.compute_hash hasher proof.Merkle_proof.tree in
  let hex = Hash.to_hex_string h in
  Lwt.return hex