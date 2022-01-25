open Plebeia

let dump_hash ctxt n =
  let _, nh = Node.compute_hash ctxt n in
  Format.eprintf "%a@." Node.pp n;
  Format.eprintf "hash: %s@.@." (Hash.to_hex_string nh)
