open Plebeia
open Test_helper
open Cursor
open Node

let commit_and_load c =
  let Cursor (tr, _n, context, info), i, _ = from_Ok @@ Cursor_storage.write_top_cursor c in
  let n = View (read_node context i Not_Extender) in
  _Cursor (tr, n, context, info)
