open Plebeia
open Test_helper

let make size =
  Mmap.Arch64.init size
    (fun i64 -> char_of_int @@ i64 mod 256)

let random_check_hash_prefix st =
  let bytes = 28 in
  let max_bytes = 1000_000 in
  let size = RS.int st (4 * max_bytes) in
  let buf = make size in
  let buf = Mmap.Arch64.get_buffer ~off:0L ~len:size buf in
  let htb = Hashtbl.create 100 in

  let get_hash_prefix buf pos = Hash.Prefix.of_string @@ Mmap.Arch64.Buffer.copy buf pos bytes in
  let set_hash_prefix buf off hp = Mmap.Arch64.Buffer.write_string (Hash.Prefix.to_string hp) buf off in

  for _ = 0 to 100 do
    let i = (RS.int st (size / 32)) * 32 in
    let j = RS.int st (32 - bytes + 1) in
    let hp = Hash.Prefix.gen bytes st in
    set_hash_prefix buf (i + j) hp;
    Hashtbl.replace htb i (j, hp);
  done;
  Hashtbl.iter
  (fun i (j, hp) ->
    let hp' = get_hash_prefix buf (i + j) in
    assert (hp = hp')
  ) htb
