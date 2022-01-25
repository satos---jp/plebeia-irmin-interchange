open Plebeia
open Test_helper

module Int64 = Stdint.Int64

let make ~max_bytes_per_array size =
  Mmap.Arch32.init ~max_bytes_per_array size
    Int64.(fun i64 -> char_of_int @@ to_int @@ rem i64 256L)

let check_get_buffer buf off len =
  let cells = Mmap.Arch32.get_buffer ~off ~len buf in
  let wlen = len / 3 in
  Mmap.Arch32.Buffer.write_string (String.make wlen 'a') cells (len / 3);
  for i = 0 to Mmap.Arch32.Buffer.len cells - 1 do
    let expected = Mmap.Arch32.get_char buf Int64.(off + of_int i) in
    let actual = Mmap.Arch32.Buffer.get_char cells i in
    if expected <> actual then
      let () = Format.eprintf "check_get_buffer Error: expected=%C, actual=%C\n" expected actual in
      assert false
  done

let random_check_get_buffer st buf off len =
  let cells = Mmap.Arch32.get_buffer ~off ~len buf in
  for _ = 1 to 100000 do
    let i = RS.int st (Mmap.Arch32.Buffer.len cells) in
    let expected = Mmap.Arch32.get_char buf Int64.(off + of_int i) in
    let actual = Mmap.Arch32.Buffer.get_char cells i in
    if expected <> actual then
      let () = Format.eprintf "check_get_buffer Error: expected=%C, actual=%C\n" expected actual in
      assert false
  done

let random_get_buffer st =
  let max_bytes_per_array = 1000_000 in
  let size = RS.int64 st Int64.(4L * of_int max_bytes_per_array) in
  let buf = make ~max_bytes_per_array size in
  let off = RS.int64 st size in
  let len = RS.int st (min max_bytes_per_array Int64.(to_int (size - off))) in
  let len = if len = 0 then 1 else len in
  random_check_get_buffer st buf off len


let random_check_hash_prefix st =
  let max_bytes_per_array = 1000_000 in
  let size = RS.int st (4 * max_bytes_per_array) in
  let len = min max_bytes_per_array size in
  let buf = make ~max_bytes_per_array (Int64.of_int size) in
  let buf = Mmap.Arch32.get_buffer ~off:0L ~len buf in
  let htb = Hashtbl.create 100 in

  let bytes = 28 in
  let get_hash_prefix buf pos = Hash.Prefix.of_string @@ Mmap.Arch32.Buffer.copy buf pos bytes in
  let set_hash_prefix buf off hp = Mmap.Arch32.Buffer.write_string (Hash.Prefix.to_string hp) buf off in

  for _ = 0 to 100 do
    let i = (RS.int st (len / 32)) * 32 in
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
