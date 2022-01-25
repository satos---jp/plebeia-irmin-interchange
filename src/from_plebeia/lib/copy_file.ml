open Plebeia
open Test_helper

(* No branching *)
let build_file rng d {Context.bytes_per_cell; hash_func; bytes_per_hash} i =
  let%lwt vc = Vc.create ~bytes_per_cell ~hash_func ~bytes_per_hash (d ^/ "src") in
  let cur = Vc.empty_cursor vc in
  let rec loop parents (Cursor.Cursor (_, n, ctxt, _info) as _cur) = function
    | 0 -> Lwt.return_unit
    | i ->
        let parent, parents =
          (* genesis at 1/100 *)
          if Gen.int 100 rng = 0 then None, parents
          else
            match Gen.shuffle parents rng with
            | p::ps ->
                begin match Gen.int 20 rng with
                  | 0 | 1 -> Some p, p::ps (* branch at 1/10 *)
                  | 2 ->
                      (* kill branch at 1/20 *)
                      begin match ps with
                        | _::ps ->
                            Some p, ps
                        | [] -> Some p, []
                      end
                  | _ -> Some p, ps
                end
            | [] -> None, []
        in
        let n' = make_resemble rng ctxt n in
        let cur' = Cursor.(_Cursor (_Top, n', ctxt, Info.empty)) in
        let%lwt cur', _hp, commit =
          from_Ok_lwt @@
          Vc.commit
            vc
            ~parent
            ~hash_override: None
            cur'
        in
        let parents = commit.hash :: parents in
        loop parents cur' (i-1)
  in
  loop [] cur i;%lwt
  from_Ok_lwt @@ Vc.close vc

let copy_file rng d {Context.bytes_per_cell; hash_func; bytes_per_hash} =
  let%lwt vc_src = Vc.open_ ~mode:Storage.Reader ~bytes_per_cell ~hash_func ~bytes_per_hash (d ^/ "src") in
  let%lwt vc_dst = Vc.create ~bytes_per_cell ~hash_func ~bytes_per_hash (d ^/ "dst") in
  let%lwt commits = Commit_db.to_list @@ Vc.commit_db vc_src in

  (* intentinally remove away some (5%) commits *)
  let commits =
    List.filter (fun _ -> Gen.int 20 rng <> 0) commits
  in

  match%lwt Copy.copy vc_src commits vc_dst with
  | Error e ->
      Lwt_fmt.eprintf "ERROR: %a@." Error.pp e;%lwt
      Error.raise e
  | Ok _ -> Lwt.return_unit
