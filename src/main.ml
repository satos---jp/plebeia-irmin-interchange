open Lwt.Syntax

module I = Irmin_wrapper
module P = Plebeia_wrapper

let test_ops ops =
	let* irmin_proof =
		let* ctxt = I.create () in
		I.produce_proof ctxt ops
	in
	let irmin_hash = I.get_start_hash irmin_proof in
	let* () = Lwt_io.printf  "irmin_hash :: %s\n" irmin_hash in

	let* plebeia_proof =
		let* ctxt = P.create () in
		P.produce_proof ctxt ops
	in

	let* plebeia_hash = P.get_start_hash plebeia_proof in
	let* () = Lwt_io.printf  "plebeia_hash :: %s\n" plebeia_hash in
	Lwt.return ()

let _ =
	Lwt_main.run (test_ops [])
