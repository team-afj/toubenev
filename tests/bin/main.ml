let () = print_newline ()
let () = Printf.eprintf "0\n%!"
let () = Py.initialize ~library_name:"/usr/local/lib/libpython3.13.dylib" ()

let _activate =
  Printf.eprintf "0\n%!";
  let runpy = Py.import "runpy" in
  let site = Py.import "site" in
  let run_path = Py.Module.get_function runpy "run_path" in
  let addsitedir = Py.Module.get_function site "addsitedir" in
  ignore @@ run_path [| Py.String.of_string ".venv/bin/activate_this.py" |];
  addsitedir [| Py.String.of_string "." |]

(* let model = Py.import "lbc24"
let () = Py.Object.print model (Py.Channel stdout) *)

let () =
  (* let volunteers =
    let v1 = { Types.Volunteer.name = "toto"; v = 4; friends = [] } in

    { Types.Volunteer.name = "tata"; v = 5; friends = [ v1 ] }
    |> Jsont_bytesrw.encode_string Types.Volunteer.intt_jsont
    |> Result.get_ok
  in *)
  (* let json_loads =
    let json = Py.import "json" in
    Py.Module.get_function json "loads"
  in *)
  (* let v = json_loads [| Py.String.of_string volunteers |] in *)
  (* let v = Py.String.of_string s in
  Py.Object.print v (Py.Channel stdout); *)
  Printf.eprintf "\n%!"

let () = Py.finalize ()

type sort = A | X [@key "B"] | C [@@deriving jsont]

type t = {
  name : string;
  maybe_parent : t option; [@option]
  ids : string list; [@default []] [@omit List.is_empty]
  sort : sort; [@key "Sort"]
}
[@@deriving jsont]

type v = A of int [@key "Id"] | S of sort [@@deriving jsont]

let _ =
  Printf.eprintf "\n%s\n"
    (Jsont_bytesrw.encode_string jsont
       {
         name = "Alice";
         maybe_parent =
           Some { name = "Bob"; maybe_parent = None; ids = [ "X" ]; sort = X };
         ids = [];
         sort = A;
       }
    |> Result.get_ok)

let _ =
  Printf.eprintf "\n%s\n"
    (Jsont_bytesrw.encode_string (Jsont.list v_jsont) [ S X; A 42 ]
    |> Result.get_ok)

let _ =
  let open Jsont_bytesrw in
  let v = [ S X; A 42 ] in
  let recode j t =
    encode_string j t |> Result.get_ok |> decode_string j |> Result.get_ok
  in
  assert (v = recode (Jsont.list v_jsont) v)
