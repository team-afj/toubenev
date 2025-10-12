let () = print_newline ()
let () = Py.initialize ~library_name:"/usr/local/lib/libpython3.13.dylib" ()

let _activate =
  let runpy = Py.import "runpy" in
  let site = Py.import "site" in
  let run_path = Py.Module.get_function runpy "run_path" in
  let addsitedir = Py.Module.get_function site "addsitedir" in
  ignore @@ run_path [| Py.String.of_string ".venv/bin/activate_this.py" |];
  addsitedir [| Py.String.of_string "." |]

let model = Py.import "lbc24"
let () = Py.Object.print model (Py.Channel stdout)
