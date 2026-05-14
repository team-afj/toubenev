type store = (string, unit) Hashtbl.t

let v =
  let finally (_store : store) = (* empty the store *) () in
  Vif.Device.v ~name:"ortools" ~finally [] @@ fun () -> Hashtbl.create 16
