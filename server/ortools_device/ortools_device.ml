type response_queue =
  (Ortools.Sat.Response.t, Ortools.Sat.Response.t option) Flux.Bqueue.t

type t = {
  tasks : (string, response_queue * unit Miou.t) Hashtbl.t;
  orphans : unit Miou.orphans;
}

let new_optim t (p : Data_repr.Rich.Planning.t) =
  let handle = new_random_uuid_v4 () |> Uuidm.to_string in
  let queue =
    (* TODO This can block if there is not enogh readers... an unbounded queue
       would be a better fit. *)
    Flux.Bqueue.(create with_close 1000)
  in
  let promise =
    Miou.call ~orphans:t.orphans @@ fun () ->
    let context = Cp_model.Model.make ~with_assumptions:false p in
    let parameters =
      Ortools.Sat_parameters.make_sat_parameters ~log_search_progress:false
        ~num_workers:8l ()
    in
    let observer response = Flux.Bqueue.put queue response in
    let response =
      Ortools_solvers.Sat.solve ~observer ~parameters context.model
    in
    Flux.Bqueue.put queue response;
    Flux.Bqueue.close queue
  in
  Hashtbl.add t.tasks handle (queue, promise);
  handle

let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
      Miou.await_exn prm;
      clean_up orphans

let v =
  let finally (t : t) = clean_up t.orphans in
  Vif.Device.v ~name:"ortools" ~finally [] @@ fun () ->
  { tasks = Hashtbl.create 16; orphans = Miou.orphans () }
