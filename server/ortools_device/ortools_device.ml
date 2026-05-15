module Bqueue = Flux.Bqueue

type response_queue =
  (Data_repr.Api.answer, Data_repr.Api.answer option) Bqueue.t

type task = unit -> unit

(* We need to have all the actual tasks created by the device's promise, not the
   request promises, which are short-lived. Request promises fill the
   [task_queue], and the [task_laucher] runs them from the device's promise. *)
(* TODO Handle cancellation ! *)
type t = {
  tasks : (string, response_queue) Hashtbl.t;
  task_queue : (task, task option) Bqueue.t;
  task_launcher : unit Miou.t;
}

let new_optim t (p : Data_repr.Rich.Planning.t) =
  let handle = new_random_uuid_v4 () |> Uuidm.to_string in
  let queue =
    (* TODO This can block if there is not enogh readers... an unbounded queue
       would be a better fit. *)
    Bqueue.(create with_close 1000)
  in
  let task =
   fun () ->
    let context = Cp_model.Model.make ~with_assumptions:false p in
    let parameters =
      Ortools.Sat_parameters.make_sat_parameters ~log_search_progress:false
        ~num_workers:8l ()
    in
    let observer response =
      let date = now ~tz:p.infos.timezone () in
      let answer = Cp_model.Context.prepare_answer date context response in
      Bqueue.put queue answer
    in
    let response =
      Ortools_solvers.Sat.solve ~observer ~parameters context.model
    in
    let date = now ~tz:p.infos.timezone () in
    let answer = Cp_model.Context.prepare_answer date context response in
    Bqueue.put queue answer;
    Bqueue.close queue
  in
  Bqueue.put t.task_queue task;
  Hashtbl.add t.tasks handle queue;
  handle

let get_queue t handle = Hashtbl.find_opt t.tasks handle

let cancel t handle =
  (* TODO: actually cancel the promise ! *)
  Hashtbl.remove t.tasks handle

let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) ->
      Miou.await_exn prm;
      clean_up orphans

let v =
  let finally (t : t) = Miou.await_exn t.task_launcher in
  Vif.Device.v ~name:"ortools" ~finally [] @@ fun () ->
  let task_queue : (task, task option) Bqueue.t =
    Bqueue.(create with_close 256)
  in
  let task_source = Flux.Source.bqueue task_queue in
  let task_launcher =
    Miou.async (fun () ->
        let orphans = Miou.orphans () in
        Flux.Source.each (fun f -> ignore (Miou.call ~orphans f)) task_source;
        clean_up orphans)
  in
  { tasks = Hashtbl.create 16; task_queue; task_launcher }
