open! Import
open! Brr
open! Brr_lwd

type checked = bool
type label = string -> Elwd.t Elwd.col
type 'value desc = Check of 'value * string * label * checked
(* TODO | Group of label * 'value desc list *)

type 'value group = { name : string; desc : 'value desc Lwd_seq.t Lwd.t }

type 'a checkbox = {
  element : Elwd.t Lwd.t;
  desc : 'a desc;
  var : 'a option Lwd.var;
}

type 'a reactive_field = {
  field : Elwd.t Lwd.t;
  all : 'a checkbox Lwd_seq.t Lwd.t;
  filtered : 'a checkbox Lwd_seq.t Lwd.t;
  value : ('a * 'a desc * 'a option Lwd.var) Lwd_seq.t Lwd.t;
}

let name ~g ~n base_name = Printf.sprintf "%s-%i-%i" base_name g n

let make_single ?persist ?(ev = []) ?(on_change = fun _ -> ()) ?var name value
    label checked =
  let id = Format.sprintf "%s-id" name in
  let result checked = if checked then Some value else None in
  let var =
    match var with
    | Some var -> var
    | None -> (
        match persist with
        | Some true -> Persistent.var ~key:id (result checked)
        | Some false | None -> Lwd.var (result checked))
  in
  let lbl = Elwd.label ~at:[ `P (At.for' (Jstr.v id)) ] label in
  let at =
    let open Attrs in
    add At.Name.id (`P id) []
    |> add At.Name.name (`P name)
    |> add At.Name.type' (`P "checkbox")
  in
  let checked =
    (* todo: this is not controllable: unchecking does not uncheck without calling the JV function*)
    Lwd.map (Lwd.get var) ~f:(function
      | Some _ -> At.checked
      | None -> At.void)
  in
  let at = `R checked :: at in
  let on_change =
    Elwd.handler Ev.change (fun ev ->
        let t = Ev.target ev |> Ev.target_to_jv in
        let checked = Jv.get t "checked" in
        let result = result (Jv.to_bool checked) in
        on_change result;
        Lwd.set var result)
  in
  let ev = `P on_change :: ev in
  let element = ref None in
  let field =
    Elwd.(
      div
        [
          `R (input ~at ~ev ~on_create:(fun e -> element := Some e) ()); `R lbl;
        ])
  in
  let () =
    Utils.listen ~f:(fun v ->
        Option.iter
          (fun checkbox ->
            let v = Option.is_some v in
            Jv.set (El.to_jv checkbox) "checked" (Jv.of_bool v))
          !element)
    @@ Lwd.get var
  in
  (field, var)

let make ?at ?(persist = true) ?(filter = Lwd.pure (fun _ -> true)) t =
  (* <fieldset><legend> *)
  (* <fieldset><legend> *)
  let make_all ~g desc =
    Lwd_seq.map
      (function
        | Check (v, n', l, c) as desc ->
            let n = Hashtbl.hash v in
            let name = name ~g ~n t.name in
            let element, var = make_single ~persist name v (l n') c in
            { element; desc; var })
      desc
  in
  let all = make_all ~g:0 t.desc in
  let filtered =
    Lwd_seq.map
      (fun ({ desc = checkbox; _ } as ck) ->
        Lwd.map filter ~f:(fun f -> if f checkbox then Some ck else None))
      all
    |> Lwd_seq.lift |> Lwd_seq.filter_map Fun.id
  in
  let elts = Lwd_seq.map (fun { element; _ } -> element) filtered in
  let value =
    Lwd_seq.fold_monoid
      (fun { desc; var; _ } ->
        Lwd_seq.element (Lwd.map (Lwd.get var) ~f:(fun v' -> (v', desc, var))))
      Lwd_seq.monoid all
    |> Lwd_seq.lift
    |> Lwd_seq.filter_map (fun (v, ck, var) ->
           Option.map (fun v -> (v, ck, var)) v)
  in
  { field = Elwd.div ?at [ `S (Lwd_seq.lift elts) ]; all; filtered; value }

(* let rec make_all ~g ~n (acc_elt, acc_value) desc =
     match desc with
     | Check (v, l, c) :: tl ->
         let elt, value = make_check ~g ~n v l c in
         let acc_elt = Lwd_seq.concat acc_elt @@ Lwd_seq.element elt in
         let acc_value =
           Lwd.map2 acc_value value ~f:(fun acc -> function
             | None -> acc | Some v -> v :: acc)
         in
         make_all ~g ~n:(n + 1) (acc_elt, acc_value) tl
     | _ -> (Lwd.pure acc_elt, acc_value)
   in
   let elts, value = make_all ~g:0 ~n:0 (Lwd_seq.empty, Lwd.pure []) t.desc in
   { field = Elwd.div [ `S (Lwd_seq.lift elts) ]; value } *)
