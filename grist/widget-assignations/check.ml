open Data_repr
open! Rich
open! Normal

let pp_error = function
  | `Too_many_volunteers (q, n) ->
      Format.sprintf "%i bénévole(s) en trop sur %s" n q.Quest.name
  | `Too_few_volunteers (q, n) ->
      Format.sprintf "%i bénévole(s) manquant sur %s" (abs n) q.Quest.name
  | `Multiple_assignements (v, q, q') ->
      Format.sprintf "%s est assigné en même temps à %s et %s" v.Volunteer.name
        q.Quest.name q'.Quest.name
  | `Msg err -> err

let check_q_filled { Api.quest; volunteers } =
  let diff =
    Volunteers.cardinal volunteers - quest.initial.required_volunteers
  in
  match diff with
  | 0 -> Ok ()
  | n when n > 0 -> Error (`Too_many_volunteers (quest, n))
  | n -> Error (`Too_few_volunteers (quest, n))

let check_can_do (infos : Event_infos.t) (checks : Static_analysis.with_cache)
    (assignations : Api.assignation list) { Api.quest; volunteers } =
  let overlapping_assignations =
    List.filter assignations ~f:(fun { Api.quest = q'; _ } ->
        (not (String.equal quest.id q'.id)) && Quest.overlaps infos quest q')
  in
  Volunteers.to_list_map volunteers ~f:(fun v ->
      let can_do =
        checks.can_do_res v quest |> Result.map_error (fun err -> `Msg err)
      in
      can_do
      :: List.map overlapping_assignations
           ~f:(fun { Api.quest = q'; volunteers } ->
             if Volunteers.mem v volunteers then
               Error (`Multiple_assignements (v, quest, q'))
             else Ok ()))
  |> List.flatten

let filter_errors l =
  List.filter_map l ~f:(function Ok () -> None | Error err -> Some err)

let assignations infos checks (l : Api.assignation list) =
  List.flat_map l ~f:(fun ass ->
      let q_filled = check_q_filled ass in
      let q_overlap = check_can_do infos checks l ass in
      filter_errors (q_filled :: q_overlap))
  |> List.map ~f:pp_error
