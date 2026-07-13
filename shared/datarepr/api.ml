open Normal

type status = Ortools.Sat.Response.status =
  | Unknown
  | ModelInvalid
  | Feasible
  | Infeasible
  | Optimal
[@@deriving jsont]

type diagnostic_level = Error | Warning | Info [@@deriving jsont]
type diagnostic = diagnostic_level * string [@@deriving jsont]

let diagnostic_level_to_string = function
  | Error -> "diag-error"
  | Warning -> "diag-warn"
  | Info -> "diag-info"

type data = {
  volunteers : Volunteers.t;
  quests : Quests.t;
  quests_groups : Quests_group.t String.Map.t;
  diagnostics : diagnostic list;
}
[@@deriving jsont]

type assignation = { quest : Quest.t; volunteers : Volunteers.t }
[@@deriving jsont]

type answer = {
  status : status;
  diagnostics : diagnostic list;
  solution : assignation list;
  sufficient_assumptions_for_infeasibility : string list;
  log : string;
  date : Lunar_jsont.Zoned_datetime.t;
  objective_value : float;
  deterministic_time : float;
  best_objective_bound : float;
}
[@@deriving jsont]

let dummy_answer =
  {
    status = Unknown;
    diagnostics = [];
    solution = [];
    sufficient_assumptions_for_infeasibility = [];
    log = "";
    date = Lunar.Zoned_datetime.epoch ();
    objective_value = 0.;
    deterministic_time = 0.;
    best_objective_bound = 0.;
  }

let max_sat (_q : Quest.t) (_v : Volunteer.t) = 1
let min_sat (_q : Quest.t) (_v : Volunteer.t) = -1

let satisfaction (q : Quest.t) (v : Volunteer.t) =
  let time =
    List.fold_left ~init:0 v.preferences ~f:(fun acc (i, s) ->
        if Time_slot.overlaps q.slot s then i + acc else acc)
  in
  time |> max (-1) |> min 1

let satisfaction (assignations : assignation list) =
  let min_sat, max_sat =
    List.fold_left assignations ~init:(0, 0)
      ~f:(fun acc { quest; volunteers } ->
        Volunteers.fold volunteers ~init:acc ~f:(fun (min_acc, max_acc) v ->
            (min_acc + min_sat quest v, max_acc + max_sat quest v)))
  in
  let sat =
    List.fold_left assignations ~init:0 ~f:(fun acc { quest; volunteers } ->
        Volunteers.fold volunteers ~init:acc ~f:(fun acc v ->
            acc + satisfaction quest v))
  in
  (2. *. Float.of_int (sat - min_sat) /. Float.of_int (max_sat - min_sat)) -. 1.
