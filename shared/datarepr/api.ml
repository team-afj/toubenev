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
  volunteers : Normal.Volunteers.t;
  quests : Normal.Quests.t;
  diagnostics : diagnostic list;
}
[@@deriving jsont]

type assignation = {
  quest : Normal.Quest.t;
  volunteers : Rich.Volunteer.t list;
}
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
