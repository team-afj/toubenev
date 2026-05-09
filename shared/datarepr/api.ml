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

type answer = {
  status : status;
  diagnostics : diagnostic list;
  solution : string;
  sufficient_assumptions_for_infeasibility : string;
  log : string;
}
[@@deriving jsont]
