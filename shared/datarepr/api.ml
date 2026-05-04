type status = Ortools.Sat.Response.status =
  | Unknown
  | ModelInvalid
  | Feasible
  | Infeasible
  | Optimal
[@@deriving jsont]

type diagnostic = Error of string [@@deriving jsont]

type answer = {
  status : status;
  diagnostics : diagnostic list;
  solution : string;
  sufficient_assumptions_for_infeasibility : string;
  log : string;
}
[@@deriving jsont]
