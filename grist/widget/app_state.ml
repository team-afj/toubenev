open! Data_repr

type t = Tables.Solutions.t [@@deriving jsont]

type normal = {
  state : t;
  data : Api.data;
  static_analysis : Static_analysis.with_cache;
}

let grist_selected_solution : int Lwd.var = Lwd.var 1
let grist_solutions : (int * Jstr.t * Jv.t) list Lwd.var = Lwd.var []
let last_answer : t option Lwd.var = Lwd.var None

let normal =
  Lwd.map (Lwd.get last_answer) ~f:(function
    | None -> None
    | Some ({ data_rich; _ } as state) ->
        let data = Conv.normalize data_rich in
        let static_analysis =
          Static_analysis.make data_rich.infos data.quests ()
        in
        Some { state; data; static_analysis })

let check_btn : [ `Ready | `In_progress ] Lwd.var = Lwd.var `Ready

type optimize_state = Not_ready | Ready of t | Running

let optimize_state : optimize_state Lwd.var = Lwd.var Not_ready

type server_status = Offline | Working | Done

let to_fr_slug = function Offline -> "⛓️‍💥" | Working -> "🤖" | Done -> "🔗"

let server_status : server_status Lwd.var = Lwd.var Offline
