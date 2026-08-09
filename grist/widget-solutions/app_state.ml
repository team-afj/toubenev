open Data_repr
open Tables

let selected_in_grist : int Lwd.var = Lwd.var 1

let solutions : (int * string * Jstr.t) Lwd_seq.t Lwd.var =
  Lwd.var Lwd_seq.empty

let sol_select =
  let options =
    Lwd.get solutions
    |> Lwd_seq.map (fun (id, name, _) -> (string_of_int id, name))
  in
  let field_desc =
    let default = "1" in
    { Forms.Field.name = "sol_solutions_select"; default; label = [] }
  in
  Forms.Field_select.make field_desc options

let selected_solution = Lwd.get sol_select.value

let active_solution_state =
  Lwd.map2 selected_solution (Lwd.get solutions) ~f:(fun id seq ->
      let id = int_of_string id in
      let solutions = Lwd_seq.to_list seq in
      let sol_opt =
        List.find_opt solutions ~f:(fun (id', _name, _state) -> id = id')
      in
      Option.bind sol_opt (fun (_, _, state) ->
          Jsont_brr.decode Solutions.jsont state |> Result.to_option))

let active_solution_normal =
  Lwd.map active_solution_state ~f:(function
    | None -> None
    | Some ({ data_rich; _ } as state) ->
        let data = Conv.normalize data_rich in
        let static_analysis =
          Static_analysis.make data_rich.infos data.quests ()
        in
        Some { Solutions.state; data; static_analysis })

type optimize_state = Not_ready | Ready of Tables.Solutions.t | Running

let optimize_state : optimize_state Lwd.var = Lwd.var Not_ready

type server_status = Offline | Working | Done

let to_fr_slug = function Offline -> "⛓️‍💥" | Working -> "🤖" | Done -> "🔗"

let server_status : server_status Lwd.var = Lwd.var Offline
