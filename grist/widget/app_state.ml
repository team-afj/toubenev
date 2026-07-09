open! Data_repr

type t = {
  data : Grist_import.data;
  data_rich : Rich.Planning.t;
  answer : Api.answer;
  analysis : Shared.Analysis.t;
}
[@@deriving jsont]

let last_answer : t option Lwd.var = Lwd.var None
let check_btn : [ `Ready | `In_progress ] Lwd.var = Lwd.var `Ready

type optimize_state = Not_ready | Ready of t | Running

let optimize_state : optimize_state Lwd.var = Lwd.var Not_ready

type server_status = Offline | Working | Done

let to_fr_slug = function Offline -> "⛓️‍💥" | Working -> "🤖" | Done -> "🔗"

let server_status : server_status Lwd.var = Lwd.var Offline
