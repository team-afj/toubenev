open Brr

external get_static_events : unit -> Jv.t = "get_static_events"

type quest_type = { id : string; name : string } [@@deriving jsont]
type place = { id : string; name : string } [@@deriving jsont]
type volunteer = { id : string; pseudo : string } [@@deriving jsont]

type raw_quest = {
  id : string;
  name : string;
  types : string list;
  place : string;
  volunteers : string list;
  start : string;
  end_ : string; [@key "end"]
}
[@@deriving jsont]

type results = {
  quest_types : quest_type list;
  places : place list;
  volunteers : volunteer list;
  quests : raw_quest list;
}
[@@deriving jsont]

let events =
  let raw = get_static_events () in
  let json = Jsont_brr.decode_jv results_jsont raw in
  Console.log [ raw; json ]
