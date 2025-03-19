open Brr

let calendar_el = El.find_first_by_selector (Jstr.v "#calendar") |> Option.get
let start = Js.Date.of_string "2025-03-19T12:24:00"
let end_ = Js.Date.of_string "2025-03-19T18:24:00"
let events = Calendar.Plain_event.make ~start ~end_ ()
let c = Calendar.make ~target:calendar_el ~plugins:[ List ] ~start ()
let () = Console.log [ events ]
let () = Calendar.set_option c Events [ events ]
let () = Static_results.events
