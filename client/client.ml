open Brr

let source = Jv.new' (Jv.get Jv.global "EventSource") [| Jv.of_string "/sse" |]

let () =
  Ev.listen Brr_io.Message.Ev.message
    (fun ev -> Console.log [ "Received"; ev ])
    (Ev.target_of_jv source)
  |> ignore
