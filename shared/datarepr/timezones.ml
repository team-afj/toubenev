open Lunar

let europe_paris = Timezone.make ~hour:2 ~min:0

let of_string = function
  | "Europe/Paris" -> europe_paris
  | _ -> failwith "Unknown timezone"
