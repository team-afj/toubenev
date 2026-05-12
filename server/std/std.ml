include Std_js

(* Re-exposed the modules shadowed in Std_js. *)
module Format = ContainersLabels.Format
module Printexc = Stdlib.Printexc

let new_random_uuid_v4 () =
  let random = String.to_bytes (Mirage_crypto_rng.generate 16) in
  Uuidm.(v4 random)

open Lunar

let now ?(tz = Timezone.utc) () =
  Unix.time () |> Int64.of_float |> Duration.from_int64
  |> Zoned_datetime.from_duration
  |> Zoned_datetime.change_timezone ~tz
