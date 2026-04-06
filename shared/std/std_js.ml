include ContainersLabels
module Lunar_jsont = Lunar_jsont

let () = Hashtbl.randomize ()

(* These modules are known to be quite detrimental to javascript bundle size. We
   shadow them to enforce their non-usage in client-side code. *)
module Format = struct end
module Printexc = struct end

let new_random_uuid_v4 () =
  let random = String.to_bytes (Mirage_crypto_rng.generate 16) in
  Uuidm.(v4 random)
