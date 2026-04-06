include ContainersLabels
module Lunar_jsont = Lunar_jsont

let () = Hashtbl.randomize ()

(* These modules are known to be quite detrimental to javascript bundle size. We
   shadow them to enforce their non-usage in client-side code. *)
module Format = struct end
module Printexc = struct end
