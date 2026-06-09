open Brr
open Brr_lwd
module Api = Data_repr.Api

let container = At.class' (Jstr.v "container")

module At = struct
  include At

  let overflow_auto = At.class' (Jstr.v "overflow-auto")
end

module El = struct
  include El

  let section ?(at = []) = El.section ~at:(container :: at)
end

module Elwd = struct
  include Elwd

  let section ?(at = []) = Elwd.section ~at:(`P container :: at)
end

let accordion ~name ?(closed = false) ~title content =
  (* TODO there might a bug in lwd, with the pure version of this function the
    open attribute disapears *)
  let at =
    let at = [ `P (At.name (Jstr.v name)) ] in
    match closed with
    | true -> at
    | false -> `P (At.v (Jstr.v "open") (Jstr.v "open")) :: at
  in
  Elwd.details ~at
    (`R (Elwd.summary [ `R title ]) :: [ `R (Elwd.section content) ])

let diag_card ((lvl, msg) : Api.diagnostic) =
  let at =
    [
      At.class' (Jstr.v "diag-card");
      At.class' (Jstr.v (Api.diagnostic_level_to_string lvl));
    ]
  in
  El.article ~at [ El.txt' msg ]
