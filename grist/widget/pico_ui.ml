open Brr
open Brr_lwd

let container = At.class' (Jstr.v "container")

module El = struct
  let section ?(at = []) = El.section ~at:(container :: at)
end

module Elwd = struct
  let section ?(at = []) = Elwd.section ~at:(`P container :: at)
end
