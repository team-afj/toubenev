open Brr
open Brr_lwd
module Api = Data_repr.Api
open Lwd_infix

let j = Jstr.v
let container = At.class' (Jstr.v "container")

module At = struct
  include At

  let overflow_auto = At.class' (Jstr.v "overflow-auto")
end

module El = struct
  include El

  let dialog ?d ?at v = El.v ?d ?at (Jstr.v "dialog") v
  let section ?(at = []) = El.section ~at:(container :: at)
end

module Elwd = struct
  include Elwd

  let section ?(at = []) = Elwd.section ~at:(`P container :: at)

  let modal ?(opened = Lwd.var false) ~title ?footer content =
    let at_opened =
      let$ shown = Lwd.get opened in
      if shown then At.v (j "open") (j "true") else At.void
    in
    let header = Elwd.header [ `R (Elwd.p [ `R (Elwd.strong [ title ]) ]) ] in
    let content =
      match footer with
      | None -> `R header :: content
      | Some footer ->
          let style = El.style [ El.txt' "input: {width: auto}" ] in
          (`R header :: content) @ [ `R (Elwd.footer (`P style :: footer)) ]
    in
    (dialog ~at:[ `R at_opened ] [ `R (Elwd.article content) ], opened)
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

module Modal = struct
  let one_shot ~title content =
    let btn =
      El.button ~at:[ At.v (j "aria-label") (j "Close"); At.rel (j "prev") ] []
    in
    let dialog =
      let header = [ btn; El.p [ El.strong [ El.txt' title ] ] ] |> El.header in
      El.dialog
        ~at:[ At.v (j "open") (j "true") ]
        [ El.article (header :: content) ]
    in
    let _ = Ev.listen Ev.click (fun _ -> El.remove dialog) (El.as_target btn) in
    El.append_children (G.document |> Document.body) [ dialog ]
end

(*
<dialog open>
  <article>
    <header>
      <button aria-label="Close" rel="prev"></button>
      <p>
        <strong>🗓️ Thank You for Registering!</strong>
      </p>
    </header>
    <p>
      We're excited to have you join us for our
      upcoming event. Please arrive at the museum
      on time to check in and get started.
    </p>
    <ul>
      <li>Date: Saturday, April 15</li>
      <li>Time: 10:00am - 12:00pm</li>
    </ul>
  </article>
</dialog>
*)
