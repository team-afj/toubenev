include ContainersLabels
module Lunar_jsont = Lunar_jsont

let () = Hashtbl.randomize ()

(* These modules are known to be quite detrimental to javascript bundle size. We
   shadow them to enforce their non-usage in client-side code. *)
module Format = struct end
module Printexc = struct end

let new_random_uuid_v4 () = Uuidm.v4_gen (Random.get_state ()) ()

module Set = struct
  include Set

  module type S = sig
    include S

    val to_list_map : f:(elt -> 'a) -> t -> 'a list
    val iter : f:(elt -> unit) -> t -> unit
    val fold : init:'a -> f:('a -> elt -> 'a) -> t -> 'a
  end

  module Make (T : OrderedType) = struct
    include Set.Make (T)

    let to_list_map ~f t = List.map ~f (to_list t)
    let iter ~f t = iter f t
    let fold ~init ~f t = fold (Fun.flip f) t init
  end
end
