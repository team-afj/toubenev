include ContainersLabels
module Lunar_jsont = Lunar_jsont

let () = Hashtbl.randomize ()

(* These modules are known to be quite detrimental to javascript bundle size. We
   shadow them to enforce their non-usage in client-side code. *)
module Format = struct end
module Printexc = struct end

let new_random_uuid_v4 =
  let state = Random.get_state () in
  fun () -> Uuidm.v4_gen state ()

module Set = struct
  include Set

  module type S = sig
    include S

    val to_list_map : f:(elt -> 'a) -> t -> 'a list
    val iter : f:(elt -> unit) -> t -> unit
    val fold : init:'a -> f:('a -> elt -> 'a) -> t -> 'a
  end

  module type OrderedTypeJsont = sig
    type t

    include OrderedType with type t := t

    val jsont : t Jsont.t
  end

  module type S_jsont = sig
    include S

    val jsont : t Jsont.t
  end

  module Make (T : OrderedType) = struct
    include Set.Make (T)

    let to_list_map ~f t = List.map ~f (to_list t)
    let iter ~f t = iter f t
    let fold ~init ~f t = fold (Fun.flip f) t init
  end

  module Make_jsont (C : OrderedTypeJsont) = struct
    include Make (C)

    let jsont =
      let enc =
        {
          Jsont.Array.enc =
            (fun f init set ->
              let i = ref (-1) in
              fold ~init
                ~f:(fun acc elt ->
                  incr i;
                  f acc !i elt)
                set);
        }
      in
      Jsont.Array.map ~enc
        ~dec_empty:(fun () -> empty)
        ~dec_add:(fun _ -> add)
        ~dec_finish:(fun _ _ set -> set)
        C.jsont
      |> Jsont.Array.array
  end
end

module Map = struct
  include Map

  module type OrderedTypeJsont = sig
    type t

    include OrderedType with type t := t

    val jsont : t Jsont.t
  end

  module type S_jsont = sig
    include S

    val jsont : 'a Jsont.t -> 'a t Jsont.t
  end

  module Make (C : OrderedType) = struct
    include Make (C)

    let add_multi ~singleton ~cons key v t =
      update key
        (function None -> Some (singleton v) | Some vs -> Some (cons v vs))
        t

    let add_multi_list key v t =
      add_multi ~singleton:List.return ~cons:List.cons key v t
  end

  module Make_jsont (C : OrderedTypeJsont) = struct
    include Make (C)

    let of_string s = Jsont_bytesrw.decode_string C.jsont s |> Result.get_ok
    let to_string v = Jsont_bytesrw.encode_string C.jsont v |> Result.get_ok

    (* TODO an array of couples would be a better fit *)
    let string_map ?kind ?doc type' =
      let dec_empty () = empty in
      let dec_add _meta key v mems =
        let key = of_string key in
        add key v mems
      in
      let dec_finish _meta mems = mems in
      let enc f mems acc =
        fold (fun key v acc -> f Jsont.Meta.none (to_string key) v acc) mems acc
      in
      Jsont.Object.Mems.map ?kind ?doc type' ~dec_empty ~dec_add ~dec_finish
        ~enc:{ enc }

    let jsont t =
      let open Jsont.Object in
      map Fun.id |> keep_unknown (string_map t) ~enc:Fun.id |> finish
  end
end

module Int = struct
  include Int
  module Set = Set.Make (Int)
  module Map = Map.Make (Int)
end

module String = struct
  module T = struct
    include String

    let jsont = Jsont.string
  end

  include T
  module Set = Set.Make_jsont (T)
  module Map = Map.Make_jsont (T)
end
