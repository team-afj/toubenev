module Place = struct
  type t = { slug : string; name : string; description : string option }
  [@@deriving jsont]
end

module Task_type = struct
  type t = {
    slug : string;
    name : string;
    description : string option;
    places : Place.t list;
  }
  [@@deriving jsont]
end

module Volunteer = struct
  type t = { id : int; name : string; friends : t list }
  type shallow = { id : int; name : string; friends : int list }

  let store : t Dynarray.t = Dynarray.create ()
  let counter = ref 0

  let make ~name (friends : t list) =
    let id = Dynarray.length store in
    let v : t = { id; name; friends } in
    Dynarray.add_last store v;
    v

  let to_shallow (t : t) : shallow =
    {
      id = t.id;
      name = t.name;
      friends = List.map ~f:(fun (t : t) -> t.id) t.friends;
    }

  let of_shallow (t : shallow) : t =
    {
      id = t.id;
      name = t.name;
      friends = List.map ~f:(Dynarray.get store) t.friends;
    }
end

type r = { v2 : int } [@@deriving jsont]
type t = A of r | B of int | C [@@deriving jsont]
type rec_ = R of rec_ | Nil [@@deriving jsont]
