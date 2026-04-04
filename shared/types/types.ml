module type Editable = sig
  type t
  type edit

  val edit_jsont : edit Jsont.t
  val apply_edit : edit -> t -> t (* result *)
end

module type Jsontable = sig
  type t

  val jsont : t Jsont.t
end

module type S = sig
  type t

  include Editable with type t := t
  include Jsontable with type t := t
end

module Random_access_list (X : S) : S = struct
  type t = X.t CCRAL.t

  let jsont =
    Jsont.array X.jsont |> Jsont.map ~dec:CCRAL.of_array ~enc:CCRAL.to_array

  type edit =
    | Insert_at of (int * X.t)
    | Remove of int
    | Update of (int * X.edit)
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | Insert_at (i, v) ->
        let before, after = CCRAL.take_drop i t in
        CCRAL.append before (CCRAL.cons v after)
    | Remove i -> CCRAL.remove t i
    | Update (i, edit) ->
        Option.fold
          (fun t v -> X.apply_edit edit v |> CCRAL.set t i)
          t (CCRAL.get t i)
end

module Place = struct
  type t = {
    id : int;
    slug : string;
    name : string;
    description : string option;
  }
  [@@deriving jsont]

  type edit =
    | New_slug of string
    | New_name of string
    | New_description of string option
  [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | New_slug slug -> { t with slug }
    | New_name name -> { t with name }
    | New_description description -> { t with description }

  let store : t Dynarray.t = Dynarray.create ()

  let make ~slug ~name ?description () =
    let id = Dynarray.length store in
    let v : t = { id; slug; name; description } in
    Dynarray.add_last store v;
    v
end

module Places = Random_access_list (Place)

module Task_type = struct
  type t = {
    id : int;
    slug : string;
    name : string;
    description : string option;
    places : Place.t list;
  }
  [@@deriving jsont]

  let store : t Dynarray.t = Dynarray.create ()

  let make ~slug ~name ?description ?places () =
    let places = Option.value ~default:[] places in
    let id = Dynarray.length store in
    let v : t = { id; slug; name; description; places } in
    Dynarray.add_last store v;
    v
end

module Volunteer = struct
  type t = { id : int; name : string; friends : t list }
  type shallow = { id : int; name : string; friends : int list }

  let store : t Dynarray.t = Dynarray.create ()

  let make ?friends ~name () =
    let friends = Option.value ~default:[] friends in
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

module Planning = struct
  type t = { places : Places.t } [@@deriving jsont]
  type edit = Places of Places.edit [@@deriving jsont]

  let apply_edit edit t =
    match edit with
    | Places edit -> { places = Places.apply_edit edit t.places }
end
