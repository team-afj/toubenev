module type Editable = sig
  type t
  type edit

  val edit_jsont : edit Jsont.t
  val apply_edit : edit -> t -> t
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

module Random_access_list : (_ : S) -> S

module Place : sig
  type t = private {
    id : int;
    slug : string;
    name : string;
    description : string option;
  }

  include S with type t := t

  val make : slug:string -> name:string -> ?description:string -> unit -> t
end

module Places : sig
  include module type of Random_access_list (Place)
end

module Task_type : sig
  type t = private {
    id : int;
    slug : string;
    name : string;
    description : string option;
    places : Place.t list;
  }

  val jsont : t Jsont.t

  val make :
    slug:string ->
    name:string ->
    ?description:string ->
    ?places:Place.t list ->
    unit ->
    t
end

module Volunteer : sig
  type t = private { id : int; name : string; friends : t list }
  type shallow = private { id : int; name : string; friends : int list }

  val make : ?friends:t list -> name:string -> unit -> t
  val to_shallow : t -> shallow
  val of_shallow : shallow -> t
end

module Planning : sig
  type t = { places : Places.t }
  type edit = Places of Places.edit

  include S with type t := t and type edit := edit
end
