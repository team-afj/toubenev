(*

Task types:

  {
    "id": 1,
    "manualSort": 1,
    "Slug": "🧹",
    "Nom": "Clean",
    "Fiche_de_poste": "",
    "Impose": "Non",
    "Specialiste_requis": false,
    "Decoupable": true,
    "Lieu_par_defaut": 1
  }

*)

module Task_type = struct
  type t = {
    id : int;
    slug : string; [@key "Slug"]
    nom : string; [@key "Nom"]
    fiche_de_poste : string; [@key "Fiche_de_poste"]
    impose : string; [@key "Impose"]
    specialiste_requis : bool; [@key "Specialiste_requis"]
    decoupable : bool; [@key "Decoupable"]
    lieu_par_defaut : int; [@key "Lieu_par_defaut"]
  }
  [@@deriving jsont]
end

module Lieu = struct
  type t = {
    id : int;
    slug : string; [@key "Slug"]
    nom : string; [@key "Nom"]
    description : string; [@key "Description"]
  }
  [@@deriving jsont]
end

let grist_list elt_jsont =
  let get_or_raise = function
    | Ok r -> r
    | Error err -> raise (Jsont.Error err)
  in
  let tag_l = Jsont.Json.encode' Jsont.string "L" |> Result.get_ok in
  let enc f acc l =
    let i = ref 0 in

    Stdlib.List.fold_left
      (fun acc e ->
        incr i;
        f acc !i (Jsont.Json.encode' elt_jsont e |> Result.get_ok))
      (f acc 0 tag_l) l
  in
  let dec_empty () = [] in
  let dec_add i elt l =
    match i with
    | 0 ->
        let kind = Jsont.Json.decode' Jsont.string elt |> get_or_raise in
        if String.equal kind "L" then l
        else
          let err =
            Jsont.Error.(
              make_msg Context.empty Jsont.Meta.none
                "Expected \"L\" as first element.")
          in
          raise (Jsont.Error err)
    | _ ->
        let e = Jsont.Json.decode' elt_jsont elt |> get_or_raise in
        e :: l
  in
  let dec_finish _meta _i = List.rev in
  Jsont.Array.map ~enc:{ enc } ~dec_empty ~dec_add ~dec_finish Jsont.json
  |> Jsont.Array.array

let grist_list elt =
  let jsont = grist_list elt in

  let null = Jsont.null [] in
  let enc = function [] -> null | _ -> jsont in
  Jsont.any ~dec_null:null ~dec_array:jsont ~enc ()

type grist_int_list = int list

let grist_int_list_jsont = grist_list Jsont.int

module Benevole = struct
  type t = {
    id : int;
    pseudo : string option; [@key "Pseudo"]
    nom : string option; [@key "Nom"]
    prenom : string; [@key "Prenom"]
    nb_heures : int; [@key "Nb_heures"]
    langues : (Jsont.Json.t[@jsont Jsont.json]); [@key "Langues"]
    telephone : string; [@key "Telephone"]
    email : string; [@key "Email"]
    specialites : (Jsont.Json.t[@jsont Jsont.json]) option; [@key "Specialites"]
    taches_interdites : (Jsont.Json.t[@jsont Jsont.json]) option;
        [@key "Taches_interdites"]
    lieux_interdits : (Jsont.Json.t[@jsont Jsont.json]) option;
        [@key "Lieux_interdits"]
    amis : (Jsont.Json.t[@jsont Jsont.json]) option; [@key "Amis"]
    ennemis : (Jsont.Json.t[@jsont Jsont.json]) option; [@key "Ennemis"]
    date_d_arrivee : int option; [@key "Date_d_arrivee"]
    date_de_depart : int option; [@key "Date_de_depart"]
    indisponibilites_quotidiennes : grist_int_list;
        [@key "Indisponibilites_quotidiennes"]
    horaires_preferes : (Jsont.Json.t[@jsont Jsont.json]) option;
        [@key "Horaires_preferes"]
    horaires_contraints : (Jsont.Json.t[@jsont Jsont.json]) option;
        [@key "Horaires_contraints"]
    indisponibilites_ponctuelles : (Jsont.Json.t[@jsont Jsont.json]) option;
        [@key "Indisponibilites_ponctuelles"]
  }
  [@@deriving jsont]
end

type jour = Lun | Mar | Mer | Jeu | Ven | Sam | Dim [@@deriving jsont]
type jour_list = jour list

let jour_list_jsont = grist_list jour_jsont

module Quete = struct
  type t = {
    id : int;
    nom : string; [@key "Nom"]
    type_ : int; [@key "Type"]
    lieu : int; [@key "Lieu"]
    recurrence : string; [@key "Recurrence"]
    jours : jour_list; [@default []] [@key "Jours"]
    date_et_heure_de_debut : int; [@key "Date_et_heure_de_debut"]
    benevoles : int; [@key "benevoles"]
    duree_heures : int; [@key "Duree_heures_"]
    date_et_heure_de_fin : int; [@key "Date_et_heure_de_fin"]
    benevoles_assignes : grist_int_list; [@key "Benevoles_assignes"]
  }
  [@@deriving jsont]
end

type data = {
  (* options : Options.t;
    info : Event_infos.t; *)
  places : Lieu.t list;
  task_types : Task_type.t list;
  volunteers : Benevole.t list;
  quests : Quete.t list;
}
[@@deriving jsont]
