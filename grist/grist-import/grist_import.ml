open Lunar

module Infos = struct
  type t = {
    name : string; [@key "Nom"]
    start : int; [@key "Date_debut"]
    end_ : int; [@key "Date_fin"]
    timezone : string; [@key "Timezone"]
  }
  [@@deriving jsont]
end

module Options = struct
  type t = { inter_quete : int [@key "Duree_inter_quete"] } [@@deriving jsont]
end

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
type grist_string_list = string list

let grist_int_list_jsont = grist_list Jsont.int
let grist_string_list_jsont = grist_list Jsont.string

module Benevole = struct
  type t = {
    id : int;
    pseudo : string option; [@key "Pseudo"]
    nom : string option; [@key "Nom"]
    prenom : string; [@key "Prenom"]
    nb_heures : float; [@key "Nb_heures"]
    langues : grist_string_list; [@default []] [@key "Langues"]
    telephone : string; [@key "Telephone"]
    email : string; [@key "Email"]
    specialites : grist_int_list; [@default []] [@key "Specialites"]
    taches_interdites : grist_int_list; [@default []] [@key "Taches_interdites"]
    lieux_interdits : grist_int_list; [@default []] [@key "Lieux_interdits"]
    amis : grist_int_list; [@default []] [@key "Amis"]
    ennemis : grist_int_list; [@default []] [@key "Ennemis"]
    date_d_arrivee : int option; [@key "Date_d_arrivee"]
    date_de_depart : int option; [@key "Date_de_depart"]
    indisponibilites_quotidiennes : grist_int_list;
        [@default []] [@key "Indisponibilites_quotidiennes"]
    horaires_preferes : grist_int_list; [@default []] [@key "Horaires_preferes"]
    horaires_contraints : grist_int_list;
        [@default []] [@key "Horaires_contraints"]
    indisponibilites_ponctuelles : grist_int_list;
        [@default []] [@key "Autres_indisponibilites"]
  }
  [@@deriving jsont]
end

type jour = Lun | Mar | Mer | Jeu | Ven | Sam | Dim [@@deriving jsont]
type jour_list = jour list

let day_of_jour = function
  | Lun -> Weekday.Mon
  | Mar -> Tue
  | Mer -> Wed
  | Jeu -> Thu
  | Ven -> Fri
  | Sam -> Sat
  | Dim -> Sun

let jour_list_jsont = grist_list jour_jsont

module Time_spec = struct
  type t = {
    recurrence : string;
    start : int;
    duration_h : float;
    days : jour_list;
    end_date : int option;
  }
  [@@deriving jsont]
end

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
    duree_heures : float; [@key "Duree_heures_"]
    fin_de_recurrence : int option; [@key "Fin_de_recurrence"]
    benevoles_assignes : grist_int_list;
        [@default []] [@key "Benevoles_assignes"]
  }
  [@@deriving jsont]
end

type data = {
  options : Options.t list;
  infos : Infos.t list;
  places : Lieu.t list;
  task_types : Task_type.t list;
  time_specs : Time_spec.t list;
  volunteers : Benevole.t list;
  quests : Quete.t list;
}
[@@deriving jsont]

type id_map = {
  places : (int, Types.Place.t) Hashtbl.t;
  task_types : (int, Types.Task_type.t) Hashtbl.t;
  volunteers : (int, Types.Volunteer.t) Hashtbl.t;
  quests : (int, Types.Quest.t) Hashtbl.t;
}

let new_id_map () =
  {
    places = Hashtbl.create 32;
    task_types = Hashtbl.create 32;
    volunteers = Hashtbl.create 128;
    quests = Hashtbl.create 256;
  }

let mandatory_of_string = function
  | "Non" -> Types.Task_type.Not_necessarily
  | "Au moins une fois" -> At_least_once
  | "Tout le monde autant" -> In_equal_proportion
  | s -> failwith ("Unexpected value:" ^ s)

let to_planning ?(id_map = new_id_map ())
    ({
       infos;
       options;
       places;
       task_types;
       time_specs;
       volunteers = vols;
       quests;
     } :
      data) =
  let make_spec ~rec_flag ~days ~start ~duration_h ~end_date =
    let start =
      (* TODO TIMEZONE *)
      Datetime.from_duration @@ Duration.from_seconds start
    in
    let recurrence =
      match rec_flag with
      | "Ponctuelle" -> Types.Time_spec.On [ Datetime.date start ]
      | "Quotidienne" -> Daily
      | "Hebdomadaire" ->
          Weekly (Weekday.Set.of_list @@ List.map ~f:day_of_jour days)
      | s -> raise (Invalid_argument s)
    in
    let duration =
      Duration.from_seconds (Float.to_int (duration_h *. 60. *. 60.))
    in
    let first_day = Some (Datetime.date start) in
    let last_day =
      Option.map
        (fun d -> Date.from_duration (Duration.from_seconds d))
        end_date
    in
    {
      Types.Time_spec.recurrence;
      start = Datetime.time start;
      duration;
      first_day;
      last_day;
    }
  in
  let infos =
    let infos = List.hd infos in
    let start_date = Date.from_duration (Duration.from_seconds infos.start) in
    let end_date = Date.from_duration (Duration.from_seconds infos.end_) in
    let timezone = Types.Timezones.of_string infos.timezone in
    {
      Types.Event_infos.name = infos.name;
      kind = Finite { start_date; end_date };
      timezone;
    }
  in
  let options =
    let options = List.hd options in
    {
      Types.Options.minimum_transfer_time =
        Duration.from_minutes options.inter_quete;
    }
  in
  let places =
    let convert_place { Lieu.id; slug; nom; description } =
      let place = Types.Place.make ~slug ~name:nom ~description () in
      Hashtbl.add id_map.places id place;
      place
    in
    CCRAL.of_list_map ~f:convert_place places
  in
  let task_types =
    let convert_task_types
        {
          Task_type.id;
          slug;
          nom = name;
          fiche_de_poste = description;
          impose;
          specialiste_requis = specialist_only;
          decoupable = divisible;
          lieu_par_defaut = _;
        } =
      let everyone_should_do_it = mandatory_of_string impose in
      let v =
        Types.Task_type.make ~slug ~name ~description ~everyone_should_do_it
          ~specialist_only ~divisible ()
      in
      Hashtbl.add id_map.task_types id v;
      v
    in
    CCRAL.of_list_map ~f:convert_task_types task_types
  in
  let time_specs =
    let convert_spec { Time_spec.recurrence; start; duration_h; days; end_date }
        =
      make_spec ~rec_flag:recurrence ~days ~start ~duration_h ~end_date
    in
    List.map ~f:convert_spec time_specs |> Array.of_list
  in
  let volunteers =
    let gather_time_slots l =
      let tz = Timezone.to_duration infos.timezone in
      let mk start end_ =
        let start_time =
          Time.make ~hour:(start - 1) ~min:0 ~sec:0 () |> Result.get_ok
        in
        (Time.(start_time - tz), Duration.from_hours (end_ - start))
      in
      let rec aux (groups, current) l =
        match (current, l) with
        | None, [] -> groups
        | None, start :: tl -> aux (groups, Some (start, start + 1)) tl
        | Some (start, end_), [] -> mk start end_ :: groups
        | Some (start, end_), start' :: tl when Int.equal start' end_ ->
            aux (groups, Some (start, start' + 1)) tl
        | Some (start, end_), start' :: tl ->
            aux (mk start end_ :: groups, Some (start', start' + 1)) tl
      in
      aux ([], None) (List.sort ~cmp:Int.compare l)
    in
    let daily_spec l =
      let slots = gather_time_slots l in
      List.map slots ~f:(fun (start, duration) ->
          {
            Types.Time_spec.recurrence = Daily;
            start;
            duration;
            first_day = None;
            last_day = None;
          })
    in
    let convert_colunteers
        {
          Benevole.id;
          pseudo = public_name;
          nom;
          prenom;
          nb_heures;
          specialites;
          taches_interdites;
          lieux_interdits;
          indisponibilites_quotidiennes;
          horaires_preferes;
          horaires_contraints;
          indisponibilites_ponctuelles;
          _;
        } =
      let name =
        match (nom, prenom) with
        | None, s | Some "", s | Some s, "" -> s
        | Some n, p -> Printf.sprintf "%s %s" p n
      in
      let daily_workload =
        let seconds = nb_heures *. 60. *. 60. in
        Lunar.Duration.from_seconds (Float.to_int seconds)
      in
      let proficiencies =
        CCRAL.of_list_map ~f:(Hashtbl.find id_map.task_types) specialites
      in
      let forbidden_tasks =
        CCRAL.of_list_map ~f:(Hashtbl.find id_map.task_types) taches_interdites
      in
      let forbidden_places =
        CCRAL.of_list_map ~f:(Hashtbl.find id_map.places) lieux_interdits
      in
      let availabilities =
        let unavailable =
          daily_spec indisponibilites_quotidiennes
          |> List.map ~f:(fun slot ->
              { Types.Availability.status = Unavailable; slot })
        in
        let ponctually_unavailable =
          List.map indisponibilites_ponctuelles ~f:(fun i ->
              Printf.eprintf "\nPRT\n%!";
              let slot = time_specs.(i - 1) in
              { Types.Availability.status = Unavailable; slot })
        in
        let best =
          daily_spec horaires_preferes
          |> List.map ~f:(fun slot ->
              { Types.Availability.status = Available 1; slot })
        in
        let worse =
          daily_spec horaires_contraints
          |> List.map ~f:(fun slot ->
              { Types.Availability.status = Available (-1); slot })
        in
        CCRAL.of_list
          (List.concat [ unavailable; best; worse; ponctually_unavailable ])
      in
      let v =
        Types.Volunteer.make ?public_name ~name ~daily_workload ~proficiencies
          ~forbidden_tasks ~forbidden_places ~availabilities ()
      in
      Hashtbl.add id_map.volunteers id v;
      v
    in
    CCRAL.of_list_map ~f:convert_colunteers vols
  in
  let () =
    (* We set friends and ennemis in a second pass *)
    List.iter vols ~f:(fun { Benevole.id; amis; ennemis; _ } ->
        let v = Hashtbl.find id_map.volunteers id in
        let friends =
          List.map ~f:(fun id -> (Hashtbl.find id_map.volunteers id).id) amis
        in
        let ennemis =
          List.map ~f:(fun id -> (Hashtbl.find id_map.volunteers id).id) ennemis
        in
        Types.Volunteer.set_friends v friends;
        Types.Volunteer.set_ennemis v ennemis)
  in
  let quests =
    let convert_quests
        {
          Quete.id;
          nom = name;
          type_;
          lieu;
          benevoles = required_volunteers;
          benevoles_assignes;
          recurrence;
          date_et_heure_de_debut;
          duree_heures;
          jours;
          fin_de_recurrence;
          _;
        } =
      let task_type = Hashtbl.find id_map.task_types type_ in
      let place = Hashtbl.find id_map.places lieu in
      let assigned_volunteers =
        CCRAL.of_list_map ~f:(Hashtbl.find id_map.volunteers) benevoles_assignes
      in
      let slot =
        make_spec ~rec_flag:recurrence ~days:jours ~start:date_et_heure_de_debut
          ~duration_h:duree_heures ~end_date:fin_de_recurrence
      in
      let v =
        Types.Quest.make ~name ~task_type ~place ~slot ~required_volunteers
          ~assigned_volunteers ()
      in
      Hashtbl.add id_map.quests id v;
      v
    in
    CCRAL.of_list_map ~f:convert_quests quests
  in
  { Types.Planning.options; infos; places; task_types; volunteers; quests }
