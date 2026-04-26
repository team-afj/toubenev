open Lunar
open Datarepr

module Infos = struct
  type t = { name : string; start : int; end_ : int; timezone : string }
  [@@deriving jsont]
end

module Options = struct
  type t = { inter_quest : int } [@@deriving jsont]
end

module Task_type = struct
  type t = {
    id : int;
    slug : string;
    nom : string;
    fiche_de_poste : string;
    impose : string;
    specialiste_requis : bool;
    decoupable : bool;
  }
  [@@deriving jsont]
end

module Lieu = struct
  type t = { id : int; slug : string; nom : string; description : string }
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
    pseudo : string option;
    nom : string option;
    prenom : string;
    nb_heures : float;
    langues : grist_string_list; [@default []]
    telephone : string;
    email : string;
    specialites : grist_int_list; [@default []]
    taches_interdites : grist_int_list; [@default []]
    amis : grist_int_list; [@default []]
    ennemis : grist_int_list; [@default []]
    date_d_arrivee : int option;
    date_de_depart : int option;
    indisponibilites_quotidiennes : grist_int_list; [@default []]
    horaires_preferes : grist_int_list; [@default []]
    horaires_contraints : grist_int_list; [@default []]
    indisponibilites_ponctuelles : grist_int_list; [@default []]
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
    nom : string;
    type_ : int;
    lieu : int;
    recurrence : string;
    jours : jour_list; [@default []]
    date_et_heure_de_debut : int;
    benevoles : int;
    duree_heures : float;
    fin_de_recurrence : int option;
    benevoles_assignes : grist_int_list; [@default []]
  }
  [@@deriving jsont]
end

module Assignation = struct
  type t = {
    name : string;
    initial_quest : int;
    start : int;
    end_ : int;
    volunteers : int list;
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
  places : (int, Rich.Place.t) Hashtbl.t;
  task_types : (int, Rich.Task_type.t) Hashtbl.t;
  volunteers : (int, Rich.Volunteer.t) Hashtbl.t;
  quests : (int, Rich.Quest.t) Hashtbl.t;
}

let new_id_map () =
  {
    places = Hashtbl.create 32;
    task_types = Hashtbl.create 32;
    volunteers = Hashtbl.create 128;
    quests = Hashtbl.create 256;
  }

let mandatory_of_string = function
  | "Non" -> Rich.Task_type.Not_necessarily
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
      | "Ponctuelle" -> Rich.Time_spec.On [ Datetime.date start ]
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
      Rich.Time_spec.recurrence;
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
    let timezone = Rich.Timezones.of_string infos.timezone in
    {
      Rich.Event_infos.name = infos.name;
      kind = Finite { start_date; end_date };
      timezone;
    }
  in
  let options =
    let options = List.hd options in
    {
      Rich.Options.minimum_transfer_time =
        Duration.from_minutes options.inter_quest;
    }
  in
  let places =
    let convert_place { Lieu.id; slug; nom; description } =
      let place = Rich.Place.make ~slug ~name:nom ~description () in
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
        } =
      let everyone_should_do_it = mandatory_of_string impose in
      let v =
        Rich.Task_type.make ~slug ~name ~description ~everyone_should_do_it
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
            Rich.Time_spec.recurrence = Daily;
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
        CCRAL.of_list_map ~f:(Hashtbl.find id_map.places) []
      in
      let availabilities =
        let unavailable =
          daily_spec indisponibilites_quotidiennes
          |> List.map ~f:(fun slot ->
              { Rich.Availability.status = Unavailable; slot })
        in
        let ponctually_unavailable =
          List.map indisponibilites_ponctuelles ~f:(fun i ->
              Printf.eprintf "\nPRT\n%!";
              let slot = time_specs.(i - 1) in
              { Rich.Availability.status = Unavailable; slot })
        in
        let best =
          daily_spec horaires_preferes
          |> List.map ~f:(fun slot ->
              { Rich.Availability.status = Available 1; slot })
        in
        let worse =
          daily_spec horaires_contraints
          |> List.map ~f:(fun slot ->
              { Rich.Availability.status = Available (-1); slot })
        in
        CCRAL.of_list
          (List.concat [ unavailable; best; worse; ponctually_unavailable ])
      in
      let v =
        Rich.Volunteer.make ?public_name ~name ~daily_workload ~proficiencies
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
        Rich.Volunteer.set_friends v friends;
        Rich.Volunteer.set_ennemis v ennemis)
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
        Rich.Quest.make ~name ~task_type ~place ~slot ~required_volunteers
          ~assigned_volunteers ()
      in
      Hashtbl.add id_map.quests id v;
      v
    in
    CCRAL.of_list_map ~f:convert_quests quests
  in
  { Rich.Planning.options; infos; places; task_types; volunteers; quests }
