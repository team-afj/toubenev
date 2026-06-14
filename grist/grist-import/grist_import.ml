open Lunar_jsont
open Data_repr

module Infos = struct
  type t = {
    name : string;
    start : int;
    end_ : int;
    timezone : string;
    day_start_time : float;
    minimum_transfer_time : int;  (** minutes *)
    daily_break_duration : float;  (** hours *)
  }
  [@@deriving jsont]
end

module Options = struct
  type t = {
    min_quest_duration : int;  (** minutes *)
    max_quest_duration : int;  (** minutes *)
    coef_friend : int;
    coef_bad_time : int;
    coef_good_time : int;
    coef_amplitude : int;
    coef_daily_eq : int;
    coef_event_eq : int;
    coef_preferred_quest : int;
    coef_constrained_quest : int;
  }
  [@@deriving jsont]
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
    free : bool;
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
    pseudo : string;
    nom : string;
    prenom : string;
    nb_heures : float; [@default 4.]
    langues : grist_string_list; [@default []]
    telephone : string;
    email : string;
    specialites : grist_int_list; [@default []]
    taches_interdites : grist_int_list; [@default []]
    favorite_quests : grist_int_list; [@default []]
    constrained_quests : grist_int_list; [@default []]
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
    benevoles : int; [@default 1]
    duree_heures : float; [@default 1.]
    fin_de_recurrence : int option;
    benevoles_assignes : grist_int_list; [@default []]
  }
  [@@deriving jsont]
end

module Assignation = struct
  type t = {
    solution : int;
    ref : string;
    name : string;
    initial_quest : int;
    start : int;  (** UTC timestamp *)
    end_ : int;  (** UTC timestamp *)
    volunteers : int list;
  }
  [@@deriving jsont]

  let v ~solution (assignation : Api.assignation) =
    let slot = assignation.quest.slot in
    let start =
      Zoned_datetime.to_utc_duration slot.start |> Duration.to_seconds
    in
    let end_ =
      Zoned_datetime.to_utc_duration (Normal.Time_slot.end_ slot)
      |> Duration.to_seconds
    in
    {
      solution;
      name = assignation.quest.name;
      ref = assignation.quest.id;
      initial_quest = Rich.id_to_int assignation.quest.initial.id;
      start;
      end_;
      volunteers =
        Normal.Volunteers.to_list_map
          ~f:(fun v -> Rich.id_to_int v.Normal.Volunteer.initial.id)
          assignation.volunteers;
    }
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
  places : Rich.Place.t Int.Map.t;
  task_types : Rich.Task_type.t Int.Map.t;
  volunteers : Rich.Volunteer.t Int.Map.t;
  quests : Rich.Quest.t Int.Map.t;
}
(* TODO It would be much easier to reuse grist ids directly *)

let new_id_map () =
  {
    places = Int.Map.empty;
    task_types = Int.Map.empty;
    volunteers = Int.Map.empty;
    quests = Int.Map.empty;
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
  let infos =
    let infos = List.hd infos in
    let start_date = Date.from_duration (Duration.from_seconds infos.start) in
    let end_date = Date.from_duration (Duration.from_seconds infos.end_) in
    let timezone = Rich.Timezones.of_string infos.timezone in
    let day_start_local =
      let seconds = Int.of_float (infos.day_start_time *. 3600.) in
      Time.from_duration (Duration.from_seconds seconds)
    in
    {
      Rich.Event_infos.name = infos.name;
      kind = Finite { start_date; end_date };
      timezone;
      day_start_local;
      minimum_transfer_time = Duration.from_minutes infos.minimum_transfer_time;
      daily_break_duration = Duration.from_hours_f infos.daily_break_duration;
    }
  in
  let make_spec ~rec_flag ~days ~start:start' ~duration_h ~end_date =
    let start =
      Zoned_datetime.from_duration @@ Duration.from_seconds start'
      |> Zoned_datetime.change_timezone ~tz:infos.timezone
    in
    let local_date = Zoned_datetime.on_local Datetime.date in
    let recurrence =
      match rec_flag with
      | "Ponctuelle" -> Rich.Time_spec.On [ local_date start ]
      | "Quotidienne" -> Daily
      | "Hebdomadaire" ->
          Weekly (Weekday.Set.of_list @@ List.map ~f:day_of_jour days)
      | s -> raise (Invalid_argument s)
    in
    let duration =
      Duration.from_seconds (Float.to_int (duration_h *. 60. *. 60.))
    in
    let first_day =
      match recurrence with On _ -> None | _ -> Some (local_date start)
    in
    let last_day =
      Option.map
        (fun d -> Date.from_duration (Duration.from_seconds d))
        end_date
    in
    {
      Rich.Time_spec.recurrence;
      start = Zoned_datetime.on_local Datetime.time start;
      duration;
      first_day;
      last_day;
    }
  in
  let options =
    let options = List.hd options in
    Rich.Options.
      {
        min_quest_duration = Duration.from_minutes options.min_quest_duration;
        max_quest_duration = Duration.from_minutes options.max_quest_duration;
        friendship_bonus = options.coef_friend;
        desired_time_bonus = options.coef_good_time;
        undesired_time_malus = options.coef_bad_time;
        desired_quest_bonus = options.coef_preferred_quest;
        undesired_quest_bonus = options.coef_constrained_quest;
        large_amplitude_malus = options.coef_amplitude;
        daily_equilibrium_malus = options.coef_daily_eq;
        event_equilibrium_malus = options.coef_event_eq;
      }
  in
  let id_map, places =
    let convert_place id_map { Lieu.id; slug; nom; description } =
      let ids = Rich.id_of_int id in
      let place = Rich.Place.make ~id:ids ~slug ~name:nom ~description () in
      ({ id_map with places = Int.Map.add id place id_map.places }, place)
    in
    let id_map, places =
      List.fold_left_map ~f:convert_place ~init:id_map places
    in
    (id_map, CCRAL.of_list places)
  in
  let id_map, task_types =
    let convert_task_types id_map
        {
          Task_type.id;
          slug;
          nom = name;
          fiche_de_poste = description;
          impose;
          specialiste_requis = specialist_only;
          decoupable = divisible;
          free;
        } =
      let ids = Rich.id_of_int id in
      let everyone_should_do_it = mandatory_of_string impose in
      let v =
        Rich.Task_type.make ~id:ids ~slug ~name ~description
          ~everyone_should_do_it ~specialist_only ~divisible ~free ()
      in
      ({ id_map with task_types = Int.Map.add id v id_map.task_types }, v)
    in
    let id_map, task_types =
      List.fold_left_map ~f:convert_task_types ~init:id_map task_types
    in
    (id_map, CCRAL.of_list task_types)
  in
  let time_specs =
    let convert_spec { Time_spec.recurrence; start; duration_h; days; end_date }
        =
      make_spec ~rec_flag:recurrence ~days ~start ~duration_h ~end_date
    in
    List.map ~f:convert_spec time_specs |> Array.of_list
  in
  let id_map, volunteers =
    let gather_time_slots l =
      let mk start end_ =
        let start_time =
          Time.make ~hour:(start - 1) ~min:0 ~sec:0 () |> Result.get_ok
        in
        (start_time, Duration.from_hours (end_ - start))
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
    let convert_volunteers id_map
        {
          Benevole.id;
          pseudo = public_name;
          nom;
          prenom;
          nb_heures;
          specialites;
          taches_interdites;
          favorite_quests;
          constrained_quests;
          indisponibilites_quotidiennes;
          horaires_preferes;
          horaires_contraints;
          indisponibilites_ponctuelles;
          date_d_arrivee;
          date_de_depart;
          _;
        } =
      let ids = Rich.id_of_int id in
      let name =
        match (nom, prenom) with
        | "", "" -> "Inconnu " ^ string_of_int id
        | "", s | s, "" -> s
        | n, p -> Printf.sprintf "%s %s" p n
      in
      let name = String.trim name in
      let public_name =
        if String.is_empty public_name then None else Some public_name
      in
      let daily_workload =
        let seconds = nb_heures *. 60. *. 60. in
        Lunar.Duration.from_seconds (Float.to_int seconds)
      in
      let proficiencies =
        CCRAL.of_list_map
          ~f:(fun i -> Int.Map.find i id_map.task_types)
          specialites
      in
      let forbidden_tasks =
        CCRAL.of_list_map
          ~f:(fun i -> Int.Map.find i id_map.task_types)
          taches_interdites
      in
      let wanted_tasks =
        CCRAL.of_list_map
          ~f:(fun i -> Int.Map.find i id_map.task_types)
          favorite_quests
      in
      let unwanted_tasks =
        CCRAL.of_list_map
          ~f:(fun i -> Int.Map.find i id_map.task_types)
          constrained_quests
      in
      let forbidden_places =
        CCRAL.of_list_map ~f:(fun i -> Int.Map.find i id_map.places) []
      in
      let arrival =
        Option.map
          (fun arrival ->
            Zoned_datetime.from_duration @@ Duration.from_seconds arrival
            |> Zoned_datetime.change_timezone ~tz:infos.timezone)
          date_d_arrivee
      in
      let departure =
        Option.map
          (fun departure ->
            Zoned_datetime.from_duration @@ Duration.from_seconds departure
            |> Zoned_datetime.change_timezone ~tz:infos.timezone)
          date_de_depart
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
              {
                Rich.Availability.status = Available options.desired_time_bonus;
                slot;
              })
        in
        let worse =
          daily_spec horaires_contraints
          |> List.map ~f:(fun slot ->
              {
                Rich.Availability.status =
                  Available (-1 * options.undesired_time_malus);
                slot;
              })
        in
        CCRAL.of_list
          (List.concat [ unavailable; best; worse; ponctually_unavailable ])
      in
      let v =
        Rich.Volunteer.make ~id:ids ?public_name ~name ~daily_workload
          ~proficiencies ~forbidden_tasks ~forbidden_places ~wanted_tasks
          ~unwanted_tasks ~availabilities ?arrival ?departure ()
      in
      ({ id_map with volunteers = Int.Map.add id v id_map.volunteers }, v)
    in
    let id_map, vols =
      List.fold_left_map ~f:convert_volunteers ~init:id_map vols
    in
    (id_map, CCRAL.of_list vols)
  in
  let () =
    (* We set friends and ennemis in a second pass *)
    List.iter vols ~f:(fun { Benevole.id; amis; ennemis; _ } ->
        let v = Int.Map.find id id_map.volunteers in
        let friends =
          List.filter_map
            ~f:(fun id ->
              match Int.Map.find_opt id id_map.volunteers with
              | None -> None
              | Some v -> Some v.id)
            amis
        in
        let ennemis =
          List.filter_map
            ~f:(fun id ->
              match Int.Map.find_opt id id_map.volunteers with
              | None -> None
              | Some v -> Some v.id)
            ennemis
        in
        Rich.Volunteer.set_friends v friends;
        Rich.Volunteer.set_ennemis v ennemis)
  in
  let id_map, quests =
    let convert_quests id_map
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
      let ids = Rich.id_of_int id in
      let task_type = Int.Map.find_opt type_ id_map.task_types in
      let place = Int.Map.find_opt lieu id_map.places in
      let assigned_volunteers =
        List.filter_map
          ~f:(fun i -> Int.Map.find_opt i id_map.volunteers)
          benevoles_assignes
        |> CCRAL.of_list
      in
      let slot =
        make_spec ~rec_flag:recurrence ~days:jours ~start:date_et_heure_de_debut
          ~duration_h:duree_heures ~end_date:fin_de_recurrence
      in
      let v =
        Rich.Quest.make ~id:ids ~name ?task_type ?place ~slot
          ~required_volunteers ~assigned_volunteers ()
      in
      ({ id_map with quests = Int.Map.add id v id_map.quests }, v)
    in
    let id_map, quests =
      List.fold_left_map ~f:convert_quests ~init:id_map quests
    in
    (id_map, CCRAL.of_list quests)
  in
  ( id_map,
    { Rich.Planning.options; infos; places; task_types; volunteers; quests } )
