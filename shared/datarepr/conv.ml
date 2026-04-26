open Normal

let normalize (data : Rich.Planning.t) =
  let volunteers =
    CCRAL.to_list data.volunteers
    |> List.map ~f:(Volunteer.normalize data.infos)
    |> Volunteers.of_list
  in
  let quests =
    CCRAL.to_list data.quests
    |> List.concat_map ~f:(Quest.normalize data.infos volunteers)
    |> Quests.of_list
  in
  { Normal.volunteers; quests }
