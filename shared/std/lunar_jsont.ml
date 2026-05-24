module Weekday = struct
  include Lunar.Weekday

  let jsont : t Jsont.t =
    Jsont.enum
      [
        ("Mon", Mon);
        ("Tue", Tue);
        ("Wed", Wed);
        ("Thu", Thu);
        ("Fri", Fri);
        ("Sat", Sat);
        ("Sun", Sun);
      ]

  let to_fr_short_string = function
    | Mon -> "Lun"
    | Tue -> "Mar"
    | Wed -> "Mer"
    | Thu -> "Jeu"
    | Fri -> "Ven"
    | Sat -> "Sam"
    | Sun -> "Dim"

  let to_fr_long_string = function
    | Mon -> "Lundi"
    | Tue -> "Mardi"
    | Wed -> "Mercredi"
    | Thu -> "Jeudi"
    | Fri -> "Vendredi"
    | Sat -> "Samedi"
    | Sun -> "Dimanche"

  let to_intl_short_string lang t =
    match lang with `Fr -> to_fr_short_string t

  let to_intl_long_string lang t = match lang with `Fr -> to_fr_long_string t

  module Set = struct
    include Set

    let jsont = Jsont.map ~dec:of_list ~enc:to_list (Jsont.list jsont)
  end
end

module Month = struct
  include Lunar.Month

  let jsont : t Jsont.t =
    Jsont.enum
      [
        ("Jan", Jan);
        ("Feb", Feb);
        ("Mar", Mar);
        ("Apr", Apr);
        ("May", May);
        ("Jun", Jun);
        ("Jul", Jul);
        ("Aug", Aug);
        ("Sep", Sep);
        ("Oct", Oct);
        ("Nov", Nov);
        ("Dec", Dec);
      ]

  let to_fr_short_string = function
    | Jan -> "Jan"
    | Feb -> "Fev"
    | Mar -> "Mar"
    | Apr -> "Avr"
    | May -> "Mai"
    | Jun -> "Juin"
    | Jul -> "Juil"
    | Aug -> "Aou"
    | Sep -> "Sep"
    | Oct -> "Oct"
    | Nov -> "Nov"
    | Dec -> "Dec"

  let to_fr_long_string = function
    | Jan -> "Janvier"
    | Feb -> "Février"
    | Mar -> "Mars"
    | Apr -> "Avril"
    | May -> "Mai"
    | Jun -> "Juin"
    | Jul -> "Juillet"
    | Aug -> "Aout"
    | Sep -> "Septembre"
    | Oct -> "Octobre"
    | Nov -> "Novembre"
    | Dec -> "Décembre"

  let to_intl_short_string lang t =
    match lang with `Fr -> to_fr_short_string t

  let to_intl_long_string lang t = match lang with `Fr -> to_fr_long_string t

  module Set = struct
    include Set

    let jsont = Jsont.map ~dec:of_list ~enc:to_list (Jsont.list jsont)
  end
end

module Duration = struct
  include Lunar.Duration

  let from_hours_f f =
    let minutes = f *. 60. in
    from_minutes (Float.to_int minutes)

  let to_minutes t =
    let hours, minutes, seconds = hms t in
    Stdlib.((hours * 60) + minutes + (seconds / 60))

  let jsont : t Jsont.t = Jsont.map ~dec:from_int64 ~enc:to_int64 Jsont.int64
end

module Timezone = struct
  include Lunar.Timezone

  let jsont : t Jsont.t =
    Jsont.map ~dec:from_string_exn ~enc:to_string Jsont.string
end

module Date = struct
  include Lunar.Date

  let to_intl_long_string lang t =
    let weekday = weekday t in
    let day_of_month = day_of_month t in
    let month = month t in
    String.concat " "
      [
        Weekday.to_intl_long_string lang weekday;
        string_of_int day_of_month;
        Month.to_intl_long_string lang month;
      ]

  let jsont : t Jsont.t =
    Jsont.map ~dec:from_string_exn ~enc:to_string Jsont.string
end

module Time = struct
  include Lunar.Time

  let jsont : t Jsont.t =
    Jsont.map ~dec:from_string_exn ~enc:to_string Jsont.string
end

module Datetime = struct
  include Lunar.Datetime

  let jsont = Jsont.string |> Jsont.map ~dec:from_string_exn ~enc:to_string
end

module Zoned_datetime = struct
  include Lunar.Zoned_datetime

  let jsont = Jsont.string |> Jsont.map ~dec:from_string_exn ~enc:to_string
  let local_date t = to_local_datetime t |> Datetime.date
  let local_time t = to_local_datetime t |> Datetime.time
  let to_local_duration t = to_local_datetime t |> Datetime.to_duration
  let to_utc_duration t = to_utc t |> Datetime.to_duration
end
