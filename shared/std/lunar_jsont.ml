module Utils = struct
  let lpad ?(char = '0') ~size n =
    let pre = if n < 0 then "-" else "" in
    let str = string_of_int (abs n) in
    let len = String.length str in
    pre ^ if len >= size then str else String.make (size - len) char ^ str
end

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
    | Aug -> "Août"
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

  (** [from_hours_f f] rounds [f] into minutes. *)
  let from_hours_f f =
    let minutes = f *. 60. in
    from_minutes Float.(round minutes |> to_int)

  let to_minutes t =
    let hours, minutes, seconds = hms t in
    Stdlib.((hours * 60) + minutes + (seconds / 60))

  let to_string t =
    let h, m, _s = hms t in
    Utils.lpad ~size:2 h ^ ":" ^ Utils.lpad ~size:2 m

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

  let to_short_string ?(format = `DDMMYYYY) t =
    let day_of_month = day_of_month t |> Utils.lpad ~size:2 in
    let month = month t |> Month.to_int |> Utils.lpad ~size:2 in
    match format with
    | `DDMM -> day_of_month ^ "/" ^ month
    | `DDMMYYYY ->
        let year = year t |> string_of_int in
        day_of_month ^ "/" ^ month ^ "/" ^ year

  let jsont : t Jsont.t =
    Jsont.map ~dec:from_string_exn ~enc:to_string Jsont.string

  module Map = struct
    type date = t

    include Map

    type 'a binding = (date[@jsont jsont]) * 'a [@@deriving jsont]

    let jsont v =
      Jsont.map ~dec:of_list ~enc:to_list (Jsont.list (binding_jsont v))
  end
end

module Time = struct
  include Lunar.Time

  let jsont : t Jsont.t =
    Jsont.map ~dec:from_string_exn ~enc:to_string Jsont.string

  let to_string ?(format = `HHMMSS) t =
    match format with
    | `HHMMSS -> to_string t
    | `HHMM ->
        let hour = hour t |> Utils.lpad ~size:2 in
        let minute = minute t |> Utils.lpad ~size:2 in
        hour ^ ":" ^ minute
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
  let to_local_minutes t = to_local_duration t |> Duration.to_minutes
  let to_utc_duration t = to_utc t |> Datetime.to_duration
end
