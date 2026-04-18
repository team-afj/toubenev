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

  module Set = struct
    include Set

    let jsont = Jsont.map ~dec:of_list ~enc:to_list (Jsont.list jsont)
  end
end

module Duration = struct
  include Lunar.Duration

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
