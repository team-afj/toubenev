include ContainersLabels
module Calendar = CalendarLib.Calendar

let parse_date s = ISO8601.Permissive.datetime s |> Calendar.from_unixfloat

let print_date d =
  Calendar.to_unixfloat d |> ISO8601.Permissive.string_of_datetime
