open Std_js
open Lunar_jsont
open Data_repr
open Rich

let utc = Timezone.make ~hour:2 ~min:0
let date y m day = Date.make ~year:y ~month:m ~day () |> Result.get_ok

let event_infos ?(day_start_h = 5) ~start_date ~end_date () =
  let day_start_local = Time.make_exn ~hour:day_start_h ~min:0 ~sec:0 () in
  Event_infos.
    {
      name = "Test";
      kind = Finite { start_date; end_date };
      timezone = utc;
      day_start_local;
      minimum_transfer_time = Duration.zero;
      daily_break_duration = Duration.zero;
    }

let weekly_spec ~start_h ?(dur_h = 1) ?first_day ?last_day weekdays =
  let start = Time.make_exn ~hour:start_h ~min:0 ~sec:0 () in
  let duration = Duration.from_hours dur_h in
  Time_spec.make
    (Weekly (Weekday.Set.of_list weekdays))
    ?first_day ?last_day start duration

let expand = Conv.expand_time_spec
let mon22 = date 2026 Month.Jun 22
let tue23 = date 2026 Month.Jun 23
let wed24 = date 2026 Month.Jun 24
let thu25 = date 2026 Month.Jun 25
let mon29 = date 2026 Month.Jun 29

let slot_dates slots =
  let open Normal.Time_slot in
  List.map ~f:(fun s -> Zoned_datetime.local_date s.start) slots

let check_dates msg expected slots =
  let got = slot_dates slots in
  Alcotest.(check (list string))
    msg
    (List.map ~f:Date.to_string expected)
    (List.map ~f:Date.to_string got)

(* Handcrafter cases corresponding to real issues *)

(* Event: Mon22 5am -> Wed24
   Task: every Tue 2am starting Tue23 *)
(* Because the task time is < event.day_start_local it used to be missing its
   first occurrence on tue23. *)
let test_weekly_start_day_matches_first_day () =
  let ei = event_infos ~start_date:mon22 ~end_date:wed24 () in
  let spec = weekly_spec ~start_h:2 ~first_day:tue23 [ Weekday.Tue ] in
  check_dates "Task on first task day before event start time." [ tue23 ]
    (expand ei spec)

(* Event: Tue23 5am -> Wed24
   Task: every Tue 2am starting Tue23 *)
(* Same as the previous case but the first day is the event first day so the
   task should not happen before event.day_start_local *)
let test_weekly_start_day_matches_event_start_before_day_start () =
  let ei = event_infos ~start_date:tue23 ~end_date:wed24 () in
  let spec = weekly_spec ~start_h:2 [ Weekday.Tue ] in
  check_dates "Task on first event day before event start time." []
    (expand ei spec)

(* Generated cases, not reviewed *)

(* Weekly recurrence where the event's first day does NOT match: no slot is
   emitted for the first day, and there are no further matching days. *)
let test_weekly_start_day_not_matching_weekday () =
  let ei = event_infos ~start_date:tue23 ~end_date:thu25 () in
  let spec = weekly_spec ~start_h:8 [ Weekday.Mon ] in
  check_dates "event starts on Tuesday, weekly Mon → 0 slots" []
    (expand ei spec)

(* Explicit spec.first_day set to the event start (a Monday): must not
   accidentally push first_day past start_date. *)
let test_weekly_explicit_first_day_equals_start () =
  let ei = event_infos ~start_date:mon22 ~end_date:wed24 () in
  let spec = weekly_spec ~start_h:8 ~first_day:mon22 [ Weekday.Mon ] in
  check_dates "first_day = event start = Mon → included" [ mon22 ]
    (expand ei spec)

(* Event spans two matching weekdays: both must appear in the result. *)
let test_weekly_two_occurrences () =
  let ei = event_infos ~start_date:mon22 ~end_date:mon29 () in
  let spec = weekly_spec ~start_h:8 [ Weekday.Mon ] in
  check_dates "two Mondays in range → 2 slots" [ mon22; mon29 ] (expand ei spec)

(* spec.last_day set to the event's last matching weekday: that day is still
   included (boundary is inclusive). *)
let test_weekly_last_day_boundary_inclusive () =
  let ei = event_infos ~start_date:mon22 ~end_date:mon29 () in
  let spec = weekly_spec ~start_h:8 ~last_day:mon29 [ Weekday.Mon ] in
  check_dates "last_day = Mon29 (inclusive) → 2 slots" [ mon22; mon29 ]
    (expand ei spec)

let () =
  Alcotest.run "expand_time_spec"
    [
      ( "weekly recurrence",
        [
          Alcotest.test_case "start day matches weekday" `Quick
            test_weekly_start_day_matches_first_day;
          Alcotest.test_case "start day matches weekday before day start" `Quick
            test_weekly_start_day_matches_event_start_before_day_start;
          Alcotest.test_case "start day does not match weekday" `Quick
            test_weekly_start_day_not_matching_weekday;
          Alcotest.test_case "explicit first_day equals event start" `Quick
            test_weekly_explicit_first_day_equals_start;
          Alcotest.test_case "two occurrences in range" `Quick
            test_weekly_two_occurrences;
          Alcotest.test_case "last_day boundary inclusive" `Quick
            test_weekly_last_day_boundary_inclusive;
        ] );
    ]
