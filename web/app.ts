import "@event-calendar/core/index.css";
import Calendar from "@event-calendar/core";
import List from "@event-calendar/list";
import TimeGrid from "@event-calendar/time-grid";
import ResourceTimeline from "@event-calendar/resource-timeline";
import ResourceTimeGrid from "@event-calendar/resource-time-grid";
import Interaction from "@event-calendar/interaction";
import static_data from "./results.json";

//// DATA

type volunteer = { id: string; title: string };
const volunteers: Map<string, volunteer> = new Map(
  static_data["volunteers"].map(({ id, pseudo }) => [
    `v_${id}`,
    {
      id: `v_${id}`,
      title: pseudo,
    },
  ])
);
// const volunteers_list = Array.from(volunteers, ([id, title]) => ({
//   id,
//   title,
// }));

//// STATE
type state = {
  active_volunteer: volunteer | undefined;
  no_nested_resources: boolean;
};
const state: state = {
  active_volunteer: undefined,
  no_nested_resources: false,
};

//// CALENDAR

const calendarElement = document.getElementById("calendar");

// async function getData() {
//   const response = await fetch("./events.ics");
//   const data = await response.arrayBuffer()
//   console.log(data)
//   calendarInstance.import([new File([data], "cal", { type : "ics"})])
// }

let events = static_data["quests"]
  .map((v) => ({
    id: `q_${v.id}`,
    allDay: false,
    start: new Date(v.start),
    end: new Date(v.end),
    title: v.name,
    editable: false,
    resourceIds: v.types
      .map((id) => `qt_${id}`)
      .concat(v.volunteers.map((id) => `v_${id}`))
      .concat(`p_${v.place}`),
  }))
  .sort((a, b) => a.start.valueOf() - b.start.valueOf());

let date = events[0].start;

let last_date = events[events.length - 1].end;
const one_day = 24 * 60 * 60 * 1000;
let duration = {
  days: Math.ceil((last_date.getTime() - date.getTime()) / one_day),
};

console.log(duration);

let calendar = new Calendar({
  target: calendarElement,
  props: {
    plugins: [Interaction, TimeGrid, List, ResourceTimeline, ResourceTimeGrid],
    options: {
      view: "timeGridWeek",
      headerToolbar: {
        start: "title",
        center:
          "timeGridWeek,listYear,resourceTimelineDay,resourceTimelineMonth,resourceTimeGridDay",
        end: "today prev,next",
      },
      date,
      duration,
      filterEventsWithResources: true,
    },
  },
});

const update_calendar = ({ active_volunteer, no_nested_resources }) => {
  // RESOURCES

  type resource = { id: string; title: string };

  let places: resource[] = static_data["places"].map((v) => ({
    id: `p_${v.id}`,
    title: v.name,
  }));
  let types: resource[] = static_data["quest_types"].map((v) => ({
    id: `qt_${v.id}`,
    title: v.name,
  }));

  const volunteers_: resource[] = active_volunteer
    ? Array.from(volunteers.values()).filter(
        (v) => v.id === active_volunteer!.id
      )
    : Array.from(volunteers.values());

  let bénévoles = {
    id: "volunteers",
    title: "Bénévoles",
    children: volunteers_,
  };
  let lieux = { id: "places", title: "Lieux", children: places };
  let types_de_quêtes = {
    id: "types",
    title: "Types de quêtes",
    children: types,
  };
  let resources = no_nested_resources
    ? [...volunteers_, ...places, ...types]
    : [bénévoles, lieux, types_de_quêtes];

  // EVENTS

  let filtered_events = active_volunteer
    ? events.filter((ev) =>
        ev.resourceIds.some((res) => res === active_volunteer!.id)
      )
    : events;

  calendar.setOption("resources", resources);
  calendar.setOption("events", filtered_events);
};

console.log(events);

/// Volunteer select
const set_active_volunteer = (id) => {
  if (id === "none") state.active_volunteer = undefined;
  else state.active_volunteer = volunteers.get(id);

  update_calendar(state);
};

const select_input = document.getElementById(
  "volunteer_select"
) as HTMLSelectElement;

volunteers.forEach((v) => select_input.add(new Option(v.title, v.id)));

select_input.onchange = (ev) => {
  const id = (ev.target! as HTMLSelectElement).value;
  set_active_volunteer(id);
};

/// Keep track of calendar interactions
calendar.setOption("eventClick", (info) => console.log("click", info));
calendar.setOption("viewDidMount", ({ type }) => {
  const prev_value = state.no_nested_resources;
  state.no_nested_resources = type === "resourceTimeGridDay";
  if (state.no_nested_resources != prev_value) update_calendar(state);
});

/// Update the calendar for the first time
update_calendar(state);
