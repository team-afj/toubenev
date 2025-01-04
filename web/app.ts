import "@event-calendar/core/index.css";
import Calendar from "@event-calendar/core";
import List from "@event-calendar/list";
import TimeGrid from "@event-calendar/time-grid";
import ResourceTimeline from "@event-calendar/resource-timeline";
import ResourceTimeGrid from "@event-calendar/resource-time-grid";
import Interaction from "@event-calendar/interaction";
import data from "./results.json";

var calendarElement = document.getElementById("calendar");

// async function getData() {
//   const response = await fetch("./events.ics");
//   const data = await response.arrayBuffer()
//   console.log(data)
//   calendarInstance.import([new File([data], "cal", { type : "ics"})])
// }

let volunteers = data["volunteers"].map((v) => ({
  id: `v_${v.id}`,
  title: v.pseudo,
}));
let places = data["places"].map((v) => ({ id: `p_${v.id}`, title: v.name }));
let types = data["quest_types"].map((v) => ({
  id: `qt_${v.id}`,
  title: v.name,
}));

let bénévoles = { id: "volunteers", title: "Bénévoles", children: volunteers };
let lieux = { id: "places", title: "Lieux", children: places };
let types_de_quêtes = {
  id: "types",
  title: "Types de quêtes",
  children: types,
};
let resources = [bénévoles, lieux, types_de_quêtes];

let events = data["quests"]
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

let ec = new Calendar({
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
      resources,
      events,
      filterEventsWithResources: true,
    },
  },
});

console.log(events);
