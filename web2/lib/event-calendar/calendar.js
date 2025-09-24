require('@event-calendar/core/index.css');

//Provides: get_calendar const
function get_calendar() {
    if (!window.r_calendar)
        window.r_calendar = require("@event-calendar/core")
    return window.r_calendar.default
}

//Provides: get_day_grid const
function get_day_grid() {
    if (!window.r_calendar_day_grid)
        window.r_calendar_day_grid = require("@event-calendar/day-grid")
    return window.r_calendar_day_grid.default
}

//Provides: get_list const
function get_list() {
    if (!window.r_calendar_list)
        window.r_calendar_list = require("@event-calendar/list")
    return window.r_calendar_list.default
}

//Provides: get_resource_timeline const
function get_resource_timeline() {
    if (!window.r_calendar_resource_timeline)
        window.r_calendar_resource_timeline = require("@event-calendar/resource-timeline")
    return window.r_calendar_resource_timeline.default
}

//Provides: get_resource_timegrid const
function get_resource_timegrid() {
    if (!window.r_calendar_resource_timegrid)
        window.r_calendar_resource_timegrid = require("@event-calendar/resource-time-grid")
    return window.r_calendar_resource_timegrid.default
}


//Provides: get_time_grid const
function get_time_grid() {
    if (!window.r_calendar_time_grid)
        window.r_calendar_time_grid = require("@event-calendar/time-grid")
    return window.r_calendar_time_grid.default
}
