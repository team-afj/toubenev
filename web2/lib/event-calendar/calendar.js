require('@event-calendar/core/index.css');

//Provides: get_calendar const
function get_calendar() {
    if (!window.r_calendar)
        window.r_calendar = require("@event-calendar/core")
    return window.r_calendar.default
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
