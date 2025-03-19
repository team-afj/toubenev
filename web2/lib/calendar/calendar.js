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
