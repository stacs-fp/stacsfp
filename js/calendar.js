var SHORT_MONTHS =
    ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov"];
var GAPIKEY = "AIzaSyCMreNwj9peTFumFUWgNHxVALc4pXNm0DY";

function updateEvents(es) {
    var events = $('<ul/>')
    $.each(es.items, function(i, e) {
        var date = new Date(e.start.dateTime);
        var li = $('<li/>')
            .addClass("row")
            .attr("itemtype", "https://schema.org/Event")
            .attr("itemscope", "")
            .appendTo(events);
        $("<div/>")
            .addClass("col-md-2 post-date")
            .html(("0" + date.getDate()).slice(-2) + "<br>" + SHORT_MONTHS[date.getMonth()])
            .appendTo(li);
        $("<div/>")
            .addClass("col-md-10 post-post")
            .append(
                $("<h2><a href=\"" + e.url + "\">" + e.summary + "</a></h2>"),
                $("<div>Where: " + e.location + "</div>"),
                $("<div>When: " + date.getUTCHours() + ":" + date.getUTCMinutes() + "</div>"),
                $("<p>" + e.description + "</p>"))
            .appendTo(li);
    });
    $("#eventlist").html(events);
}

function initEvents() {
    var date = new Date();
    $.ajax({
        method: 'GET',
        url: "https://www.googleapis.com/calendar/v3/calendars/stacsfp@gmail.com/events?key=" + GAPIKEY,
        dataType: 'jsonp',
        data: {
            'showDeleted': false,
            'singleEvents': true,
            'timeMin': (new Date(date.getFullYear(), date.getMonth(), 1)).toISOString(),
            'timeMax': (new Date(date.getFullYear(), date.getMonth() + 1, 0)).toISOString(),
            'orderBy': 'startTime'
        },
        success: updateEvents
    });
}

initEvents();
