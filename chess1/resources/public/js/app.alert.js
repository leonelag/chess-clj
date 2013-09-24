var App = App || {};

var divAlerts = $('div#alerts');

App.Alert = {
  info: function(title, txt) {
    var div = $("<div class='alert alert-info'>" +
                "<strong>" + title + "</strong>" + " " +
                txt +
                "</div>");
    div.appendTo(divAlerts);
    div.delay(2000).fadeOut({
      duration: 2000
    });
  }
};
