var $;
var App = {
  otherUsersLoggedIn: function() {
    $.getJSON('app/users.json', function(data) {
      var items = [];
      $('div#users-logged-in').empty();
      if (data.users.length > 0) {
        for (var i = 0; i < data.users.length; i++) {
          items.push('<li>' + data.users[i] + '</li>');
        }
        $('<ul/>', {
          'class': 'my-new-list',
           html: items.join('')
        }).appendTo('#users-logged-in');
      } else {
        $('<p>No users logged in.</p>').appendTo('#users-logged-in');
      }
    });
  },

  userLoggedIn: function() {
    $.getJSON('app/login.json', function(data) {

      console.log("UserLoggedIn: " + JSON.stringify(data, null, 2));

      var username = data.logged;
      var loginLink = $('span#loginLink');
      var logoutLink = $('span#logoutLink');
      if (username) {
        loginLink.text('Welcome, ' + username);
        logoutLink.show();
      } else {
        loginLink.text('Login');
        logoutLink.hide();
      }
    });
  },

  divLoginVisible: false,
  toggleLoginDialog: function() {
    if (this.divLoginVisible) {
      $('div#divLogin').hide();
    } else {
      $('div#divLogin').show();
      $('input#username').focus();
    }
    this.divLoginVisible = !this.divLoginVisible;
  },

  login: function() {
    $.post('app/login',
           { username: $('input#username').val(),
             password: $('input#password').val() },
           function() { App.userLoggedIn(); })
     .done(function(data) {
      var r = data;
      var p = $('p#loginResult');
      var divLogin = $('div#divLogin');
      p.removeAttr('class');
      p.show();
      if (r.status == 'ok') {
        p.addClass('app-msg-success');
        p.text("Login successful!");
        divLogin.fadeOut();
      } else {
        p.addClass('app-msg-failure');
        p.text("Login failed!");
      }
    })
    .fail(function(data, statusText) {
      console.log('Error logging in. Status :' + statusText);
    });
  },

  logout: function() {
    $.post('app/logout');
    App.userLoggedIn();
  },

  // functions executed periodically
  heartbeatFns: [],
  heartbeat: function() {
    for (var i = 0; i < App.heartbeatFns.length; i++) {
      App.heartbeatFns[i]();
    }
  }
};

$(document).ready(function() {
  // checking users logged in.
  App.heartbeatFns.push(App.otherUsersLoggedIn);

  var delayMs = 2000;
  setInterval(App.heartbeat, delayMs);

  App.userLoggedIn();
});
