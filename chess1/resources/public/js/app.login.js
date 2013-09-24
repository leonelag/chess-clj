var App = App || {};

App.Login = {
  otherUsersLoggedIn: function() {
    $.getJSON('app/users.json', function(data) {
      var div = $('div#users-logged-in');

      if (data.users.length <= 0) {
        div.empty();
        $('<p>No users logged in.</p>').appendTo(div);
        return;
      }

      var i;
      var users = [];

      for (i = 0; i < data.users.length; i++) {
        if (data.users[i] != App.Login.username) {
          users.push(data.users[i]);
        }
      }

      var items = [];
      div.empty();

      // aux fn to make a link
      var inviteLnk = function(username, gameType, txt) {
        var a = $("<a href='#'>" + txt + "</a>");
        a.click(function() {
          App.Login.invite(username, gameType);
        });
        return a;
      }

      for (i = 0; i < users.length; i++) {
        var username = data.users[i];
        var li = $('<li>' + username + '</li>');
        inviteLnk(username, 'classic',    'Invite to classic game').appendTo(li);
        inviteLnk(username, 'chaturanga', 'Invite to Chaturanga game').appendTo(li);
        inviteLnk(username, 'blitzkrieg', 'Invite to BlitzKrieg game').appendTo(li);
        items.push(li);
      }

      var ul = $('<ul/>', {
        'class': 'my-new-list',
      });
      for (i = 0; i < items.length; i++) {
        li.appendTo(ul);
      }
      ul.appendTo(div);
    });
  },

  invite: function(username, gameType) {
    console.log("Invite: " + username + " to " + gameType);
  },

  /**
   * Username of the user currently logged in or null if the user
   * is not logged in.
   */
  username: null,

  userLoggedIn: function() {
    $.getJSON('app/login.json', function(data) {
      App.Login.username = data.logged;
      var loginLink  = $('li#loginLink'),
          logoutLink = $('li#logoutLink'),
          loginTxt   = $('li#loginTxt a');
      if (App.Login.username) {
        loginLink.hide();
        loginTxt.text('Welcome, ' + App.Login.username);
        loginTxt.show();
        logoutLink.show();
      } else {
        loginLink.show();
        loginTxt.text('');
        loginTxt.hide('');
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
           function() { App.Login.userLoggedIn(); })
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
    App.Login.userLoggedIn();
  },
};
