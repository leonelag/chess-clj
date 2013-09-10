$(document).ready(function() {
  var App = Ember.Application.create();

  App.Router.map(function() {
  });

  App.IndexRoute = Ember.Route.extend({
    model: function() {
      return ['red', 'yellow', 'blue'];
    }
  });

  App.loginView = Ember.View.extend({
    tagName: 'li',
    templateName: 'loginLink'
  });
  App.loginView.create().appendTo('#loginContainer');
});
