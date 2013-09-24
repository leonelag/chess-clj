$(document).ready(function() {
  // checking users logged in.
  App.heartbeatFns.push(App.Login.otherUsersLoggedIn);

  var delayMs = 2000;
  setInterval(App.heartbeat, delayMs);

  App.Login.userLoggedIn();
});
