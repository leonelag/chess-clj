var App = {
  // functions executed periodically
  heartbeatFns: [],
  heartbeat: function() {
    for (var i = 0; i < App.heartbeatFns.length; i++) {
      App.heartbeatFns[i]();
    }
  }
};
