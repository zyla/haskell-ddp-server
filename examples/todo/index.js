'use strict';

var Asteroid = require('asteroid').createClass();

var endpoint = 'ws://localhost:3009/websocket';

// Connect to a Meteor backend
var asteroid = new Asteroid({
  endpoint: endpoint,
  SocketConstructor: WebSocket,
});

asteroid.connect();

asteroid.subscribe("todos");

asteroid.on("error", function(event) {
  console.log(event);
});

asteroid.ddp.on("added", function(event) {
  console.log('added', event);
  var li = document.createElement('li');
  li.innerText = event.fields.todo;
  document.getElementById('todos').appendChild(li);
});

asteroid.ddp.on("removed", function(event) {
  console.log('removed', event);
});

asteroid.ddp.on("changed", function(event) {
  console.log('changed', event);
});

document.getElementById('form').onsubmit = function() {
  var todo = document.getElementById('todo').value;
  asteroid.call('addTodo', todo); 
  return false;
};
