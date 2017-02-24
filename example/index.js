'use strict';

const Asteroid = require('asteroid').createClass();
const WebSocket = require('ws');

console.log('siema');
// Connect to a Meteor backend
const asteroid = new Asteroid({
    endpoint: "ws://localhost:3008/websocket",
SocketConstructor: WebSocket,
    
});

asteroid.connect();

console.log('created');

// Use real-time collections
asteroid.subscribe("tasksPublication");

asteroid.on("error", function(event) {
  console.log(event);
});

asteroid.ddp.on("added", function(event) {
    console.log('added', event);
});

// Login
//asteroid.loginWithPassword({username, email, password});

// Call method and use promises
asteroid.call("newUser")
    .then(result => {
        console.log("Success");
        console.log(result);
    })
    .catch(error => {
        console.log("Error");
        console.error(error);
    });
