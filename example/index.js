'use strict';

const Asteroid = require('asteroid').createClass();
const WebSocket = require('ws');

const endpoint = process.argv[2];
if(!endpoint) {
  console.error("Usage: node index.js <websocket url>");
  process.exit(1);
}

// Connect to a Meteor backend
const asteroid = new Asteroid({
  endpoint: endpoint,
  SocketConstructor: WebSocket,
});

asteroid.connect();

// Use real-time collections
asteroid.subscribe("tasksPublication");

asteroid.on("error", function(event) {
  console.log(event);
});

asteroid.ddp.on("added", function(event) {
    console.log('added', event);
});

asteroid.ddp.on("removed", function(event) {
    console.log('removed', event);
});

asteroid.ddp.on("changed", function(event) {
    console.log('changed', event);
});

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
