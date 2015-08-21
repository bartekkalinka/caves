"use strict";

require.config({
    urlArgs: "bust=" + (new Date()).getTime()
});

requirejs(['scripts/globals', 'scripts/draw'], function(glob, draw) {
$(document).ready(function() {
var wsUri = "ws://localhost:8080/game?name=aaa";
var websocket;

// INIT MODULE
function init() {
    draw.initGrid(5);
    initWebSocket();
}
function initWebSocket() {

    websocket = new WebSocket(wsUri);
    websocket.onopen = function(evt) {
        onOpen(evt)
    };
    websocket.onclose = function(evt) {
        onClose(evt)
    };
    websocket.onmessage = function(evt) {
        onMessage(evt)
    };
    websocket.onerror = function(evt) {
        onError(evt)
    };
}
// WEBSOCKET EVENTS MODULE
function onOpen(evt) {
}
function onClose(evt) {
}
function onMessage(evt) {
   var obj = JSON.parse(evt.data)
   draw.clearCanvas();
   $("#debug").html(obj.shapes.length)
   var shape
   var i
   for(i=0; i<obj.shapes.length; i+=1) {
     shape = obj.shapes[i];
     draw.saveShape(shape.tiles, shape.dx, shape.dy);
     draw.drawShape(shape.dx - 1, shape.dy - 1, 0 - obj.screen.x, 0 - obj.screen.y)
   }
}
function onError(evt) {
}
function doSend(message) {
    websocket.send(message);
}

// USER INPUT MODULE
function doKeyDown(e) {
  switch (e.keyCode)
  {
  case 37:
      doSend("left");
      break;
  case 38:
      doSend("up");
      break;
  case 39:
      doSend("right");
      break;
  case 40:
      doSend("down");
      break;
  }
}
window.addEventListener("keydown", doKeyDown, true);
init();
});
});