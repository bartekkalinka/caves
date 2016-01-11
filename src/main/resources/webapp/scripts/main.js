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
   var obj = JSON.parse(evt.data);
   glob.tilePixels = obj.tilePixels;
   draw.clearCanvas();
   $("#debug").html(obj.tilePixels);
   draw.drawShape(obj.shape.tiles);
   draw.drawPlayer(obj.player.x, obj.player.y);
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
  case 107: //+
      doSend("zoomin");
      break;
  case 109: //-
      doSend("zoomout");
      break;
  }
}
window.addEventListener("keydown", doKeyDown, true);
init();
});
});