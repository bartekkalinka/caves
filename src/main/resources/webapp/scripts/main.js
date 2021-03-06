"use strict";

require.config({
    urlArgs: "bust=" + (new Date()).getTime()
});

requirejs(['scripts/globals', 'scripts/draw', 'scripts/debug'], function(glob, draw, debug) {
$(document).ready(function() {
var wsUri = "ws://" + window.location.hostname + ":" + window.location.port + "/game?name=aaa";
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
  doSend("init");
}
function onClose(evt) {
}
function drawOtherPlayers(otherPlayers) {
  var i, op;
  for(i = 0; i < otherPlayers.length; i++) {
    op = otherPlayers[i];
    draw.drawPlayer(op[0], op[1], "game.singleplayer.FaceDirection.Straight");
  }
}
function onMessage(evt) {
   var obj = JSON.parse(evt.data);
   glob.tilePixels = obj.tilePixels;
   draw.clearCanvas();
   draw.drawShape(obj.shape.tiles, obj.offset);
   draw.drawPlayer(obj.player.x, obj.player.y, obj.faceDirection[0]);
   drawOtherPlayers(obj.otherPlayers);
   debug.handleMessage(obj.debugInfo);
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
      doSend("leftKeyDown");
      break;
  case 38:
      doSend("upKeyDown");
      break;
  case 39:
      doSend("rightKeyDown");
      break;
  case 40:
      doSend("downKeyDown");
      break;
  case 107: //+
      doSend("zoomin");
      break;
  case 109: //-
      doSend("zoomout");
      break;
  case 68: //d
      debug.toggleShowFlag();
      break;
  case 80: //p
      debug.togglePause();
      break;
  case 72: //h
      doSend("horizontal");
      break;
  case 86: //v
      doSend("vertical");
      break;
  }
}
function doKeyUp(e) {
  switch (e.keyCode)
  {
  case 37:
      doSend("leftKeyUp");
      break;
  case 38:
      doSend("upKeyUp");
      break;
  case 39:
      doSend("rightKeyUp");
      break;
  case 40:
      doSend("downKeyUp");
      break;
  }
}
window.addEventListener("keydown", doKeyDown, true);
window.addEventListener("keyup", doKeyUp, true);
init();
});
});