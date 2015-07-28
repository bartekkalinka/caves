require.config({
    urlArgs: "bust=" + (new Date()).getTime()
});

requirejs(['scripts/globals', 'scripts/draw'], function(glob, draw) {
$(document).ready(function() {
var wsUri = "ws://localhost:8080/game?name=aaa";

// INIT MODULE
function init() {
    draw.initshapeTab(2);
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
   draw.drawSquare(obj.player.x * 20, 20, "rgb(255,0,0)");
   draw.drawSquare(obj.other.x * 20, obj.other.y * 20, "rgb(0,0,255)")
   $("#debug").html(obj.shape.length + " " + obj.shape[0])
   draw.saveSquare(obj.shape, 0, 0);
   draw.drawShape(0, 0);
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
  case 39:
      doSend("right");
      break;
  }
}
window.addEventListener("keydown", doKeyDown, true);
init();
});
});