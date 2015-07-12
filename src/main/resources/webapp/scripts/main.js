$(document).ready(function() {
var wsUri = "ws://localhost:8080/";
var canvas = document.getElementById("canv");
var ctx = canvas.getContext("2d");

// INIT MODULE
function init() {
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
   $("#debug").html(evt.data)
   var obj = JSON.parse(evt.data)
   clearCanvas();
   drawSquare(obj.player.x * 20, 20, "rgb(255,0,0)");
   drawSquare(obj.other.x * 20, obj.other.y * 20, "rgb(0,0,255)")
}
function onError(evt) {
}
function doSend(message) {
    websocket.send(message);
}

// DRAWING MODULE
function clearCanvas() {
    ctx.fillStyle = "rgb(0,0,0)";
    ctx.fillRect(0, 0, canvas.width, canvas.height);
}
function drawSquare(x, y, color) {
    ctx.fillStyle = color;
    ctx.fillRect(x, y, 20, 20);
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
})