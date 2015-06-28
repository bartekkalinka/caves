var wsUri = "ws://localhost:8080/";
var output;
function init() {
    output = document.getElementById("output");
    testWebSocket();
}
function testWebSocket() {
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
function onOpen(evt) {
}
function onClose(evt) {
}
function onMessage(evt) {
   $("#msg").html(evt.data)
}
function onError(evt) {
    writeToScreen('<span style="color: red;">ERROR:</span> ' + evt.data);
}
function doSend(message) {
    websocket.send(message);
}
function writeToScreen(message) {
    var pre = document.createElement("p");
    pre.style.wordWrap = "break-word";
    pre.innerHTML = message;
    output.appendChild(pre);
}
window.addEventListener("load", init, false);

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
window.addEventListener( "keydown", doKeyDown, true);