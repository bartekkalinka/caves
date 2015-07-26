$(document).ready(function() {
var wsUri = "ws://localhost:8080/game?name=aaa";
var canvas = document.getElementById("canv");
var ctx = canvas.getContext("2d");

// INIT MODULE
function init() {
    initshapeTab(2);
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
   clearCanvas();
   drawSquare(obj.player.x * 20, 20, "rgb(255,0,0)");
   drawSquare(obj.other.x * 20, obj.other.y * 20, "rgb(0,0,255)")
   $("#debug").html(obj.shape.length + " " + obj.shape[0])
   saveSquare(obj.shape, 0, 0);
   drawShape(0, 0);
}
function onError(evt) {
}
function doSend(message) {
    websocket.send(message);
}
// DRAWING MODULE
var glob = {
    tilesize : 64,
    size : 6,
    detail : 3,
    shapeTab : []
  };
// drawing single squares
function clearCanvas() {
    ctx.fillStyle = "rgb(0,0,0)";
    ctx.fillRect(0, 0, canvas.width, canvas.height);
}
function drawSquare(x, y, color) {
    ctx.fillStyle = color;
    ctx.fillRect(x, y, 20, 20);
}
// drawing terrain
// TODO move to separate file
function initshapeTab(n) {
    glob.shapeTab = new Array();
    for(i = 0; i < n; i++) {
        glob.shapeTab[i] = new Array();
    }
}
function saveSquare(shapeData, dx, dy) {
    var obj = shapeData;
    obj.detail = glob.detail;
    glob.shapeTab[dx][dy] = obj;
}
function getSquareShape(dx, dy) {
    return glob.shapeTab[dx][dy].noise;
}
function getSquareDetail(dx, dy) {
    return glob.shapeTab[dx][dy].detail;
}
function getSquareOffset(dx, dy) {
    return [dx * squareSize(), dy * squareSize()];
}
function clearSquare(dx, dy) {
    var offset = getSquareOffset(dx, dy);
    ctx.fillStyle = "rgb(0,0,0)";
    ctx.fillRect(offset[0], offset[1], squareSize(), squareSize());
}
function drawShape(dx, dy) {
    clearSquare(dx, dy);
    ctx.fillStyle = "rgb(255,0,0)";
    var shape = getSquareShape(dx, dy);
    var offset = getSquareOffset(dx, dy);
    var detailScale = Math.pow(2, getSquareDetail(dx, dy))
    var size = glob.size * detailScale
    var tilesize = glob.tilesize / detailScale
    for(i=0; i<size; i+=1) {
      for(j=0; j<size; j+=1) {
        if(shape[j][i] >= 500) {
          ctx.fillRect(
                  offset[0] + tilesize * j,
                  offset[1] + tilesize * i, tilesize, tilesize
          );
        }
      }
    }
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