define( ['scripts/globals'], function (glob) {
    var canvas = document.getElementById("canv");
    var ctx = canvas.getContext("2d");

    // DRAWING MODULE
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
    function squareSize() {
      return glob.size * glob.tilesize
    }
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
        return glob.shapeTab[dx][dy];
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
    function isShapeTileSet(shape, j, i) {
      return shape[j][i] === "1";
    }
    function drawShape(dx, dy) {
      clearSquare(dx, dy);
      ctx.fillStyle = "rgb(255,0,0)";
      var shape = getSquareShape(dx, dy);
      var offset = getSquareOffset(dx, dy);
      var detailScale = Math.pow(2, getSquareDetail(dx, dy));
      var size = glob.size * detailScale;
      var tilesize = glob.tilesize / detailScale;
      for(i=0; i<size; i+=1) {
        for(j=0; j<size; j+=1) {
          if(isShapeTileSet(shape, j, i)) {
            ctx.fillRect(
              offset[0] + tilesize * j,
              offset[1] + tilesize * i, tilesize, tilesize
            );
          }
        }
      }
    }

    return {
        "initshapeTab" : initshapeTab,
        "saveSquare" : saveSquare,
        "clearCanvas" : clearCanvas,
        "drawSquare" : drawSquare,
        "drawShape" : drawShape
    };
});