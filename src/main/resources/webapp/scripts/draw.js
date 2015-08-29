"use strict";

define( ['scripts/globals'], function (glob) {
    var canvas = document.getElementById("canv");
    var ctx = canvas.getContext("2d");

    // DRAWING MODULE
    function clearCanvas() {
        ctx.fillStyle = "rgb(0,0,0)";
        ctx.fillRect(0, 0, canvas.width, canvas.height);
    }
    function shapePixels() {
      return glob.initShapeTiles * glob.baseTilePixels
    }
    function initGrid(gridShapes) {
        var i
        glob.grid = new Array();
        for(i = 0; i < gridShapes; i++) {
            glob.grid[i] = new Array();
        }
    }
    function saveShape(shape, dx, dy) {
        var obj = shape;
        obj.detail = glob.targetDetail;
        glob.grid[dx][dy] = obj;
    }
    function getShape(dx, dy) {
        return glob.grid[dx][dy];
    }
    function getShapeDetail(dx, dy) {
        return glob.grid[dx][dy].detail;
    }
    function getGridPixelsOffset(dx, dy) {
        return [dx * shapePixels(), dy * shapePixels()];
    }
    function clearShapeSquare(dx, dy, x, y) {
      var pixelsOffset = getGridPixelsOffset(dx, dy);
      ctx.fillStyle = "rgb(0,0,0)";
      ctx.fillRect(x + pixelsOffset[0], y + pixelsOffset[1], shapePixels(), shapePixels());
    }
    function isShapeTileSet(shape, j, i) {
      return shape[j][i] === "1";
    }
    function drawShape(dx, dy, x, y) {
      var readdx = dx + 1
      var readdy = dy + 1
      clearShapeSquare(dx, dy, x, y);
      ctx.fillStyle = "rgb(255,0,0)";
      var shape = getShape(readdx, readdy);
      var pixelsOffset = getGridPixelsOffset(dx, dy);
      var detailScale = Math.pow(2, getShapeDetail(readdx, readdy));
      var shapeTiles = glob.initShapeTiles * detailScale;
      var tilePixels = glob.baseTilePixels / detailScale;
      var i, j
      for(i=0; i<shapeTiles; i+=1) {
        for(j=0; j<shapeTiles; j+=1) {
          if(isShapeTileSet(shape, j, i)) {
            ctx.fillRect(
              x + pixelsOffset[0] + tilePixels * j,
              y + pixelsOffset[1] + tilePixels * i,
              tilePixels + 1,
              tilePixels + 1
            );
          }
        }
      }
    }

    return {
        "initGrid" : initGrid,
        "saveShape" : saveShape,
        "clearCanvas" : clearCanvas,
        "drawShape" : drawShape
    };
});