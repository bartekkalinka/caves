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
    function getGridPixelsOffset(dx, dy) {
        return [dx * shapePixels(), dy * shapePixels()];
    }
    function clearShapeSquare(x, y) {
      ctx.fillStyle = "rgb(0,0,0)";
      ctx.fillRect(x, y, shapePixels(), shapePixels());
    }
    function isShapeTileSet(shape, j, i) {
      return shape[j][i] === "1";
    }
    function drawShape(shape, x, y) {
      clearShapeSquare(x, y);
      ctx.fillStyle = "rgb(255,0,0)";
      var detailScale = Math.pow(2, glob.targetDetail);
      var shapeTiles = glob.initShapeTiles * detailScale;
      var tilePixels = glob.baseTilePixels / detailScale;
      var i, j
      for(i=0; i<shapeTiles; i+=1) {
        for(j=0; j<shapeTiles; j+=1) {
          if(isShapeTileSet(shape, j, i)) {
            ctx.fillRect(
              x + tilePixels * j,
              y + tilePixels * i,
              tilePixels + 1,
              tilePixels + 1
            );
          }
        }
      }
    }

    return {
        "initGrid" : initGrid,
        "clearCanvas" : clearCanvas,
        "drawShape" : drawShape
    };
});