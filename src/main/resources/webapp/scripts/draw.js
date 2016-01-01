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
      return glob.shapeTiles * glob.tilePixels
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
    function clearShapeSquare() {
      ctx.fillStyle = "rgb(0,0,0)";
      ctx.fillRect(0, 0, shapePixels(), shapePixels());
    }
    function isShapeTileSet(shape, j, i) {
      return shape[j][i] === "1";
    }
    function drawShape(shape) {
      clearShapeSquare();
      ctx.fillStyle = "rgb(255,0,0)";
      var shapeTilesY = shape.length;
      var shapeTilesX = shape[0].length;
      var tilePixels = glob.tilePixels;
      var i, j
      for(i=0; i<shapeTilesX; i+=1) {
        for(j=0; j<shapeTilesY; j+=1) {
          if(isShapeTileSet(shape, j, i)) {
            ctx.fillRect(
              tilePixels * j,
              tilePixels * i,
              tilePixels + 1,
              tilePixels + 1
            );
          }
        }
      }
    }

    var playerImage = new Image();

    function loadImage() {
      playerImage.src = "gfx/player_face.PNG";
    }

    loadImage();

    function drawPlayer() {
      ctx.drawImage(playerImage, (canvas.width / 2) - ((canvas.width / 2) % glob.tilePixels), (canvas.height / 2) - ((canvas.height / 2) % glob.tilePixels), glob.tilePixels, glob.tilePixels * 2);
    }

    return {
        "initGrid" : initGrid,
        "clearCanvas" : clearCanvas,
        "drawShape" : drawShape,
        "drawPlayer" : drawPlayer
    };
});