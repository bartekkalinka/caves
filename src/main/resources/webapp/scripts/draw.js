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
      return shape[j][i] !== "0";
    }
    function tileColor(shape, j, i) {
      if(shape[j][i] === "1")
        return "rgb(255,0,0)";
      else if(shape[j][i] === "2")
        return "rgb(128,0,0)"
    }

    function drawShape(shape, offset) {
      clearShapeSquare();
      var shapeTilesY = shape.length;
      var shapeTilesX = shape[0].length;
      var tilePixels = glob.tilePixels;
      var i, j
      for(i=0; i<shapeTilesX; i+=1) {
        for(j=0; j<shapeTilesY; j+=1) {
          if(isShapeTileSet(shape, j, i)) {
            ctx.fillStyle = tileColor(shape, j, i);
            ctx.fillRect(
              tilePixels * j - offset[0],
              tilePixels * i - offset[1],
              tilePixels + 1,
              tilePixels + 1
            );
          }
        }
      }
    }

    var playerFrontImage = new Image();
    var playerLeftImage = new Image();
    var playerRightImage = new Image();

    function loadImages() {
      playerFrontImage.src = "gfx/player_face.PNG";
      playerLeftImage.src = "gfx/player_left_1.PNG";
      playerRightImage.src = "gfx/player_right_1.PNG";
    }

    loadImages();

    function drawPlayer(x, y, faceDirection) {
      var playerImage;
      if(faceDirection == "game.state.FaceDirection.Straight")
        playerImage = playerFrontImage;
      else if(faceDirection == "game.state.FaceDirection.Left")
        playerImage = playerLeftImage;
      else if(faceDirection == "game.state.FaceDirection.Right")
        playerImage = playerRightImage;
      ctx.drawImage(playerImage, x, y, glob.tilePixels, glob.tilePixels * 2);
    }

    return {
        "initGrid" : initGrid,
        "clearCanvas" : clearCanvas,
        "drawShape" : drawShape,
        "drawPlayer" : drawPlayer
    };
});