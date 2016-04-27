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
    function drawTunnelTile(j, i, tilePixels, offset, ctx) {
       ctx.strokeStyle = "rgb(128,0,0)";
       ctx.lineWidth = 2;
       ctx.beginPath();
       ctx.moveTo(
         tilePixels * j - offset[0],
         tilePixels * i - offset[1]
       );
       ctx.lineTo(
         tilePixels * (j + 1) - offset[0],
         tilePixels * (i + 1) - offset[1]
       );
       ctx.stroke();
     }
    function drawStandardTile(j, i, tilePixels, offset, ctx) {
      ctx.fillStyle = "rgb(255,0,0)";
      ctx.fillRect(
        tilePixels * j - offset[0],
        tilePixels * i - offset[1],
        tilePixels + 1,
        tilePixels + 1
      );
    }
    function drawTile(shape, j, i, tilePixels, offset, ctx) {
      if(shape[j][i] === "1")
        drawStandardTile(j, i, tilePixels, offset, ctx);
      else if(shape[j][i] === "2")
        drawTunnelTile(j, i, tilePixels, offset, ctx);
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
            drawTile(shape, j, i, tilePixels, offset, ctx);
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
      if(faceDirection == "game.singleplayer.FaceDirection.Straight")
        playerImage = playerFrontImage;
      else if(faceDirection == "game.singleplayer.FaceDirection.Left")
        playerImage = playerLeftImage;
      else if(faceDirection == "game.singleplayer.FaceDirection.Right")
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