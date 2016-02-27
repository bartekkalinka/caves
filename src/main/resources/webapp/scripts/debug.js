"use strict";

define( ['scripts/globals'], function (glob) {
    function handleMessage(message) {
        if(glob.debug.pause) return;
        if(glob.debug.concat) {
          glob.debug.info = glob.debug.info + message;
        }
        else {
          glob.debug.info = "";
        }
        $("#shortDebug").html(message);
        $("#longDebug").html(glob.debug.info);
    }

    function toggleConcatFlag() {
        glob.debug.concat = !glob.debug.concat;
    }

    function togglePause() {
        glob.debug.pause = !glob.debug.pause;
    }

    return {
        "handleMessage" : handleMessage,
        "toggleConcatFlag" : toggleConcatFlag,
        "togglePause" : togglePause
    };
});
