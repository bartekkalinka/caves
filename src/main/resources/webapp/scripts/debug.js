"use strict";

define( ['scripts/globals'], function (glob) {
    function handleMessage(message) {
        if(glob.debug.concat) {
          glob.debug.info = glob.debug.info + message;
        }
        else {
          glob.debug.info = message;
        }
        $("#debug").html(glob.debug.info);
    }

    function toggleConcatFlag() {
        glob.debug.concat = !glob.debug.concat;
    }

    return {
        "handleMessage" : handleMessage,
        "toggleConcatFlag" : toggleConcatFlag
    };
});
