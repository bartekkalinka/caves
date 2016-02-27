"use strict";

define( ['scripts/globals'], function (glob) {
    function message(debugMessage) {
        if(glob.debugConcat) {
          glob.debugInfo = glob.debugInfo + debugMessage;
        }
        else {
          glob.debugInfo = debugMessage;
        }
        $("#debug").html(glob.debugInfo);
    }

    function toggleConcatFlag() {
        glob.debugConcat = !glob.debugConcat;
    }

    return {
        "message" : message,
        "toggleConcatFlag" : toggleConcatFlag
    };
});
