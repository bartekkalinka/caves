"use strict";

define( ['scripts/globals'], function (glob) {
    function handleMessage(message) {
        if(glob.debug.pause) return;
        if(glob.debug.concat) {
          glob.debug.messages.push(message);
        }
        else {
          glob.debug.messages = [];
        }
        $("#shortDebug").html(message);
        renderMessages();
    }

    function renderMessages() {
        var i, rendered = "";
        for(i=0; i<glob.debug.messages.length; i++) {
            rendered += (glob.debug.messages[i] + "<br>");
        }
        $("#longDebug").html(rendered);
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
