"use strict";

define( ['scripts/globals'], function (glob) {
    function handleMessage(message) {
        var shortMessage;
        if(glob.debug.pause) return;
        if(glob.debug.show) {
          glob.debug.messages.push(message);
          if(glob.debug.messages.length > glob.debug.messagesMaxLength) glob.debug.messages.shift();
          shortMessage = message;
        }
        else {
          glob.debug.messages = [];
          shortMessage = "";
        }
        $("#shortDebug").html(shortMessage);
        renderMessages();
    }

    function renderMessages() {
        var i, rendered = "";
        for(i=0; i<glob.debug.messages.length; i++) {
            rendered += (glob.debug.messages[i] + "<br>");
        }
        $("#longDebug").html(rendered);
    }

    function toggleShowFlag() {
        glob.debug.show = !glob.debug.show;
    }

    function togglePause() {
        glob.debug.pause = !glob.debug.pause;
    }

    return {
        "handleMessage" : handleMessage,
        "toggleShowFlag" : toggleShowFlag,
        "togglePause" : togglePause
    };
});
