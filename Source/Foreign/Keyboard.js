(function(){
    var keysDown = {};

    document.addEventListener("keydown", function(event) { keysDown[event.code] = true });
    document.addEventListener("keyup", function(event) { keysDown[event.code] = false });

    window["KEYBOARD"] = {
        "isKeyDown" : function(key) {
            return key in keysDown && keysDown[key] === true
        }
    };
})();