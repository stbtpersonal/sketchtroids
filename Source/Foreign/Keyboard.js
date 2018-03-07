(function(){
	var keysDown = { "a" : true, "b" : false };

	window["KEYBOARD"] = {
		"isKeyDown" : function(key) {
			return (key in keysDown) && keysDown[key] === true
		}
	}
})();