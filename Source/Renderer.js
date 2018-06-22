"use strict";

(function() {
	var measureTextWidth = function(context, text, font) {
		var previousFont = context.font;

		context.font = font;
		var width = context.measureText(text).width;
		context.font = previousFont;

		return width;
	};

	window["RENDERER"] = {
        "measureTextWidth" : measureTextWidth
    };
})();