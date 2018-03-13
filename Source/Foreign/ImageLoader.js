(function() {
    var loadedImages = {};
    var hadErrorOccured = false;

    window["IMAGE_LOADER"] = {
        "loadImage" : function (path) {
            if (!(path in loadedImages)) {
                loadedImages[path] = null;

                var imageElement = new Image();

                imageElement.onload = function() {
                    loadedImages[path] = imageElement;
                };

                imageElement.onerror = function(error) {
                    console.error(error);
                    hadErrorOccured = true;
                };

                imageElement.src = path;
            }
        },

        "isImageLoaded" : function(path) {
            return path in loadedImages && loadedImages[path] !== null;
        },

        "getLoadedImage" : function(path) {
            return loadedImages[path];
        },

        "hadErrorOccured" : function() {
            return hadErrorOccured;
        }
    };
})();