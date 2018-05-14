"use strict";

(function() {
    var alphaThreshold = 0;
    var mooreNeighborhood = [
        { x : -1, y : -1 },
        { x : 0, y : -1 },
        { x : 1, y : -1 },
        { x : 1, y : 0 },
        { x : 1, y : 1 },
        { x : 0, y : 1 },
        { x : -1, y : 1 },
        { x : -1, y : 0 }
    ];

    var extractImageData = function(bitmap, width, height) {
        var canvas = document.createElement("canvas");
        canvas.width = width;
        canvas.height = height;

        var context = canvas.getContext("2d");
        context.drawImage(bitmap, 0, 0, width, height);

        return context.getImageData(0, 0, width, height).data;
    }

    var convertToRgba = function(imageData, width, height) {
        var rgbaRows = [];
        for (var row = 0; row < height; row++) {
            var rgbaCells = [];
            for (var cell = 0; cell < width; cell++) {
                var index = ((width * row) + cell) * 4;
                var rgba = { red : imageData[index], green : imageData[index + 1], blue : imageData[index + 2], alpha : imageData[index + 3] };
                rgbaCells.push(rgba);
            }

            rgbaRows.push(rgbaCells);
        }

        return rgbaRows;
    }

    var findStartCoordinates = function(rgbas, width, height) {
        for (var y = 0; y < height; y++) {
            for (var x = 0; x < width; x++) {
                var rgba = rgbas[y][x];
                if (rgba.alpha > alphaThreshold) {
                    return { x : x, y : y, mooreNeighborIndex : 4 };
                }
            }
        }

        return null;
    }

    var findNextCoordinates = function(rgbas, width, height, previousCoordinates) {
        var oppositeToPreviousIndex = (previousCoordinates.mooreNeighborIndex + (mooreNeighborhood.length / 2)) % mooreNeighborhood.length;
        var mooreNeighborIndex = (oppositeToPreviousIndex + 1) % mooreNeighborhood.length;
        while (mooreNeighborIndex !== oppositeToPreviousIndex) {
            var mooreNeighbor = mooreNeighborhood[mooreNeighborIndex];
            var neighborX = previousCoordinates.x + mooreNeighbor.x;
            var neighborY = previousCoordinates.y + mooreNeighbor.y;
            if (neighborX >= 0 && neighborX < width && neighborY >= 0 && neighborY < height) {
                var neighborAlpha = rgbas[neighborY][neighborX].alpha;
                if (neighborAlpha > alphaThreshold) {
                    return { x : neighborX, y : neighborY, mooreNeighborIndex : mooreNeighborIndex }
                }
            }

            mooreNeighborIndex++;
            if (mooreNeighborIndex >= mooreNeighborhood.length) {
                mooreNeighborIndex = 0;
            }
        } 

        return null;
    }

    var isContourFinished = function(contour) {
        var firstCoordinates = contour[0];
        var secondCoordinates = contour[1];
        var panultimateCoordinates = contour[contour.length - 2];
        var ultimateCoordinates = contour[contour.length - 1];
        return firstCoordinates.x === panultimateCoordinates.x && firstCoordinates.y === panultimateCoordinates.y &&
            secondCoordinates.x === ultimateCoordinates.x && secondCoordinates.y === ultimateCoordinates.y;
    }

    var getContour = function(rgbas, width, height) {
        var startCoordinates = findStartCoordinates(rgbas, width, height);
        if (!startCoordinates) {
            return [];
        }
        else {
            var contour = [startCoordinates];
            var nextCoordinates = findNextCoordinates(rgbas, width, height, startCoordinates);
            if (nextCoordinates) {
                contour.push(nextCoordinates);
                do {
                    nextCoordinates = findNextCoordinates(rgbas, width, height, nextCoordinates);
                    contour.push(nextCoordinates);
                } while (!isContourFinished(contour))
            }

            return contour;
        }
    }
    
    var build = function(bitmap) {
        var width = bitmap.width;
        var height = bitmap.height;

        var imageData = extractImageData(bitmap, width, height);
        var rgbas = convertToRgba(imageData, width, height);
        var contour = getContour(rgbas, width, height);
        console.log(width, height, rgbas, contour);

        return [0, 1, 2, 3];
    };

    window["COLLISION_POLYGON"] = {
        "build" : build
    };
})();