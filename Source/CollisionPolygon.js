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

    var distanceThreshold = 5;

    var extractImageData = function(bitmap, width, height) {
        var canvas = document.createElement("canvas");
        canvas.width = width;
        canvas.height = height;

        var context = canvas.getContext("2d");
        context.drawImage(bitmap, 0, 0, width, height);

        return context.getImageData(0, 0, width, height).data;
    };

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
    };

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
    };

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
    };

    var isContourFinished = function(contour) {
        var firstCoordinates = contour[0];
        var secondCoordinates = contour[1];
        var panultimateCoordinates = contour[contour.length - 2];
        var ultimateCoordinates = contour[contour.length - 1];
        return firstCoordinates.x === panultimateCoordinates.x && firstCoordinates.y === panultimateCoordinates.y &&
            secondCoordinates.x === ultimateCoordinates.x && secondCoordinates.y === ultimateCoordinates.y;
    };

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

                contour.pop();
            }

            return contour;
        }
    };

    var distance = function(pointA, pointB) {
        return Math.sqrt(Math.pow(pointB.x - pointA.x, 2) + Math.pow(pointB.y - pointA.y, 2));
    };

    var perpendicularDistance = function(point, lineFrom, lineTo) {
        if (lineFrom.x === lineTo.x && lineFrom.y === lineTo.y) {
            return distance(point, lineFrom);
        }
        else {
            var nominator = Math.abs((lineTo.x - lineFrom.x) * (lineFrom.y - point.y) - (lineFrom.x - point.x) * (lineTo.y - lineFrom.y));
            var denominator = Math.sqrt(Math.pow(lineTo.x - lineFrom.x, 2) + Math.pow(lineTo.y - lineFrom.y, 2));
            return nominator / denominator;
        }
    };

    var reducePolygon = function reducePolygon(polygon) {
        var maxDistance = 0;
        var maxDistanceIndex = 0;

        var firstPoint = polygon[0];
        var lastPoint = polygon[polygon.length - 1];

        for (var i = 0; i < polygon.length; i++) {
            var point = polygon[i];
            var distance = perpendicularDistance(point, firstPoint, lastPoint);
            if (distance > maxDistance) {
                maxDistance = distance;
                maxDistanceIndex = i;
            }
        }

        if (maxDistance > distanceThreshold) {
            var lowPolygon = polygon.slice(0, maxDistanceIndex + 1);
            var highPolygon = polygon.slice(maxDistanceIndex, polygon.length);

            var lowReducedPolygon = reducePolygon(lowPolygon);
            var highReducedPolygon = reducePolygon(highPolygon);

            return lowReducedPolygon.slice(0, lowReducedPolygon.length - 1).concat(highReducedPolygon);
        }
        else {
            return [firstPoint, lastPoint];
        }
    };
    
    var build = function(bitmap) {
        var width = bitmap.width;
        var height = bitmap.height;

        var imageData = extractImageData(bitmap, width, height);
        var rgbas = convertToRgba(imageData, width, height);
        var contour = getContour(rgbas, width, height);
        var polygon = reducePolygon(contour);

        return polygon.map(function(point) { return [point.x, point.y] }).reduce(function(a, b) { return a.concat(b) });
    };

    window["COLLISION_POLYGON"] = {
        "build" : build
    };
})();