(function() {
    var isDebugEnabled = false;

    window["DEBUG"] = {
        "isDebugEnabled" : function() { return isDebugEnabled; },
        "setDebugEnabled" : function(isEnabled) { isDebugEnabled = isEnabled }
    };
})();