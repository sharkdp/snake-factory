// module Main

exports.onRun = function(f) {
    return function() {
        var handler = function() {
            var code = document.getElementById("code").value;
            f(code)();
        };

        document.getElementById("run").addEventListener("click", handler);
        document.addEventListener("keydown", function(e) {
            if (e.keyCode == 32) {
                e.preventDefault();
                handler();
            }
        });

        return {};
    };
};
