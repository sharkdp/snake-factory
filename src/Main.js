// module Main

exports.clickHandler = function(f) {
    return function() {
        document.getElementById("run").addEventListener("click", function () {
            var code = document.getElementById("code").value;
            f(code)();
        });
        return {};
    };
};
