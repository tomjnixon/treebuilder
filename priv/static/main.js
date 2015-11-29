
function compile_cpp(cpp_code, callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('POST', '/js_compile');
    xhr.setRequestHeader('Content-Type', 'application/json');
    xhr.onload = function() {
        if (xhr.status === 200) {
            var json = JSON.parse(xhr.responseText);
            callback(json.js_code, json.errors);
        }
    };
    xhr.send(JSON.stringify({
        cpp_code: cpp_code
    }));
}

function get_test_mod(callback) {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', '/assets/test.cpp');
    xhr.onload = function() {
        if (xhr.status === 200) {
            callback(xhr.responseText);
        }
    };
    xhr.send();
}

function test() {
    get_test_mod(function (cpp_code) {
        console.log("compiling: " + cpp_code);
        compile_cpp(cpp_code, function(js_code, errors) {
            console.log("js: " + js_code);
            console.log("errors: " + errors);
        });
    });
}

test();
