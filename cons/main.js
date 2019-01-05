import '../static/snabbdom.js';
import '../static/snabbdom-attributes.js';
import '../static/snabbdom-eventlisteners.js';


let container = document.getElementById('container');

let patch = snabbdom.init([
    snabbdom_attributes.default,
    snabbdom_eventlisteners.default,
]);

let h = snabbdom.h;

document.recv = function() {
    let json = document.javascript;
    container = patch(container, translate(json))
}

let send = function(event, identifier) {
    var msg = {
        "identifier": identifier,
        "event": {'target.value': event.target.value},
    };
    document.scheme = JSON.stringify(msg);
    document.resume();
}

let makeCallback = function(identifier) {
    return function(event) {
        return send(event, identifier);
    };
}

/* Translate json to `vnode` using `h` snabbdom helper */
let translate = function(json) {
    let on = json.options.on;
    for (let key in on) {
        on[key] = makeCallback(on[key]);
    }

    // recurse to translate children
    let children = [];
    if (json.children !== undefined) {
        children = json.children.map(function(child) {  // TODO: optimize with a for-loop
            if (child instanceof Object) {
                return translate(child);
            } else { // it's a string or a number
                return child;
            }
        });
    }

    return h(json.tag, json.options, children);
}


// application

document.inbox = 0;


let chibi;

function start(program, args) {
    return Chibi({
        print: console.log,
        printErr: console.error,
        program: program,
        arguments: args
    });
}


fetch("main.scm").then(function(response) {
    response.text().then(function(main) {
        console.log("starting chibi scheme...");
        chibi = start(main, []);
    });
})
