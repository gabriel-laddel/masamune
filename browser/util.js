function helpUrlFor(thing) {
    var mdcXpcomClassUrl = function (classid) {
        return 'https://developer.mozilla.org/en-US/search?q=' + escape('"' + classid + '"');
    };
    var mdcXulElementUrl = function (element) {
        return 'http://developer.mozilla.org/en/XUL/' + element.nodeName;
    };
    if (typeof thing == 'string') {
        if(thing.match(/^@mozilla.org\//)) return mdcXpcomClassUrl(thing);
    };
    (thing.QueryInterface && (function () {
        var NS_NOINTERFACE = 0x80004002;
        try {
            thing.QueryInterface(Components.interfaces.nsIDOMXULElement);
            return true;
        } catch (e) {
            return null;
        };
    })())();
    return mdcXulElementUrl(thing);
};
function docFor(thing) {
    var printout = 'TYPE: ' + typeof thing + '\n';
    if (thing.name) {
        printout = 'NAME: ' + thing.name + '\n';
    } else {
        if (thing.nodeName) {
            printout = printout + 'NODENAME: ' + thing.nodeName + '\n';
        };
    };
    if (typeof thing == 'function') {
        var list = argList(thing);
        printout += 'ARGS: ' + (list.length == 0 ? '[none declared]' : list.join(', ')) + '\n';
    };
    if (thing.doc && typeof thing.doc == 'string') {
        printout = printout + '\n' + thing.doc + '\n';
    };
    return printout;
};
function argList(fn) {
    var rx = new RegExp('^function (\\w+)?\\(([^\\]*)?\\) {');
    var match3 = fn.toString().match(rx);
    return match3[2] ? match3[2].split(', ') : [];
};
