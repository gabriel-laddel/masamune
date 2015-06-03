require('new-tabs.js');
require('clicks-in-new-buffer.js');
view_source_use_external_editor = true;
editor_shell_command = 'emacsclient';
homepage = 'file:///root/algol/conkeror/help/tutorial.html';
minibuffer_auto_complete_default = true;
url_completion_use_history = true;
download_buffer_automatic_open_target = OPEN_NEW_BUFFER_BACKGROUND;
url_completion_use_bookmarks = true;
hints_display_url_panel = true;
clicks_in_new_buffer_target = OPEN_NEW_BUFFER_BACKGROUND;
add_hook('mode_line_hook', mode_line_adder(loading_count_widget), true);
remove_hook('mode_line_hook', mode_line_adder(clock_widget));
interactive('copy-url', 'Copy the current buffer\'s URL to the clipboard', function (I) {
    text = I.window.buffers.current.document.location.href;
    writeToClipboard(text);
    return I.window.minibuffer.message('copied: ' + text);
});
interactive('reload-config', 'reload conkerorrc', function (I) {
    load_rc();
    return I.window.minibuffer.message('config reloaded');
});
interactive('view-current-buffer-archive', 'Visit this url\'s Waybackmachine archive in the current buffer', function (I) {
    return load_url_in_current_buffer(I.window.buffers.current.document.location.href, new interactive_context());
});
interactive('extensions', 'Open the extensions manager in a new tab.', function () {
    return load_url_in_new_buffer('chrome://mozapps/content/extensions/extensions.xul?type=extensions', new interactive_context());
});
interactive('console', 'Opens the XUL console in a new tab.', function () {
    return load_url_in_new_buffer('chrome://global/content/console.xul', new interactive_context());
});
define_key(default_global_keymap, 'C-c u', 'copy-url');
define_key(default_global_keymap, 'C-c r', 'reload-config');
define_key(default_global_keymap, 'C-x f', 'follow-new-buffer-background');
define_key(content_buffer_normal_keymap, 'C-f', 'forward');
define_key(content_buffer_normal_keymap, 'C-b', 'back');
define_key(content_buffer_normal_keymap, 'M-y', 'search-clipboard-contents');
define_key(content_buffer_normal_keymap, 'M-Y', 'search-clipboard-contents-doublequoted');
var port = 4258;
var serv;
var contextWindowType;
var Cc = Components.classes;
var Ci = Components.interfaces;
var conkeror = Cc['@conkeror.mozdev.org/application;1'].getService().wrappedJSObject;
var loader = Cc['@mozilla.org/moz/jssubscript-loader;1'].getService(Ci.mozIJSSubScriptLoader);
var srvPref = Cc['@mozilla.org/preferences-service;1'];
var srvObserver = Cc['@mozilla.org/observer-service;1'];
function lg(msg) {
    return dump(msg + '\n');
}
lg.doc = "Log is a reserved symbol - and translates to Math.log";
function REPL() {
    this.__exposedProps__ = this.__exposedProps__ || _generateExposedProps(this.__proto__);
    return this;
};
loader.loadSubScript('file:///root/quicklisp/local-projects/masamune/browser/repl.js', REPL.prototype);
function _generateExposedProps(object) {
    var props = {  };
    Object.keys(object).filter(function (k) {
        return k[0] !== '_';
    }).forEach(function (k) {
        return props[k] = 'r';
    });
    return props;
};
var sessions = { _list : [],
                 add : function (session) {
    return this._list.push(session);
},
                 remove : function (session) {
    var index = this._list.indexOf(session);
    return index != -1 ? this._list.splice(index, 1) : null;
},
                 get : function (index) {
    return this._list[index];
},
                 quit : function () {
    this._list.forEach(function (session) {
        return session.quit;
    });
    return this._list.splice(0, this._list.length);
}
               };
function start(port) {
    try {
        serv = Cc['@mozilla.org/network/server-socket;1'].createInstance(Ci.nsIServerSocket);
        serv.init(port, true, -1);
        serv.asyncListen(this);
        return lg('REPL Listening at: 127.0.0.1: ' + port);
    } catch (e) {
        return lg('REPL: error ' + e);
    };
};

function onSocketAccepted(serv, transport) {
    try {
        var outstream = transport.openOutputStream(Ci.nsITransport.OPEN_BLOCKING , 0, 0);
        var outstreamutf8 = Cc['@mozilla.org/intl/converter-output-stream;1']
            .createInstance(Ci.nsIConverterOutputStream);
        outstreamutf8.init(outstream, 'UTF-8', 0, 0);

        var instream = transport.openInputStream(0, 0, 0);
        var instreamutf8 = Cc['@mozilla.org/intl/converter-input-stream;1']
            .createInstance(Ci.nsIConverterInputStream);
        instreamutf8.init(instream, 'UTF-8', 1024, 0);
    } catch(e) {
        lg('E, MOZREPL : Error : ' + e);
    }

    var context = Cc['@mozilla.org/appshell/window-mediator;1']
        .getService(Ci.nsIWindowMediator)
        .getMostRecentWindow('');

    if(context === null) {
        context = Cc['@mozilla.org/appshell/appShellService;1']
            .getService(Ci.nsIAppShellService)
            .hiddenDOMWindow.wrappedJSObject;
    }

    var session = new REPL();
    session.onOutput = function(string) {
        outstreamutf8.writeString(string);
    };
    session.onQuit = function() {
        lg('I, MOZREPL : Client closed connection : ' + transport.host + ':' + transport.port);        
        instream.close();
        outstream.close();
        sessions.remove(session);
    };
    session.init(context);

    lg('I, MOZREPL : Client connected : ' + transport.host + ':' + transport.port +
        ' : ' + (context instanceof Ci.nsIDOMWindow ?
                 context.document.location.href : context));

    var pump = Cc['@mozilla.org/network/input-stream-pump;1']
        .createInstance(Ci.nsIInputStreamPump);
    pump.init(instream, -1, -1, 0, 0, false);
    pump.asyncRead({
        onStartRequest: function(request, context) {},
        onStopRequest: function(request, context, status) {
                session.quit();
            },
        onDataAvailable: function(request, context, inputStream, offset, count) {
            var str = {}
            instreamutf8.readString(count, str)
            session.receive(str.value);
            }
        }, null);

    sessions.add(session);
};
function sendBuffers() {
    var windowEnum = Cc['@mozilla.org/appshell/window-mediator;1'].getService(Ci.nsIWindowMediator).getEnumerator('');
    var singleXWindow = windowEnum.getNext();
    var buffers3 = singleXWindow.buffers.buffer_history;
    var out = '(:buffers (';
    var _js4 = buffers3.length;
    for (var i = 0; i < _js4; i += 1) {
        var buffer = buffers3[i];
        out += '(:uri "' + buffer.document.location.href + '" :scroll-y ' + buffer.scrollY + ')';
    };
    return dump('\n' + out + '))' + '\n');
};
function stop() {
    lg('REPL: closing');
    serv.close();
    sessions.quit();
    return serv = null;
};
function isActive() {
    return serv ? true : null;
};
function onStopListening(serv, status) {
    return null;
};
function observe(subject, topic, data) {
    if (topic === 'profile-after-change') {
        servObserver.addObserver(this, 'network:offline-status-changed', false);
        return srvPref.getBranch('network.').getBoolPref('online') ? this(start) : null;
    } else if (topic === 'network:offline-status-changed') {
        if (data === 'online') {
            return this.start(port);
        } else if (data === 'offline') {
            return isActive() ? this.stop() : null;
        };
    } else if (topic === 'quit-application-granted') {
        return this.stop();
    };
};
function setContextWindowType(windowType) {
    return contextWindowType = windowType;
};
start(port);
add_hook('buffer_loaded_hook', sendBuffers, null, true);
add_hook('buffer_scroll_hook', sendBuffers, null, true);
add_hook('kill_buffer_hook', sendBuffers, null, true);
add_hook('select_buffer_hook', sendBuffers, null, true);
function getWindows() {
    var windowEnum = Cc['@mozilla.org/appshell/window-mediator;1'].getService(Ci.nsIWindowMediator).getEnumerator('');
    var windows = [];
    while (windowEnum.hasMoreElements()) {
        windows.push(windowEnum.getNext());
    };
    return windows;
};
function currentBufferHref() {
    var windowEnum = Cc['@mozilla.org/appshell/window-mediator;1'].getService(Ci.nsIWindowMediator).getEnumerator('');
    var singleXWindow = windowEnum.getNext();
    var buffer = singleXWindow.buffers.buffer_history[0];
    return buffer.document.location.href;
};
function maybeRemoveWikiPageExtras() {
    if (currentBufferHref().match('wiki')) {
        getWindows()[0].buffers.buffer_history[0].document.getElementById('content').style.marginLeft = 0;
        getWindows()[0].buffers.buffer_history[0].document.getElementById('mw-panel').parentNode.removeChild(getWindows()[0].buffers.buffer_history[0].document.getElementById('mw-panel'));
        getWindows()[0].buffers.buffer_history[0].document.getElementById('mw-page-base').parentNode.removeChild(getWindows()[0].buffers.buffer_history[0].document.getElementById('mw-page-base'));
        return getWindows()[0].buffers.buffer_history[0].document.getElementById('mw-head').parentNode.removeChild(getWindows()[0].buffers.buffer_history[0].document.getElementById('mw-head'));
    };
};
add_hook('buffer_dom_content_loaded_hook', maybeRemoveWikiPageExtras, null, true);