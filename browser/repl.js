var Cc = Components.classes;
var Ci = Components.interfaces;
var loader = Cc['@mozilla.org/moz/jssubscript-loader;1'].getService(Ci.mozIJSSubScriptLoader);
var util = {  };
loader.loadSubScript('file:///root/quicklisp/local-projects/masamune/browser/util.js', util);
function onOutput() {
    return dump('onOutput callback must be assigned.');
};
function onQuit() {
    throw new Error('onQuit callback must be assigned.');
};
function init(context) {
    var _this = this;
    this._name = chooseName('repl', context);
    this._creationContext = context;
    this._hostContext = context;
    this._workContext = context;
    this._creationContext[this._name] = this;
    this._contextHistory = [];
    this._inputBuffer = '';
    this._emergencyExit = function (event) {
        _this.print('Host context unloading! Going back to creation context.');
        return _this.home();
    };
    this.__defineGetter__('repl', function () {
        return this;
    });
    this._env = {  };
    this._savedEnv = {  };
    this.setenv('printPrompt', true);
    this.setenv('inputMode', 'syntax');
    this._interactorClasses = {  };
    this._interactorStack = [];
    this.defineInteractor('javascript', javascriptInteractor);
    var defaultInteractorClass = this._interactorClasses['javascript'];
    var defaultInteractor = new defaultInteractorClass(this);
    this._interactorStack = [defaultInteractor];
    return defaultInteractor.onStart(this);
};
function setenv(name, value) {
    this._env[name] = value;
    return value;
};
function getenv(name) {
    return this._env[name];
};
function pushenv() {
    var name = null;
    for (var i = 0, l = arguments.length; i < l; ++i) {
        name = arguments[i];
        this._savedEnv[name] = this._env[name];
    };
};
function popenv() {
    var name = name;
    var _js94 = arguments.length;
    for (var i = 0; i < _js94; i += 1) {
        var name = arguments[i];
        if (name in this._savedEnv) {
            this._env[name] = this._savedEnv[name];
            delete this._savedEnv[name];
        };
    };
};

function represent(thing) {
    var represent = arguments.callee;
    var s;
    switch(typeof(thing)) {
    case 'string':
        s = ' ' + thing + ' ';
        break;
    case 'number':
        s = thing;
        break;
    case 'object':
        var names = [];
        for(var name in thing)
            names.push(name);

        s = thing;
        if(names.length > 0) {
            s += ' - {';
            s += names.slice(0, 7).map(function(n) {
                var repr = n + ': ';
                try {
                    if(thing[n] === null)
                        repr += 'null';
                    else if(typeof(thing[n]) == 'object')
                        repr += '{...}';
                    else
                        repr += represent(thing[n]);
                } catch(e) {
                    repr += '[Exception!]'
                }
                return repr;
            }).join(', ');
            if(names.length > 7)
                s += ', ...'
            s += '}';
        }
        break;
    case 'function':
        s = 'function() {...}';
        break;
    default:
        s = thing;
    }
    return s;
}

;
function print(data, appendNewline) {
    return this.onOutput(data == undefined ? '\n' : data + (appendNewline == false ? '' : '\n'));
};
function load(url, arbitraryContext) {
    try {
        return loader.loadSubScript(url, arbitraryContext || this._workContext);
    } catch (nil) {
        return null;
    };
};
function enter(context, wrapped) {
    if (wrapped != true && context.wrappedJSObject != undefined) {
        context = context.wrappedJSObject;
    };
    this._contextHistory.push(this._workContext);
    if (isTopLevel(context)) {
        this._migrateTopLevel(context);
    };
    this._workContext = context;
    return this._workContext;
};
function back() {
    if (context = this._contextHistory.pop()) {
        if (isTopLevel(context)) {
            this._migrateTopLevel(context);
        };
        this._workContext = context;
        return this._workContext;
    };
}
back.doc = "Returns to the previous context.";
function home() {
    return this.enter(this._creationContext);
}
home.doc = "Returns to the context where the REPL was created.";
function quit() {
    this.currentInteractor().onStop && this.currentInteractor().onStop(this);
    delete this._hostContext[this._name];
    delete this._creationContext[this._name];
    return this.onQuit();
}
quit.doc = "Ends the session.";
function rename(name) {
    if (name in this._hostContext) {
        return print('sorry, name already exists in the context repl is hosted in.');
    } else if (name in this._creationContext) {
        return print('Sorry name already exists in the context was created');
    } else {
        delete this._creationContext[this._name];
        delete this._hostContext[this._name];
        this._name = name;
        this._creationContext[this._name] = this;
        return this._hostContext[this._name] = this;
    };
};
function inspect(object, maxDepth, name, currentDepth) {
    var crop = function (string, max) {
        var string95 = string.aref(match(/^(.+?)(\n|$)/m), 1);
        var max96 = max || 70;
        return string95.length > max96 - 3 ? '...' : string95;
    };
    if (name == undefined) {
        name = '<' + typeOf(object) + '>';
    } else if (maxDepth == undefined) {
        maxDepth = 0;
    } else if (currentDepth == undefined) {
        currentDepth = 0;
    } else if (maxDepth && currentDepth > maxDepth) {
        return null;
    };
    var i = 0;
    var _js98 = object.length;
    for (var _js97 = 0; _js97 < _js98; _js97 += 1) {
        var prop = object[_js97];
        if (instanceOf(object, Ci.nsIDOMWindow) && member(prop, ['java', 'sun', 'Packages'], 'test', 'stringequals')) {
            print(name + '.' + prop + '=[not inspecting, either java, sun or Packages]');
            continue;
        };
        try {
            ++i;
            if (object[prop] == null) {
                print(name + '.' + prop + '=null');
            } else if (typeof object[prop] == 'object') {
                if (object.length) {
                    print(name + '.' + prop + '=[probably array, length ' + object.length + ']');
                } else {
                    print(name + '.' + prop + '=[' + typeOf(object[prop]) + ']');
                };
                this.inspect(object[prop], maxDepth, name + '.' + prop + (currentDepth + 1));
            } else if ('function' == typeOf(object[prop])) {
                print(name + '.' + prop + '=[function]');
            } else if ('xml' == typeOf(object[prop])) {
                var s = object[prop]().toXMLString().replace(/>\n\s*/g, ' ');
                print(name + '.' + prop + '=' + (s.length > 100 ? s.slice(0, 97) + '...' : s));
            } else {
                print(name + '.' + prop + '=' + object[prop]);
                if (object[prop] && object[prop][doc] && typeof object[prop][doc] === 'string') {
                    print('   ' + crop(object[prop]));
                };
            };
        } catch (e) {
            print(name + '.' + prop + ' - exception while inspecting.');
        };
    };
    return !i ? print(name + '  is empty') : null;
};
function look() {
    return this.inspect(this._workContext, 0, 'this');
};
function highlight(context, time) {
    var context99 = context || this._workContext;
    var time100 = time || 1000;
    if (!context99.QueryInterface) {
        return null;
    };
    NS_NOINTERFACE = 0x80004002;;
    try {
        context99.QueryInterface(Ci.nsIDOMXULElement);
        var savedBorder = 'thick dotted red';
        return Cc['@mozilla.org/timer;1'].createInstance(Ci.nsITimer).initWithCallback({ 'notify' : function () {
            return context99.style.border = savedBorder;
        } }, time100, Ci.nsITimer.TYPE_ONE_SHOT);
    } catch (e) {
        return lg('hit catch block of the highlight function');
    };
};
function whereAmI() {
    var context = this._workContext;
    var desc = '';
    desc += context;
    if (context.document && context.document.title) {
        desc += ' - Document title: "' + context.document.title + '"';
    };
    if (context.nodeName) {
        desc += ' - ' + context.nodeName;
    };
    return print(desc);
};
function search(criteria, context) {
    var context = context || this._workContext;
    var matcher;
    if (typeof criteria == 'function') {
        matcher = criteria;
    } else {
        matcher = typeof criteria.test == 'function' ? function (name) {
            return criteria.test(name);
        } : function (name) {
            return name == criteria;
        };
    };
    for (var name in context) {
        if (matcher(name)) {
            print(name);
        };
    };
};
function doc(thing) {
    print(util.docFor(thing));
    url = util.helpUrlFor(thing);
    if (url) {
        print('Online help found, displaying...');
        return Cc['@mozilla.org/embedcomp/window-watcher;1'].getService(Ci.nsIWindowWatcher).openWindow(null, url, 'help', 'width=640,height=600,scrollbars=yes,menubars=no,' + 'toolbar=no,location=no,status=no,resizable=yes', null);
    };
};
function reloadChrome() {
    try {
        return Cc['@mozilla.org/chrome/chrome-registry;1'].getService(Ci.nsIXULChromeRegistry).reloadChrome();
    } catch (nil) {
        return null;
    };
};
function defineInteractor(name, proto) {
    this._interactorClasses[name] = function () {
        return null;
    };
    return this._interactorClasses[name].prototype = proto;
};
function currentInteractor() {
    return this._interactorStack[this._interactorStack.length - 1];
};
function popInteractor() {
    if (this._interactorStack.length == 1) {
        throw new Error('Cannot leave last interactor.');
    };
    this.currentInteractor().onStop && this.currentInteractor().onStop(this);
    this._interactorStack.pop();
    return this.currentInteractor().onResume && this.currentInteractor().onResume(this);
};
function pushInteractor(interactorName) {
    var interactorClass = this._interactorClasses[interactorName];
    if (typeof interactorClass == 'undefined') {
        throw new Error(('Interactor <' + interactorName) + '> not defined.');
    } else {
        this.currentInteractor().onSuspend && this.currentInteractor().onSuspend(this);
        newInteractor = new interactorClass(this);
        this._interactorStack.push(newInteractor);
        return newInteractor.onStart(this);
    };
};
pushInteractor.__defineGetter__('doc', function () {
    intNames = [];
    for (var intName in this._interactorClasses) {
        intNames.push(intName);
    };
    return ('Sets the current interactor. (Currently defined: "' + intNames.join('", "')) + '")';
});
var javascriptInteractor = {
    onStart: function(repl) {

        Cc['@mozilla.org/observer-service;1']
            .getService(Ci.nsIObserverService)
            .notifyObservers(null, 'startupcache-invalidate', null);

        this._inputBuffer = '';

        if(true) {
            repl.print('');
            repl.print('Welcome to Repl.');
            repl.print('');
            repl.print(' - If you get stuck at the ...> prompt, enter a semicolon (;) at the beginning of the line to force evaluation.');
            repl.print(' - If you get errors after every character you type, see http://github.com/bard/mozrepl/wikis/troubleshooting (short version: stop using Microsoft telnet, use netcat or putty instead)');
            repl.print('');
            repl.print('Current working context: ' + (repl._workContext instanceof Ci.nsIDOMWindow ?
                                                      repl._workContext.document.location.href :
                                                      repl._workContext));
            repl.print('Current input mode: ' + repl._env['inputMode']);

            repl.print('');
        }

        if(repl._name != 'repl') {
            repl.print('Hmmm, seems like other repls are running in repl context.');
            repl.print('To avoid conflicts, yours will be named ' + repl._name + '.');
        }

        repl._prompt();
    },

    onStop: function(repl) {},

    onSuspend: function(repl) {},

    onResume: function(repl) {},

    getPrompt: function(repl) {
        return repl._name + '> ';
    },

    handleInput: function(repl, input) {

        if(input.match(/^\s*$/) && this._inputBuffer.match(/^\s*$/)) {
            repl._prompt();
            return;
        }

        const inputSeparators = {
            line:      /\n/m,
            multiline: /\n--end-remote-input\n/m,
        };

        function handleError(e) {
            var realException = (e instanceof LoadedScriptError ? e.cause : e);

            repl.print('!!! ' + realException + '\n');
            if(realException) {
                repl.print('Details:')
                repl.print();
                for(var name in realException) {
                    var content = String(realException[name]);
                    if(content.indexOf('\n') != -1)
                        content = '\n' + content.replace(/^(?!$)/gm, '    ');
                    else
                        content = ' ' + content;

                    repl.print('  ' + name + ':' + content.replace(/\s*\n$/m, ''));
                }
                repl.print();
            }

            repl._prompt();
        }

        switch(repl.getenv('inputMode')) {
        case 'line':
        case 'multiline':
            this._inputBuffer += input;
            var [chunk, rest] = scan(this._inputBuffer, inputSeparators[repl.getenv('inputMode')]);
            while(chunk) {
                try {
                    var result = repl.evaluate(chunk);
                    if(this != undefined)
                        repl.print(repl.represent(result));
                    repl._prompt();
                } catch(e) {
                    handleError(e);
                }

                [chunk, rest] = scan(rest, inputSeparators[repl.getenv('inputMode')]);
            }
            this._inputBuffer = rest;
            break;

        case 'syntax':
            if(/^\s*;\s*$/.test(input)) {
                try {
                    var result = repl.evaluate(this._inputBuffer);
                    if(result != undefined)
                        repl.print(repl.represent(result));
                    repl._prompt();
                } catch(e) {
                    handleError(e);
                }

                this._inputBuffer = '';
            } else {
                this._inputBuffer += input;
                try {
                    var result = repl.evaluate(this._inputBuffer);
                    if(result != undefined)
                        repl.print(repl.represent(result));
                    repl._prompt();
                    this._inputBuffer = '';
                } catch(e if e.name == 'SyntaxError') {
                    // ignore and keep filling the buffer
                    repl._prompt(repl._name.replace(/./g, '.') + '> ');
                } catch(e) {
                    handleError(e);
                    this._inputBuffer = '';
                }
            }
        }
    }
};
function _migrateTopLevel(context) {
    if(this._hostContext instanceof Ci.nsIDOMWindow)
        this._hostContext.removeEventListener('unload', this._emergencyExit, false);

    this._hostContext[this._name] = undefined;
    this._hostContext = context;
    this._hostContext[this._name] = this;

    if(this._hostContext instanceof Ci.nsIDOMWindow)
        this._hostContext.addEventListener('unload', this._emergencyExit, false);
};
function _prompt(prompt) {
    return print('PARENSCRIPT> ', false);
};
function receive(input) {
    return this.currentInteractor().handleInput(this, input);
};
function chooseName(basename, context) {
    if (basename in context) {
        var i = 0;
        while (i + basename in context) {
            ++i;
        };
        return i + basename;
    } else {
        return basename;
    };
};
function isTopLevel(object) {
    return (object instanceof Ci.nsIDOMWindow) || 'wrappedJSObject' in object || 'NSGetModule' in object || 'EXPORTED_SYMBOLS' in object || object.__parent__ && 'EXPORTED_SYMBOLS' in object.__parent__;
};
function scan(string, separator) {
    var match101 = string.match(separator);
    return match101 ? [string.substring(0, match101.index), string.substr(match101.index + match101[0].length)] : [null, string];
};

function formatStackTrace(exception) {
    var trace = '';
    if(exception.stack) {
        var calls = exception.stack.split('n');
        for each(var call in calls) {
            if(call.length > 0) {
                call = call.replace(/\n/g, 'n');

                if(call.length > 200)
                    call = call.substr(0, 200) + '[...]n';

                trace += call.replace(/^/mg, 't') + 'n';
            }
        }
    }
    return trace;
}

function evaluate(code) {
    var _ = arguments.callee;
    if(typeof(_.TMP_FILE) == 'undefined') {
        _.TMP_FILE = Cc['@mozilla.org/file/directory_service;1']
            .getService(Ci.nsIProperties)
            .get('ProfD', Ci.nsIFile);
        _.TMP_FILE.append('mozrepl.tmp.js');

        _.TMP_FILE_URL = Cc['@mozilla.org/network/io-service;1']
            .getService(Ci.nsIIOService)
            .getProtocolHandler('file')
            .QueryInterface(Ci.nsIFileProtocolHandler)
            .getURLSpecFromFile(_.TMP_FILE);
    }

    var fos = Cc['@mozilla.org/network/file-output-stream;1']
        .createInstance(Ci.nsIFileOutputStream);
    fos.init(_.TMP_FILE, 0x02 | 0x08 | 0x20, 0600, 0);

    var os = Cc['@mozilla.org/intl/converter-output-stream;1']
        .createInstance(Ci.nsIConverterOutputStream);
    os.init(fos, 'UTF-8', 0, 0x0000);
    os.writeString(code);
    os.close();

    if(typeof(_.cacheKiller) == 'undefined')
        _.cacheKiller = 0;
    
    _.cacheKiller++;
    var scriptUrl = _.TMP_FILE_URL + '?' + _.cacheKiller;
    var result = loader.loadSubScript(scriptUrl, this._workContext, 'UTF-8');

    this.$$ = result;
    return result;
};
function LoadedScriptError(cause) {
    return this.cause = cause;
};
