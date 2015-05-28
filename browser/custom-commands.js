// peek
// =============================================================================
// The peek command (and its helper family of aliases for some common RegExp filters) mimic repl.inspect, but sorting output a bit more orderly by type, and by default only lists direct properties of the object inspected. Pass a second numeric argument N to also list inherited properties to the N:th level, and/or a property name RegExp, and/or a property type RegExp, to list only the subset of key/values of the peeked object that match given criteria:

// repl> repl.peek(this);
// object ChromeWindow // this is the type of the object passed, here: "this"
// boolean: // first, all boolean properties and their values are listed, in alpha order:
//   OPT_IN = true
//   a11yEnabled = false
//   gBidiUI = false
//   gInPrintPreviewMode = false
//   gIsLoadingBlank = false
//   gMustLoadSidebar = false
//   gPrintSettingsAreGlobal = false
//   gSavePrintSettings = false
// function: // then all javascript functions (that you can inspect with fn.toSource())
//   $
// // ...
// native function: // next, all the native functions (whose function body is "{[native code]}")
//   XPCNativeWrapper
//   XPCSafeJSObjectWrapper
//   addEventListener
//   getInterface
// null: // as you see, types are also listed in alpha order
//   gChromeState
//   gClickAndHoldTimer
//   gContextMenu
//   gFocusedElement
//   gPrevCharset
//   gProgressCollapseTimer
// number:
//   MAX_FOLDER_ITEM_IN_MENU_LIST = 5
//   MAX_HISTORY_MENU_ITEMS = 15
//   MOUSE_SCROLL_IS_HORIZONTAL = 4
//   MOUSE_SCROLL_ZOOM = 3
// // ...
// object Array: // all normal js arrays, like ["hi!", 4711]
//   zzz
// object HTMLAnchorElement: // more esoteric types also shown
//   node
// object Location:
//   location
// object Navigator:
//   navigator
// object Object: // these are all js literals like { "hi": 4711 }
//   BookmarksEventHandler
//   BookmarksMenuDropHandler
//   BrowserOffline
// // ...
// self [object ChromeWindow]: // all these are self-referential properties, i e this.window === this
//   Firenomics
//   window
// string: // for strings, we only show <= 40-character string contents:
// // ...
//   NEWLINE = "\n"
//   ORGANIZER_FOLDER_ANNO = "PlacesOrganizer/OrganizerFolder"
//   ORGANIZER_QUERY_ANNO = "PlacesOrganizer/OrganizerQuery"
//   ORGANIZER_ROOT_BOOKMARKS = String(/* 54 chars */)
// undefined: // and finally, all the properties whose value is the undefined value:
//   gWebPanelURI
//   prev
 // repl> repl.peek(this, /move/i)
// object ChromeWindow

// 5/523 matches // only five directo property names match "move" case insensitively:
// number:
//   RELOAD_ACTION_MOVE = 3
//   RELOAD_ACTION_REMOVE = 2
//   REMOVE_PAGES_CHUNKLEN = 300
//   REMOVE_PAGES_MAX_SINGLEREMOVES = 10
// object Object:
//   gEditItemOverlay
 // repl> repl.peekNative(this);
// object ChromeWindow // again, the type of the given object

// 4/523 matches // only 4 of the 523 direct properties matched the filter
// native function:
//   XPCNativeWrapper
//   XPCSafeJSObjectWrapper
//   addEventListener
//   getInterface
 // repl> repl.peekObj(/^XPC/, mozrepl)
// object Object // bnothing special about the mozrepl object
 
// 2/12 matches // 2 of its 12 direct properties are objects of an XPC* type
// object XPCWrappedNative_NoHelper:
//   pref
//   server

// Prints all the direct (or prototype chain to the `depth`:ed level) properties
// of "at" in the repl, optionally matching regexp `re` only -- ordered by type.
function peek(at, depth, re, typeRe) {
  function lsKeys(data, level, matches, total) {
    var types = [], type, header = '';
    for (type in data) {
      if (!data.hasOwnProperty(type)) continue;
      types.push(type);
    }

    if (matches !== total)
      header = matches +'/'+ total +' matches';
    if (level)
      header += (header ? ' on ': '') +'prototype ancestor '+ level +':';
    if (header) repl.print('\n' + header);

    types.sort().forEach(function ls(type) {
      repl.print(type +':\n  '+ (data[type].sort().join('\n  ')));
    });
  }

  if ('number' !== typeof depth) {
    typeRe = re;
    re = depth;
    depth = 0;
  }

  var l = 0, self = at, self_type = 'self ['+ typeOf(self) +']', intp = /^\d+$/;
  do {
    var type = typeOf(at);
    repl.print(type + (/^(?:number|string)$/.test(type) ? ': '+ at : ''));
    if (/^(?:number|string|null|undefined)$/.test(type)) return;

    // data[type] = [key, key, ...]
    var data = {}, key, val, count = 0, matches = 0;
    for (key in at) {
      if (!at.hasOwnProperty(key)) continue;
      if (self_type === 'self [object Array]' && intp.test(key)) continue;
      ++count;
      if (re && !re.test(key)) continue;
      ++matches;

      try {
        val = at[key];
        type = typeOf(val);
      } catch (e) {
        type = 'unknown';
      }
      if (typeRe && !typeRe.test(type)) {
        --matches;
        continue;
      }

      if (self === val)
        type = self_type;
      else if ('boolean' === type || 'number' === type)
        key += ' = '+ val;
      else if ('string' === type)
        if (val.length < 40)
          key += ' = '+ uneval(val);
        else
          key += ' = String(/* '+ val.length +' chars */)';
      data[type] = (data[type] || []).concat(key);
    }
    lsKeys(data, l++, matches, count);
  } while (depth-- && (at.__proto__ !== at) && (at = at.__proto__));
}

// Reports useful types such as "string", "number", "null", "native function",
// "function", "undefined", "object Object", "object Array", "object RegExp",
// and the special identity type "self[whatever the type of `at` itself was]"
function typeOf(x) {
  var k, t = null == x ? null === x ? 'null' : 'undefined' : typeof x;
  if ('function' === t)
    return /\{\x5Bnative code\x5D\}$/.test(x.toSource()) ? 'native ' + t : t;
  if ('object' !== t || !(k = Object.prototype.toString.call(x)) ||
      !(k = /^\x5Bobject (.*)\x5D$/.exec(k)) || !(k = k[1]))
    return t; // number, string, null, undefined, super-weird "object"s
  // /^(Array|Object|RegExp)$/.test(k) ? k :
  return t +' '+ k; // typed objects
}

// Peeks at JSONable properties only. Remember to pass an Infinity depth, if you
// want everything that serializing the object would pick up.
function peekJSON(o, depth, re) {
  peek(o, depth || 0, re,
       /^(?:boolean|number|string|object Object|object Array|null)$/);
}

function peekUnJSON(o, depth, re) {
  peek(o, depth || 0, re,
       /^(?!(boolean|number|string|object Object|object Array|null)$)/);
}

// Peeks at members of type object only. If arg 0 is a RegExp, only peek at the
// objects of a type matching that RegExp; ie peekObj(/^nsXPC/, this) will only
// list the nsXPCComponents, nsXPCComponents_Classes, nsXPC... type members.
function peekObj(o, depth, re, typeRe) {
  if ('string' === typeof o || 'object RegExp' === typeOf(o)) {
    var flags = o.ignoreCase === true ? 'i' : '';
    if ('string' != typeof o) o = o.toSource().replace(/^\/|\/[a-z]*$/g, '');
    o = '^object '+ (o.replace(/^(?!\^)/, '.*').replace(/^\^/, ''));
    return peekObj(depth, re, typeRe, new RegExp(o, flags));
  }
  return peek(o, depth || 0, re, typeRe || /^object /);
}

function peekFn(o, depth, re) {
  peek(o, depth || 0, re, /^(?!native )?function$/);
}

function peekNative(o, depth, re) {
  peek(o, depth || 0, re, /^native function$/);
}

// WINDOWS
// =============================================================================
// repl> var windows = repl.getWindows()

// This function will cause the REPL to throw an error if you try to pass the resulting array back directly, but getWindows()[i] and getWindows().length work fine.

function getWindows() {
    var windowEnum = Cc['@mozilla.org/appshell/window-mediator;1']
        .getService(Ci.nsIWindowMediator).getEnumerator('');
    var windows = [];
    while(windowEnum.hasMoreElements())
        windows.push(windowEnum.getNext());

    return windows;
}

// grab
// =============================================================================
// repl> var res = repl.grab();
// repl> /* click an element in the browser... */
// repl> alert(res.event.target);

function grab() {
    if(this._workContext instanceof Ci.nsIDOMWindow)
        var window = this._workContext;
    else
        throw new Error('Not in a window.');

    var prevTitle = window.top.title;
    var prevEl, prevColor;
    function onOver(event) {
        var curEl = event.target;

        window.top.title =
            '<' + curEl.nodeName + '> in ' + curEl.ownerDocument.location.href;

        if(prevEl)
            prevEl.style.backgroundColor = prevColor;

        prevEl = curEl;
        prevColor = curEl.style.backgroundColor;

        curEl.style.backgroundColor = '#E6E5C8';
    };

    var repl = this;
    function onClick(event) {
        result.event = event;
        repl.highlight(event.target);
        event.stopPropagation();
        finished();
    };

    function finished() {
        window.document.removeEventListener('click', onClick, true);
        window.document.removeEventListener('mouseover', onOver, true);
        prevEl.style.backgroundColor = prevColor;
        window.top.title = prevTitle;
    }

    var result = {};
    window.document.addEventListener('click', onClick, true);
    window.document.addEventListener('mouseover', onOver, true);
    return result;
}
