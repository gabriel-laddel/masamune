
/*
*	QEvent 0.96 - a powerful tiny extensible standalone event library for elements and objects
*
*		- lightweight footprint
*		- no namespace pollution - everything is wrapped in obj.$QEvent
*		- normalizes the DOM event model
*		- fixes common IE bugs including: all common used event properties, IE 2px bug http://ajaxian.com/archives/javascript-tip-cross-browser-cursor-positioning, fix many IE leaks, event handlers are FIFO executed
*		- prevents repeated registration of same type and listener
*		- 'this' in listeners references to object or element itself
*		- fixes window 'beforeunload' issue (doesn't work in Opera) 
*		- fixes window 'unload' (must be removed by itself)
*		- fixes focus and blur events
*
*		- DOM events are more extendable: event keys, event objects ( event.myCustomFn() ), custom events
*		  firing events works for elements
*		- tested on (IE6-7, Firefox2/3, Safari, Opera, Chrome) 
*
*	Methods:
*
*		QEvent.add	        - attach event listener to element or object, DOM event will be fixed
*		QEvent.remove		- detach event.. you know
*		QEvent.fire		- fire event
*
*		QEvent.addOnce		- event listener will be removed after first call
*		QEvent.toggle		- attach or detach by fourth parameter
*
*		QEvent.addDom		- attach DOM event listener, 
*		QEvent.removeDom	- detach DOM event listener
*
*	Methods add, remove, addOnce, toggle accept also objects - add(el, { type: fn })
*	Examples: see index.htm
*
*	Custom Events
*
*		These custom events are included, view source to see how to create them.
*		
*		- keyenter
*		- mouseOverOut
*		- mouseenter, mouseleave
*		- mousehover
*		- mousewheel
*		- domready
*		- beforeunload - doesn't work in Opera
*		- clickout - also example to see how to attach custom DOM event
*		- focus, blur
*
*	To see how QEvent could be extended, look at bottom of this script.
*
*	License: MIT-style license.
*	Copyright: Copyright (c) 2008 Daniel Steigerwald, daniel.steigerwald.cz
*
*	Notes: 
*			- some ideas and snippets come from MooTools, see source
*			- there is also one potential leak for IE, in case of load event is not fired, 
*			  also unload will not be fired, maybe I will fixed it in future
*/

var QEvent = (function() {

	var toPurge = [],
		isIe = document.attachEvent && !document.addEventListener,
		isGecko = !isIe && navigator.taintEnabled && !!document.getBoxObjectFor,
		winProp = isIe ? 'parentWindow' : 'defaultView',
		$QEVENTS = '$QEVENTS';

	var isDomObj = function(obj) {
		return !!(obj.addEventListener || obj.attachEvent);
	};

	// obj - element, document or window
	var getWindow = function(obj) {
		return obj[winProp] || (obj.ownerDocument && obj.ownerDocument[winProp]) || obj;
	};

	var prepare = function(fn) {
		return function(obj, type, arg) {
			if (typeof type == 'object') for (var k in type) arguments.callee(obj, k, type[k]);
			else {
				var isDom = isDomObj(obj),
					events = obj[$QEVENTS] = obj[$QEVENTS] || {}, // events storage lazy creation
					storage = events[type] = events[type] || { listeners: [], domFixed: isDom && [] };
				fn(obj, type, arg, storage, isDom);
			}
		};
	};

	var add = prepare(function(obj, type, fn, storage, isDom) {
		var listeners = storage.listeners;
		for (var i = listeners.length; i--; ) if (listeners[i] == fn) return; // prevents repeated registration of same type and listener 
		listeners.push(fn);
		if (!isDom) return;
		var custom = QEvent.DomEvents[type];
		if (custom) {
			if (custom.onAdd) custom.onAdd.call(obj, fn);
			if (custom.condition) {
				var old = fn;
				fn = function(e) {
					if (custom.condition.call(obj, e)) old.call(obj, e);
				};
			}
			type = custom.base || type;
		}
		if (!type) return; // never attach custom event without base event
		var win = getWindow(obj);
		storage.domFixed.push(function(e) {
			e = e || win.event;
			if (e) e = new DomEvent(e, win);
			fn.call(obj, e);
		});
		if (storage.handler) return; // event handlers are FIFO executed by this also in IE
		addDom(obj, type, storage.handler = function(e) {
			for (var i = 0, l = storage.domFixed.length; i < l; i++) storage.domFixed[i](e);
		});
	});

	var remove = prepare(function(obj, type, fn, storage, isDom) {
		var listeners = storage.listeners;
		for (var i = listeners.length; i--; ) {
			if (listeners[i] === fn) {
				listeners.splice(i, 1);
				if (isDom) {
					var custom = QEvent.DomEvents[type];
					if (custom && custom.onRemove) custom.onRemove.call(obj, fn);
					var fixed = storage.domFixed;
					fixed.splice(i, 1);
					if (!fixed.length) removeDom(obj, type, storage.handler);
				}
				break;
			}
		}
	});

	var fire = prepare(function(obj, type, args, storage, isDom) {
		if (!args || typeof args.constructor != Array) args = [args];
		for (var i = 0, l = storage.listeners.length; i < l; i++)
			storage.listeners[i].apply(obj, args);
	});

	var addOnce = prepare(function(obj, type, fn, storage, isDom) {
		add(obj, type, function() {
			remove(obj, type, arguments.callee);
			fn.apply(obj, arguments);
		});
	});

	var toggle = prepare(function(obj, type, fn, storage, isDom, toggle) {
		(toggle ? add : remove)(obj, type, fn);
	});

	var addDom = function(el, type, fn) {
		// fix unload event, this event have to be unloaded by itself
		if (type == 'unload') {
			var old = fn;
			fn = function() {
				removeDom(el, 'unload', fn);
				old();
			};
		}
		else toPurge.push([el, type, fn]);
		if (el.addEventListener) el.addEventListener(type, fn, QEvent.useCapture[type]);
		else el.attachEvent('on' + type, fn);
	};

	var removeDom = function(el, type, fn, useCapture) {
		if (type != 'unload') removeFromPurge(el, type, fn);
		if (el.removeEventListener) el.removeEventListener(type, fn, QEvent.useCapture[type]);
		else el.detachEvent('on' + type, fn);
	};

	// almost copied from MooTools Event.js
	function DomEvent(event, win) {
		var doc = win.document, type = event.type, target = event.target || event.srcElement;
		while (target && target.nodeType == 3) target = target.parentNode;
		if (/key/.test(type)) {
			var code = event.which || event.keyCode,
				key = QEvent.DomKeys[code];
			if (type == 'keydown') {
				var fKey = code - 111;
				if (fKey > 0 && fKey < 13) key = 'f' + fKey;
			}
			key = key || String.fromCharCode(code).toLowerCase();
		}
		else if (/(click|mouse|menu)/i.test(type)) {
			var standardMode = !doc.compatMode || doc.compatMode == 'CSS1Compat';
			doc = standardMode ? doc.documentElement : doc.body;
			var page = {
				x: event.pageX || event.clientX + doc.scrollLeft,
				y: event.pageY || event.clientY + doc.scrollTop
			};
			var client = {
				x: (event.pageX) ? event.pageX - win.pageXOffset : event.clientX,
				y: (event.pageY) ? event.pageY - win.pageYOffset : event.clientY
			};
			// fix ie 2px bug			
			if (isIe && standardMode) {
				var cl = doc.clientLeft, ct = doc.clientTop;
				page.x -= cl, page.y -= ct;
				client.x -= cl, client.y -= ct;
			}
			if (/DOMMouseScroll|mousewheel/.test(type)) {
				var wheel = (event.wheelDelta) ? event.wheelDelta / 120 : -(event.detail || 0) / 3;
			}
			var rightClick = event.which == 3 || event.button == 2,
				related = null;
			if (type.match(/over|out/)) {
				switch (type) {
					case 'mouseover': related = event.relatedTarget || event.fromElement; break;
					case 'mouseout': related = event.relatedTarget || event.toElement;
				}
				if (isGecko) try {
					while (related && related.nodeType == 3) related = related.parentNode;
				}
				catch (e) { related = false; }
			}
		}
		this.event = event, this.type = type;
		this.page = page, this.client = client, this.rightClick = rightClick;
		this.wheel = wheel;
		this.relatedTarget = related, this.target = target;
		this.code = code, this.key = key;
		this.shift = event.shiftKey, this.control = event.ctrlKey, this.alt = event.altKey, this.meta = event.metaKey;
	};

	function extendDomEvent(obj) {
		for (var k in obj) DomEvent.prototype[k] = obj[k];
	};

	// garbaging for IE
	function purge() {
		for (var i = toPurge.length; i--; ) {
			var item = toPurge[i], el = item[0];
			removeDom.apply(null, item);
			el[$QEVENTS] = null;
			if (el.clearAttributes) el.clearAttributes();
		}
		toPurge = null;
	}

	function removeFromPurge(el, type, fn) {
		for (var i = toPurge.length, item; i--; ) {
			item = toPurge[i];
			if (item[0] == el && item[1] == type && item[2] == fn) {
				toPurge.splice(i, 1);
				break;
			}
		}
	}

	if (isIe) addDom(window, 'unload', purge);

	return {
		version: '0.95',
		add: add,
		remove: remove,
		fire: fire,
		addOnce: addOnce,
		toggle: toggle,
		addDom: addDom,
		removeDom: removeDom,
		isIe: isIe,
		isGecko: isGecko,
		extendDomEvent: extendDomEvent,
		useCapture: { blur: true, focus: true }
	};
})();

// EXTENSIONS

// custom DOM events
(function() {

	function hasChild(parent, child) {
		(hasChild = parent.contains
			? function(parent, child) { return parent != child && parent.contains(child); }
			: function(parent, child) { return !!(parent.compareDocumentPosition(child) & 16); }
		)(parent, child);
	};

	function mouseEnterLeaveCheck(e) {
		var related = e.relatedTarget;
		if (related == undefined) return true;
		if (related === false) return false;
		return (this.nodeType != 9 && related != this && related.prefix != 'xul' && !hasChild(this, related));
	};

	QEvent.DomEvents = {

		keyenter: {
			base: 'keyup',
			condition: function(e) {
				return e.key == 'enter';
			}
		},

		mouseOverOut: {
			onAdd: function(fn) {
				QEvent.add(this, { mouseover: fn, mouseout: fn });
			},
			onRemove: function(fn) {
				QEvent.remove(this, { mouseover: fn, mouseout: fn });
			}
		},

		mouseenter: {
			base: 'mouseover',
			condition: mouseEnterLeaveCheck
		},

		mouseleave: {
			base: 'mouseout',
			condition: mouseEnterLeaveCheck
		},

		mousehover: {
			onAdd: function(fn) {
				QEvent.add(this, { mouseenter: fn, mouseleave: fn });
			},
			onRemove: function(fn) {
				QEvent.remove(this, { mouseenter: fn, mouseleave: fn });
			}
		},

		mousewheel: {
			base: QEvent.isGecko ? 'DOMMouseScroll' : 'mousewheel'
		},

		// can be used on window or document
		domready: {
			onAdd: function(fn) {
				var domready = QEvent.DomEvents.domready;
				if (domready.fired) fn.call(this);
				else if (!domready.registered) {
					domready.registered = true;
					var fire = function() {
						if (domready.fired) return;
						domready.fired = true;
						QEvent.fire(window, 'domready');
						QEvent.fire(document, 'domready');
					};
					var doc = this.nodeType == 9 ? this : this.document;
					if (QEvent.isIe) setTimeout(function() {
						try { doc.documentElement.doScroll('left'); }
						catch (ex) {
							setTimeout(arguments.callee, 20);
							return;
						}
						fire();
					}, 20);
					else QEvent.addDom(doc, 'DOMContentLoaded', fire);
				}
			}
		},

		// doesnt work in opera.., it's special event, but useful. 
		// Listener has to return string, which will serve as a leaving question.
		beforeunload: {
			onAdd: function(fn) {
				window.onbeforeunload = function() {					
					return fn();
				}
				QEvent.add(window, 'unload', function() {
					window.onbeforeunload = null;
				});				
			}
		}
	}

})();

// clickout
(function() {

	var doc = top.document;

	var check = function(e, el) {
		var t = e.target;
		while (t && t.nodeType) {
			if (el == t) return false;
			t = t.parentNode;
		}
		return true;
	}

	QEvent.DomEvents.clickout = {

		onAdd: function(fn) {
			var el = this;
			QEvent.add(doc, 'click', fn.$QEventClickout = function(e) {
				if (check(e, el)) fn();
			});
		},

		onRemove: function(fn) {
			QEvent.remove(doc, 'click', fn.$QEventClickout);
		}

	};

})();

// focus blur fix
(function(base, real) {
	var E = QEvent;
	E.DomEvents[base] = {
		onAdd: function(fn) {
			E.add(this, real, fn);
		},
		onRemove: function(fn) {
			E.remove(this, real, fn);
		}
	};
	return arguments.callee;
})
('focus', QEvent.isIe ? 'focusin' : 'focus')
('blur', QEvent.isIe ? 'focusout' : 'blur');

// DOM event keys
QEvent.DomKeys = {
	13: 'enter',
	38: 'up',
	40: 'down',
	37: 'left',
	39: 'right',
	27: 'esc',
	32: 'space',
	8: 'backspace',
	9: 'tab',
	46: 'delete'
};

// DOM event methods
QEvent.extendDomEvent({

	stop: function() {
		return this.stopPropagation().preventDefault();
	},

	stopPropagation: function() {
		if (this.event.stopPropagation) this.event.stopPropagation();
		else this.event.cancelBubble = true;
		return this;
	},

	preventDefault: function() {
		if (this.event.preventDefault) this.event.preventDefault();
		else this.event.returnValue = false;
		return this;
	},

	isOver: function() {
		return this.type == 'mouseover';
	}

});	