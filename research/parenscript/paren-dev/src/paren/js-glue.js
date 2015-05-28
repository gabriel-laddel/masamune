function refreshCss() {
    function toA(x) {
        var a =[]; for (var i=0; i < x.length; i++) { a.push(x[i]); }; return a;
    };
    var links = toA(document.getElementsByTagName('link')).filter(function(x) { return x.getAttribute("rel") == "stylesheet"; });
    function newHref(href) { href = href.replace(/(&|\?)forceReload=d /,""); href = href + (href.indexOf('?')>=0 ? '&':'?') + 'forceReload=' + (new Date().valueOf()); return href; };
    links.map(function(x) { x.href = newHref(x.href); });
}