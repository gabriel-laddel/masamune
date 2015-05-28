((:HTML :XMLNS "http://www.w3.org/1999/xhtml")
 (:HEAD (:TITLE "Browser User Manual") 
	((:LINK :REL "stylesheet" :TYPE "text/css" :HREF "file:///root/algol/conkeror/help/manual.css")))
 (:BODY (:H1 "User Manual") :HR (:H2 "Introduction")
	(:P "
      Conkeror is a Mozilla-based web browser whose design is inspired by
      "
	    ((:A :HREF "http://www.gnu.org/software/emacs") "GNU
      Emacs")
	    ".
    ")
	(:P "
      You can learn Conkeror's key bindings
      by "
	    ((:A :HREF "file:///root/algol/conkeror/help/tutorial.html") "reading the
      tutorial")
	    ". The keyboard shortcut to visit the tutorial
      is "
	    ((:SPAN :CLASS "key") "C-h t") ". That is, first
      press "
	    ((:SPAN :CLASS "key") "Ctrl-h") ", then release those keys, and
      press "
	    ((:SPAN :CLASS "key") "t") ".
    ")
  :HR (:H2 "Overview of the Keys") (:H3 "Browsing")
	(:TABLE
	 (:THEAD (:TR (:TH (:B "key")) (:TH (:B "M-x command")) (:TH (:B "meaning"))) "
      ")
	 (:TBODY (:TR (:TD ((:SPAN :CLASS "key") "g")) (:TD "find-url") (:TD "open new URL")) (:TR (:TD ((:SPAN :CLASS "key") "B")) (:TD "back") (:TD "&nbsp;"))
		 (:TR (:TD ((:SPAN :CLASS "key") "F")) (:TD "forward") (:TD "&nbsp;")) (:TR (:TD ((:SPAN :CLASS "key") "r")) (:TD "reload") (:TD "&nbsp;"))
		 (:TR (:TD ((:SPAN :CLASS "key") "C-g")) (:TD "abort") (:TD "stop")) (:TR (:TD ((:SPAN :CLASS "key") "C-h i")) (:TD "help-page") (:TD "Show this page."))
		 (:TR (:TD ((:SPAN :CLASS "key") "C-h t")) (:TD "tutorial") (:TD "Show the Conkeror tutorial."))))
	(:H3 "Movement")
	(:TABLE (:TR (:TD ((:SPAN :CLASS "key") "C-a")) (:TD "beginning of line")) (:TR (:TD ((:SPAN :CLASS "key") "C-e")) (:TD "end of line"))
		(:TR (:TD ((:SPAN :CLASS "key") "C-f")) (:TD "Forward a column")) (:TR (:TD ((:SPAN :CLASS "key") "C-b")) (:TD "backward a column"))
		(:TR (:TD ((:SPAN :CLASS "key") "C-n")) (:TD "Forward a line")) (:TR (:TD ((:SPAN :CLASS "key") "C-p")) (:TD "backward a line"))
		(:TR (:TD ((:SPAN :CLASS "key") "C-v")) (:TD "Page down")) (:TR (:TD ((:SPAN :CLASS "key") "M-v")) (:TD "Page up"))
		(:TR (:TD ((:SPAN :CLASS "key") "M-&lt;")) (:TD "Beginning of document")) (:TR (:TD ((:SPAN :CLASS "key") "M-&gt;")) (:TD "End of document"))
		(:TR (:TD ((:SPAN :CLASS "key") "C-s")) (:TD "Open i-search forward")) (:TR (:TD ((:SPAN :CLASS "key") "C-r")) (:TD "Open i-search backward")))
	(:H3 "I-Search")
	(:TABLE (:TR (:TD ((:SPAN :CLASS "key") "C-s")) (:TD "Search forward")) (:TR (:TD ((:SPAN :CLASS "key") "C-r")) (:TD "Search backward"))
		(:TR (:TD ((:SPAN :CLASS "key") "C-g")) (:TD "Quit i-search (jump back to where i-search started)"))
		(:TR (:TD ((:SPAN :CLASS "key") "backspace")) (:TD "Undo search")) (:TR (:TD "any modifier plus a key, RET or TAB") (:TD "Close i-search")))
	(:H3 "Webjumps")
	(:P "
      Webjumps are similar to, but potentially more powerful than Firefox's
      bookmark keywords.  You type a webjump name into the location prompt,
      followed by one or more search terms.  Conkeror substitutes your search
      terms into an url associated with the webjump.
    ")
	(:P "
      Conkeror has a few webjumps already, but you can find many more, and share
      your own at "
	    ((:A :HREF "http://conkeror.org/Webjumps") "the
      webjumps page of the conkeror wiki")
	    ".
    ")
	(:TABLE (:TR (:TD "conkerorwiki") (:TD "Search conkeror.org")) (:TR (:TD "answers") (:TD "search answers.com"))
		(:TR (:TD "bugzilla") (:TD "search mozilla bugzilla")) (:TR (:TD "clhs") (:TD "search the Common Lisp Hyper Spec"))
		(:TR (:TD "cliki") (:TD "search the Common Lisp wiki")) (:TR (:TD "creativecommons") (:TD "search creativecommons.org"))
		(:TR (:TD "dictionary") (:TD "Search dictionary.reference.com")) (:TR (:TD "duckduckgo") (:TD "Search with duckduckgo"))
		(:TR (:TD "ebay") (:TD "search ebay.com")) (:TR (:TD "google") (:TD "Search with google")) (:TR (:TD "image") (:TD "Search google images"))
		(:TR (:TD "kuro5hin") (:TD "search kuro5hin.com")) (:TR (:TD "lucky") (:TD "Google \"I'm feeling lucky\" search"))
		(:TR (:TD "maps") (:TD "Search Google Maps")) (:TR (:TD "ratpoisonwiki") (:TD "search the ratpoison wiki"))
		(:TR (:TD "savannah") (:TD "search savannah.gnu.org")) (:TR (:TD "scholar") (:TD "search google scholar"))
		(:TR (:TD "slang") (:TD "Search urbandictionary.com")) (:TR (:TD "slashdot") (:TD "search slashdot.com"))
		(:TR (:TD "sourceforge") (:TD "search sourceforge.net")) (:TR (:TD "stumpwmwiki") (:TD "search the StumpWM wiki"))
		(:TR (:TD "wikipedia") (:TD "Search wikipedia.org")) (:TR (:TD "wiktionary") (:TD "Search wiktionary.org")) (:TR (:TD "yahoo") (:TD "search yahoo")))
	(:P "
      delicious webjumps can be added by putting the following in your rc file:
    ")
	(:PRE "add_delicious_webjumps(\"myusername\");")
	(:P "
      this will create the following webjumps:
    ")
	(:TABLE (:TR (:TD "adelicious") (:TD "Add a delicious bookmark.")) (:TR (:TD "delicious") (:TD "View your delicious bookmarks"))
		(:TR (:TD "sdelicious") (:TD "Search your delicious bookmarks")) (:TR (:TD "sadelicious") (:TD "Search all delicious bookmarks")))
	(:P "
      lastfm webjumps can be added by putting the following in your rc file:
    ")
	(:PRE "define_lastfm_webjumps(\"myusername\");")
	(:P "
      this will create the following webjumps:
    ")
	(:TABLE (:TR (:TD "lastfm") (:TD "&nbsp;")) (:TR (:TD "lastfm-user") (:TD "&nbsp;")) (:TR (:TD "lastfm-music") (:TD "&nbsp;"))
		(:TR (:TD "lastfm-group") (:TD "&nbsp;")) (:TR (:TD "lastfm-tag") (:TD "&nbsp;")) (:TR (:TD "lastfm-label") (:TD "&nbsp;"))
		(:TR (:TD "lastfm-event") (:TD "&nbsp;")))
	(:P "
      You can easily add your own custom webjumps as such:
    ")
	(:PRE "define_webjump(\"reddit\", \"http://www.reddit.com/search?q=%s\");")
	(:P "
      The above will define the \"reddit\" webjump which
      searches "
	    ((:A :HREF "http://www.reddit.org") "reddit") "
      threads. The \"%s\" in the URL will be replaced by the search term you enter
      after the webjump name. I.e. entering \"reddit haskell compiler\" in the
      minibuffer will redirect you to the search results for reddit threads
      matching \"haskell compiler\" as a search string.
    ")
	(:H3 "Buffer Management")
	(:TABLE (:TR (:TD ((:SPAN :CLASS "key") "C-u g")) (:TD "Open an URL in a new buffer"))
		(:TR (:TD ((:SPAN :CLASS "key") "C-x b")) (:TD "Select a buffer based on it's name.")) (:TR (:TD ((:SPAN :CLASS "key") "M-p")) (:TD "previous buffer"))
		(:TR (:TD ((:SPAN :CLASS "key") "M-n")) (:TD "Next buffer")) (:TR (:TD ((:SPAN :CLASS "key") "C-x k")) (:TD "kill buffer"))
		(:TR (:TD ((:SPAN :CLASS "key") "C-x 5 f") " or " ((:SPAN :CLASS "key") "C-u C-u g")) (:TD "Open an URL in a new window"))
		(:TR (:TD ((:SPAN :CLASS "key") "C-x 5 0")) (:TD "Close the current window (and all buffers in it)"))
		(:TR (:TD ((:SPAN :CLASS "key") "C-x C-c")) (:TD "Quit conkeror")))
	:HR (:H2 "Universal Argument")
	(:P "
      Conkeror support the universal
      argument, "
	    ((:SPAN :CLASS "key") "C-u") ". It's a prefix binding that changes
      how a command behaves. In Conkeror, "
	    ((:SPAN :CLASS "key") "C-u") " has two
      main effects. The first effect is that the command will be executed
      multiple times. For example, typing "
	    ((:SPAN :CLASS "key") "C-u C-n") " will
      cause conkeror to scroll down 4 lines. "
	    ((:SPAN :CLASS "key") "C-u 12 C-n") "
      will cause conkeror to scroll down 12 lines.  The second effect is to open
      in a new buffer or a new window. "
	    ((:SPAN :CLASS "key") "C-u g conkeror.org
      RET")
	    " This opens the conkeror project web page in a new
      buffer. "
	    ((:SPAN :CLASS "key") "C-u C-u n 12 RET") " will open link no. 12 in
      a new window.
    ")
	(:P "
      There are some commands where the effect is ambiguous. Does
      "
	    (:CODE "C-u B") " go back four pages in the history or does it go back
      one and open the result in a new buffer? We are working on adding a second
      universal argument that would allow you to do both.
    ")
	"

    "
  :HR (:H2 "Conkeror Resources") (:UL (:LI ((:A :HREF "http://conkeror.org/") "Conkeror homepage")))))
