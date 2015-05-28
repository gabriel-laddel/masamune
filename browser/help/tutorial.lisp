((:HTML :XMLNS "http://www.w3.org/1999/xhtml")
 (:HEAD (:TITLE "Browser Tutorial") 
	((:LINK :REL "stylesheet" :TYPE "text/css" :HREF "file:///root/algol/conkeror/help/manual.css")))
 (:BODY (:H1 "Browser Tutorial")
	(:P " If you're not comfortable with a QWERTY keyboard, you can rotate through keyboard layouts with F1.")
	(:P "
  The key sequence to exit conkeror (the web browser you're currently using) is "
	    ((:CODE :CLASS "key") "C-x C-c") ".
")
	(:P ((:CODE :CLASS "key") "C-g") " is the abort key.  It is used to cancel a command
  from the minibuffer, or stop a web page from loading.
")
	(:P ((:CODE :CLASS "key") "Space") " and " ((:CODE :CLASS "key") "Backspace") " scroll a page down and
  a page up, respectively.
")
	(:H3 "Basic Navigation")
	(:P "
  To browse to an URL in a new content buffer, type "
	    ((:CODE :CLASS "key") "C-x
  C-f")
	    ".  This key sequence works from anywhere in Conkeror.
  Additionally, if you are already in a content buffer (as opposed to a
  buffer showing download progress or help), you can navigate the current
  buffer to a new url with the "
	    ((:CODE :CLASS "key") "g") " (find-url) key.
  The mnemonic is \"go\".  The key sequence "
	    ((:CODE :CLASS "key") "C-x C-v") "
  (find-alternate-url) is also available in content buffers.  It is
  identical to "
	    ((:CODE :CLASS "key") "g") " except that the prompt will be
  pre-filled with the current URL for you to edit.
")
	(:P "
  The "
	    ((:CODE :CLASS "key") "B") " key (note uppercase) is bound to
  the "
	    (:CODE "back") " command.  Practice basic navigation by
  typing "
	    ((:CODE :CLASS "key") "g") " and entering the text \"about:\" in
  the url prompt.  To come back to this tutorial from the about: page,
  type "
	    ((:CODE :CLASS "key") "B") ".
")
	(:H3 "Hinting")
	(:P "
  The system for interacting with the elements of a document is called
  the hinting system.  With the hinting system, you can do things like
  follow hyperlinks, copy URLs, focus form fields, and more.  Let's
  use the hinting system to follow a hyperlink.  You will receive
  instructions about how to return here after you follow the
  hyperlink.  If the numbered list and hyperlink following this
  paragraph are not in view, scroll so that they are.
")
	(:OL (:LI "Press " ((:CODE :CLASS "key") "f") ".  (mnemonic: follow)")
	     (:LI "The hyperlink should be hilighted, with a number on the left
  side.  Type that number and hit enter."))
	((:A :HREF "file:///root/algol/conkeror/help/back-forward.html") "This is a
hyperlink.")
	(:P "
  As a matter of fact, in addition to following links by number, you can
  also follow them by a substring of the link text.  You could select the
  link above by typing "
	    ((:CODE :CLASS "key") "f t h i s") ".  When more than
  one link with similar text is in view, hinting will narrow down the
  choices as you type.  You can always specify the one you want by
  disambiguating the sequence with the hint number.
")
	(:P "
  But hinting is for more than just hyperlinks.  Conkeror provides
  several "
	    (:EM "hint classes") " for operating on different types of
  elements.  The hint class is specified by typing the corresponding
  key sequence of the class before the key sequence of the command.
  It was not necessary to specify a hint class for following the
  hyperlink above because links are the default hint class for the
  follow command.  The following table lists the most common hint
  classes.
")
	"

"
	(:TABLE (:TR (:TD (:EM "key")) (:TD (:EM "hint class")) (:TD (:EM "mnemonic")))
		(:TR (:TD ((:CODE :CLASS "key") "n")) (:TD "links and form elements") (:TD "li" (:B "n") "k"))
		(:TR (:TD ((:CODE :CLASS "key") "i")) (:TD "images") (:TD (:B "i") "mage"))
		(:TR (:TD ((:CODE :CLASS "key") "m")) (:TD "frameset frames and top window") (:TD "fra" (:B "m") "e"))
		(:TR (:TD ((:CODE :CLASS "key") "* *")) (:TD "dom nodes") (:TD "wildcard")))
	(:P "
  In the hinting system, hint number zero (0) is special.  It refers to the
  current URL in the buffer.
")
	(:P "
  The "
	    ((:CODE :CLASS "key") "c") " key is bound to the copy command.  Its
  default hint class is links.  To copy the URL of the page you are
  currently browsing, type "
	    ((:CODE :CLASS "key") "c 0") ".  To copy the url of
  a frame or iframe, type "
	    ((:CODE :CLASS "key") "m c") ".  If there is more
  than one frame visible, you will be prompted for a number, just as with
  hyperlinks.
")
	(:P "
  When none of the conventional hint classes cover the element that
  you want to operate on, you can use the dom-nodes hint class, to
  choose from among all visible dom nodes.  This hint class is bound
  to "
	    ((:CODE :CLASS "key") "* *") ".  Try copying the text of this
  paragraph now by typing "
	    ((:CODE :CLASS "key") "* * c") " followed by
  the hint number for this paragraph.
")
	(:H3 "Finding Text (Isearch)")
	(:P ((:CODE :CLASS "key") "C-s") " invokes interactive text search mode, isearch
  for short.  "
	    ((:CODE :CLASS "key") "C-r") " invokes reverse isearch.  In this
  mode, just type the text you want to find into the minibuffer and Conkeror
  will search the document as you type.  To jump to the next match,
  hit "
	    ((:CODE :CLASS "key") "C-s") " again.  To jump to the previous match,
  hit "
	    ((:CODE :CLASS "key") "C-r") ".  To conclude your search,
  hit "
	    ((:CODE :CLASS "key") "return") ", or to abort it,
  press "
	    ((:CODE :CLASS "key") "escape") " or " ((:CODE :CLASS "key") "C-g") ".  If
  you have previously concluded an isearch, you can use the
  keys "
	    ((:CODE :CLASS "key") "S") " and " ((:CODE :CLASS "key") "R") " to jump to
  next and previous matches without going back into isearch mode.  This is
  called non-interactive resume.  To resume your last successful isearch in
  interactive mode, hit "
	    ((:CODE :CLASS "key") "C-s") "
  or "
	    ((:CODE :CLASS "key") "C-r") " twice.
")
	(:H3 "Copying Text")
	(:P "
  Apart from "
	    ((:CODE :CLASS "key") "* * c") " described above, there are other
  ways to copy text for times when what you want to copy does not neatly
  fill one dom node.  The process is straight-forward.  First, use isearch
  ("
	    ((:CODE :CLASS "key") "C-s") ") and search for the start of the text you
  want to copy.  When you have brought the search to the start of the text
  you want to copy, hit "
	    ((:CODE :CLASS "key") "return") " to conclude the
  search.  Although no cursor is visible, there is an invisible cursor where
  the search concluded.  Hold the "
	    ((:CODE :CLASS "key") "shift") " key and use
  the arrow keys to make a selection.  When you have selected the text you
  want to copy, release all keys and hit "
	    ((:CODE :CLASS "key") "M-w") " to
  copy the text to the clipboard.
")
	(:P (:B "Tip:") " Although isearch is usually the quickest way to position the
  cursor, there are other ways.  One of them is to use the "
	    (:EM "focus
  link")
	    " browser-object command (" ((:CODE :CLASS "key") "n ;") ").  When you
  focus a link, the invisible cursor will be left at the first character of
  the link text, and you can use shifted arrow keys to make a selection from
  that point.  If the methods outlined so far are unsuitable for a situation
  you encounter, then you can use "
	    (:CODE "M-x caret-mode") " to get a small
  but visible blinking cursor that you can move to where you need it.
  Call "
	    (:CODE "M-x caret-mode") " again to toggle it off.
")
	"

"
	(:H3 "More information on the browser")
	(:P "
  Now that you've completed the tutorial, you should have the basic skills to navigate the web.  There is however, much more to learn about conkeror. You can start with the User Manual, accessible via " ((:code :class "key") "C-h i") ". The manual is somewhat incomplete and should be supplemented with online resources such as the "
	    ((:A :HREF "http://conkeror.org/QuickStart") "QuickStart") " and "
	    ((:A :HREF "http://conkeror.org/") "Conkeror Wiki.") " You can always return to this tutorial via " ((:CODE :CLASS "key") "C-h t")) 
	(:H1 "Continuing the Masamune Tutorial")
	(:P ((:CODE :CLASS "key") "C-t ?") 
	    " will display a partial list of the window manager's current keybindings. Focus Emacs via " ((:CODE :CLASS "key") "C-t e")
	    " and visit the file " ((:a :href "file:///root/quicklisp/local-projects/masamune/introduction.lisp") 
	 "~/quicklisp/local-projects/masamune/introduction.lisp") " using "((:CODE :CLASS "key") "C-x C-f")  " (Emacs and the browser share many of the same keybindings).")))
