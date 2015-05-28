(todo "move by highlighted symbol / replace highlighted symbol" :group :editing)
(todo "move by highlighted symbol" :group :editing)
(todo "modify standard-buffer to use an obinseq-based structure for faster line operations")
(todo "download and hook up to the common lisp hyperspec")
(todo "speed up com-goto-line (when possible)")
(todo "replace the use of the scroller pane by custom pane")
(todo "vertical scrolling")
(todo "fix horizontal scrolling such that the scrollbar can be used to move the point offscreen"
(todo "speaking of which, why have typeout panes at all? Or at least why
  not make them full-fledged climacs panes such that one can type in
  them, copy/paste, etc...?")
(todo "support M-Right and M-Left in masamune mode")
(todo "C-k at the end of a line breaks in masamune mode")
(todo "support cycling through possible choices for windows to switch to in C-x b")
(todo "allow for selecting among possibilities in a DEFINITION view")
(todo "allow for closing individual panes")
(todo "make tab (or double-tab) in M-x command entry pane show possibilities")
(todo "the choice of buffer for C-x b should be smarter")
(todo "weird flicker in creating/removing views (C-x 1, 2 or 3)")
(todo "delete region on really large regions is prohibitively slow")
(todo "10 Proposal for new buffer/pane relations
There is a proposal on the table to make the way Climacs manages buffers and panes
incompatible with that of Emacs, and in the process thus cleaning up 30 years of baggage.
The proposal is to no longer allow buffers without panes. Instead, a buffer will always
be associated with at least one pane, though that pane could be adopted or disowned to
make it visible or invisible. The advantage of this organization is that a buffer will no
longer contain a point. Also, panes can contain other things that buffers such as buffer
lists, debugger applications, etc.
For this to work, we need to define how the effect of certain commands related to buffers
and windows will be altered. The proposal is:
C-x 2 creates an additional pane with its own point and that shares the buffer of the
current pane. It also adopts the new pane by the same mechanism used now (creating a
vbox pane containing the two. C-x 3 is similar except that it uses a hbox instead.
C-x 0 does not destroy the pane, but just disowns it (by replacing the rack it is in by
the pane itself).
C-x 1 does the equivalent of C-x 0 on all other visible panes.
C-x k kills the current pane. If that happens to be the last pane containing a particular
buffer, then the buffer is lost as well.
C-x b replaces the current pane by some arbitrary pane displaying the buffer that has
been requested (or creates a new buffer and a new pane if the buffer requested does not
exist?).")
