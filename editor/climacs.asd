(defpackage :climacs.system
  (:use :cl :asdf))

(in-package :climacs.system)

(defsystem :climacs
  :depends-on (:mcclim :flexichain)
  :components
  ((:file "packages")
   (:file "text-syntax"         :depends-on ("packages"))
   (:file "lisp-syntax"         :depends-on ("core" "groups"))
   (:file "masamune-editing"    :depends-on ("lisp-syntax" "misc-commands"))
   (:file "c-syntax"            :depends-on ("core"))
   (:file "c-syntax-commands"   :depends-on ("c-syntax" "misc-commands"))
   (:file "typeout"             :depends-on ("packages"))
   (:file "gui"                 :depends-on ("packages" "typeout"))
   (:file "core"                :depends-on ("gui"))
   (:file "io"                  :depends-on ("packages" "gui"))
   (:file "groups"              :depends-on ("core"))
   (:file "climacs"             :depends-on ("gui" "core"))
   (:file "developer-commands"  :depends-on ("core"))
   (:file "file-commands"       :depends-on ("gui" "core" "io"))
   (:file "misc-commands"       :depends-on ("gui" "core" "groups"))
   (:file "search-commands"     :depends-on ("gui" "core" "groups"))
   (:file "window-commands"     :depends-on ("gui" "core"))))
