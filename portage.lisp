(in-package #:mm)

;;; Building a LiveCD, DVD, USB
;;; ============================================================================
;;; https://forums.gentoo.org/viewtopic-t-57754.html
;;; 
;;; Building Masamune 
;;; ============================================================================
;;;
;;; Distinct phases of building Masamune
;;; 1. Linux kernel
;;; 2. Required ALGOL systems
;;; 3. Browser
;;; 4. Lisp
;;; 
;;; Kernel
;;; ============================================================================
;;; /usr/src/linux-debian-sources/
;;;
;;; lspci
;;; lsusb
;;; lsmod
;;; uname -a
;;; 
;;; ls /dev/snd/con* << lists sound cards
;;; 
;;; http://unix.stackexchange.com/questions/115620/configuring-compiling-and-installing-a-custom-linux-kernel
;;; ^ start here - I read through 5 pages of google results - these links are all that is needed.
;;; http://edoceo.com/howto/kernel-modules
;;; https://www.kernel.org/
;;; https://wiki.gentoo.org/wiki/Kernel/Gentoo_Kernel_Configuration_Guide
;;; http://0xax.gitbooks.io/linux-insides/content/Booting/index.html
;;; 
;;; /usr/src/linux/.config << nothing there 
;;; 
;;; some kernel configuration files
;;; http://kernel.ubuntu.com/~kernel-ppa/configs/lucid/
;;; 
;;; /usr/src/linux-debian-sources-3.16.2/Documentation:
;;; 
;;; /usr/lib64/systemd <<< wtf?! why does this exist? 
;;; 
;;; cd /usr/src/linux..../ && make menuconfig
;;; 
;;; make localmodconfig - apparently this configures the kernel based on the hardware you've got 
;;;
;;; Required ALGOL
;;; ============================================================================
;;; /usr/lib64/portage/ contains ~70k LoC (mostly python). Although I have very
;;; simple requirements, this is somewhat more than what I can deal with at the
;;; moment.
;;;
;;; I'm treating the packages I can install as the canonical implementation.
;;;
;;; Relevent files, directories
;;; 
;;; /usr/lib64/
;;; /var/lib/portage/world
;;; /etc/portage/package.keywords
;;; /etc/portage/package.use
;;; /etc/make.conf
;;; /var/db/pkg/ << contains a bunch of information about portage systems
;;; /var/tmp/ << can I just remove all of this crap?
;;; 
;;; equery y maxima
;;; emerge --info
;;; emerge -s maxima
;;; equery files imaxima
;;;
;;; list all installed packages
;;; - equery list "*"
;;; - cd /var/db/pkg/ && ls -d */*
;;; - list all packages the user has installed cat /var/lib/portage/world
;;;
;;; Any documentation that a program might have (other than man pages) is stored in /usr/share/doc/gentoolkit-[version]/[program-name]/. 
;;; 
;;; equery depgraph imaxima << checkout equery --helpe
;;; 
;;; Equery reimplementaiton
;;; 
;;; __init__.py is uesless
;;; 
;;; USE flags are portage's way of organizing compile time options
;;; 
;;; Misc
;;; ============================================================================
;;; 
;;; https://www.gentoo.org/support/documentation/
;;; https://packages.gentoo.org/
;;; https://wiki.gentoo.org/wiki/Gentoolkit
;;;

;; (with-open-file (stream #P"/usr/portage/app-accessibility/SphinxTrain/metadata.xml" :direction :input)
;;   (s-xml:parse-xml stream))

(defun end-function-parse-stack? (parse-stack)
  (labels ((opposite (char) (cond ((char= char #\}) #\{)
				  ((char= char #\{) #\}))))
    (loop with seen = nil
	  for e in parse-stack
	  do (cond ((null e)
		    (setf seen (cons e seen)))
		   ((and seen (char= e (opposite (car seen))))
		    (setf seen (rest seen)))
		   (t (setf seen (cons e seen))))
	  finally (return (null seen)))))

(defun parse-sh-function (string)
  (format t "~& ")
  (multiple-value-bind (function-init-start function-init-end)
      (scan "[a-zA-Z0-9_]*\\(\\) {" string)
    (loop with parse-stack = '(#\{)
    	  for char across (drop function-init-end string)
    	  for idx = 0 then (1+ idx)
    	  do (cond ((end-function-parse-stack? parse-stack)
		    (return (values (subseq string function-init-start (+ 15 (+ idx function-init-start)))
				    (+ 15 (+ idx function-init-start)))))
		   
		   ((or (char= #\{ char) (char= #\} char))
		    (setq parse-stack (append parse-stack (list char)))))
	  finally (return (values (when (end-function-parse-stack? parse-stack)
				    (subseq string function-init-start))
				  (length string))))))

;; (defun parse-ebuild (pathname)
;;   (assert (probe-file pathname))
;;   (let* ((lines (remove-if #'emptyp (mm::split "\\n" (slurp-file pathname))))
;; 	 (comments (filter (lambda (s) (and (not (emptyp s)) (char= #\# (aref s 0)))) lines))
;; 	 (env-variables (filter (lambda (s) (regex-matches "^[a-zA-z0-9]*=[a-zA-z0-9]*" s)) lines))
;; 	 (inherit (rest (split #\space (string= "inherit" (take 7 line)))))
;; 	 (functions ))
;;     ))

;; (with-open-file (stream #P"/usr/portage/app-accessibility/edbrowse/metadata.xml"
;; 			:direction :input)
;;   (s-xml:parse-xml stream))
