;;; system aporopos
;;; ============================================================================
;;; the following programs are used to find CL codebases that might not be in
;;; quicklisp, and resolving all (including those in quicklisp) to one of their
;;; homepages so that you can (eventually) get a quick description of a ASDF
;;; system in a single keystroke, even if the author has not included a homepage
;;; in his description.

(in-package mm)

(declaim (inline strip remove-tags))
  
(defun system-apropos (search-word &key (cliki t) (github t) (quicklisp t))
  "Search for CL projects with `search-word' in Quicklisp Cliki and GitHub.
`search-word' must be a string, number or symbol (symbol will be automatically
converted into downcase-string).

Keys:

 :cliki       search cliki
 :github      search github
 :quicklisp   search quicklisp

In case `search-word' contains #\\Space, Quicklisp-search is OR-search, whereas
Cliki-search, GitHub- are AND-search. Consider, 

`(quicksearch \"foo bar\")'

This will search quicklisp for \"foo\" or \"bar\", and Web resources for \"foo\"
and \"bar\"
"
  (let+ ((word (write-to-string search-word :case :downcase :escape nil)))
    (remove nil (append (when cliki  (search-cliki word))
			(when github (search-github word))
			(when quicklisp (iter (for w in (ppcre:split " " word))
					      (appending (search-quicklisp w))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quicklisp

(defun installed-p (sys)
  (handler-case (not (ql-dist:check-local-archive-file
		      (ql-dist:release sys)))
    (ql-dist:missing-local-archive () nil)))

(defun get-url (sys)
  ;; Memo: 2013-08-08 by tkych
  ;; Don't exchange the following conditional-clause
  ;; (ql:where-is-system (ql-dist:name sys)).
  ;; ql:where-is-system contains asdf:find-system, and
  ;; (asdf:find-system "asdf-encodings") might reload asdf.
  ;; Added: 2013-08-12 by tkych
  ;; This is not a problem if asdf is the latest version.
  (aif (nth-value 2 (asdf:locate-system (ql-dist:name sys)))
      (cat (directory-namestring it)
	   #.(format nil "~%      ")
	   (slot-value (ql-dist:release sys)
		       'ql-dist::archive-url))
    (slot-value (ql-dist:release sys)
		'ql-dist::archive-url)))

(defun search-quicklisp (word)
  (iter (for sys in (ql-dist:provided-systems t))
	(when (or (search word (ql-dist:name sys))
		  (search word (ql-dist:name (ql-dist:release sys))))
	  (collect (list :tzg-url  (get-url sys)
			 :url      (format nil "http://quickdocs.org/~A/" 
					   (ql-dist:name (ql-dist:release sys)))
			 :version  (ql-dist:version (ql-dist:dist sys))
			 :name     (ql-dist:name sys)
			 :source   "Quicklisp"
			 :installed(installed-p sys))))))

(defun strip (string)
  (string-trim '(#\Space #\Return #\Newline) string))

(defun remove-tags (string)
  (ppcre:regex-replace-all "(<.+?>)" string ""))

(defun request (query-url)
  (drakma:http-request query-url :preserve-uri t :external-format-out :utf-8))

(defun parse-cliki-repos (response)
  (let+ ((results (ppcre:scan-to-strings
                   "(?s)<ol start=.+?>(.+?)</ol>" response))
         (repos (ppcre:all-matches-as-strings
                 "(?s)<li>(.+?)</li>" results)))
    (when repos
      (iter (for repo :in repos)
            (ppcre:register-groups-bind (url title description)
                ("(?s)<li><a href=\"(.+?)\" class=\"internal\">(.+?)</a>\\s?<br\\s?/?>(.+?)</li>"
                 repo)
              (collect (list title url
                             (let+ ((desc (strip (remove-tags description))))
                               (when (string/= "" desc) desc)))))))))

(defun parse-cliki-next-page-url (response)
  (let+ ((urls nil)
	 (paginator (ppcre:scan-to-strings
		     "(?s)<div id=\"paginator\">(.+?)</div>" response)))
    (ppcre:do-register-groups (query)
        ("<a href=\"\\\?query=(.+?)\">" paginator)
      (push (format nil "http://www.cliki.net/site/search?query=~A" query)
	     urls))
    (let+ ((rest-urls (nreverse (rest urls)))) ;first and last is the same.
      (subseq rest-urls
              0 (min 40 (length rest-urls))))))

(defun search-cliki (word-or-next-page-url &optional intermediate-results)
  (let+ ((query-url             "http://www.cliki.net/site/search?query=~A")
	 (word-or-next-page-url (if intermediate-results word-or-next-page-url
				    (nsubstitute #\+ #\Space word-or-next-page-url :test #'char=)))
	 (response              (request (if intermediate-results word-or-next-page-url
					     (format nil query-url word-or-next-page-url))))
	 (repos                 (append intermediate-results (parse-cliki-repos response)))
	 (next-page             (parse-cliki-next-page-url response)))
    ;; NOTE 2014-07-11T12:42:55-07:00 Gabriel Laddel
    ;; Cliki returns the first page as the next url, which can loop forever if
    ;; you're not careful. =0 happens to be the last two characters of
    ;; `next-page' when this occurs
    (if (not (or (null next-page) (equal "=0" (subseq (car next-page) (- (length (car next-page)) 2)))))
	(search-cliki (car next-page) repos)
	(iter (for (name relative-url description) in repos)
	      (collect (list :url (cat "http://www.cliki.net" relative-url)
			     :name name
			     :description description
			     :source "Cliki"))))))

(defun response-string (response)
  (etypecase response
    (STRING response)
    (VECTOR (flexi-streams:octets-to-string response :external-format :utf-8))))

(defun parse-github-repos (response)
  (iter (for repo in (gethash "repositories" (yason:parse response)))
  	(unless (gethash "fork" repo)	; only find master
  	  (collect (list :name        (gethash "name" repo)
  			 :url         (gethash "url" repo)
  			 :description (gethash "description" repo)
			 :source "Github")))))

(defun search-github (word)
  (->> word
    (do-urlencode:urlencode)
    (format nil "https://api.github.com/legacy/repos/search/~A?language=Common%20Lisp")
    (request)
    (response-string)
    (parse-github-repos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bitbucket
;;; 
;;; Broken. `
;;; (qs:? "tcod" :u)` doesn't turn anything up, and that's the first result for
;;; (google "bitbucket common lisp"). Did their API change?

;; (defun parse-bitbucket-repos (response)
;;   (let* ((results (ppcre:scan-to-strings
;;                    "(?s)<section id=\"repo-list\">(.+?)</section>"
;;                    response))
;;          (repos (ppcre:all-matches-as-strings
;;                  "(?s)<article class=\"repo-summary\">(.+?)</article>"
;;                  results)))
;;     (when repos
;;       (iter (for repo in repos)
;;             (ppcre:register-groups-bind (url title)
;;                 ("(?s)<a class=\"repo-link\" href=\"(.+?)\">.+? / (.+?)</a>"
;;                  repo)
;;               (collect (list title url
;;                              (ppcre:register-groups-bind (description)
;;                                  ("(?s)<p>(.+?)</p>" repo)
;;                                (strip (remove-tags description))))))))))

;; (defun parse-bitbucket-next-page-url (response)
;;   (let ((urls nil)
;;         (paginator (ppcre:scan-to-strings
;;                     "(?s)<ol class=\"paginator\">(.+?)</ol>" response)))
;;     (ppcre:do-register-groups (next-url)
;;         ("<a href=\"(.+?)\">" paginator)
;;       (push next-url urls))
;;     (let ((rest-urls (nreverse (rest urls)))) ;first and last is the same.
;;       (subseq rest-urls 0 (min 40 (length rest-urls))))))

;; (defun search-bitbucket (word-or-next-page-url &optional intermediate-results)
;;   (let+ ((query-url             "https://bitbucket.org/repo/all/relevance?name=~A&language=common+lisp")
;; 	 (word-or-next-page-url (nsubstitute #\+ #\Space word-or-next-page-url :test #'char=))
;; 	 (response              (request (if intermediate-results word-or-next-page-url
;; 					     (format nil query-url word-or-next-page-url))))
;; 	 (repos                 (parse-bitbucket-repos response)))
;;     (aif (parse-bitbucket-next-page-url response)
;; 	(search-bitbucket it repos) repos)))
