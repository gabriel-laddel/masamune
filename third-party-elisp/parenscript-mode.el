;;; parenscript-mode -*- lexical-binding: t -*-
;;; 
;; These commands generate JavaScript from the Parenscript code and display it
;; but don't send it to the browser for evaluation:

;;    - parenscript-expand-sexp
;;    - parenscript-expand-last-expression
;;    - parenscript-expand-defun
;;    - parenscript-expand-region
;;    - parenscript-expand-buffer
;;    - parenscript-expand-dwim

;; From within an expansion buffer you can press e to send the JavaScript to
;; the browser, w to copy it to the kill ring, s to save it to a file (you'll
;; be prompted for the destination) or q to dismiss the buffer. The copy
;; command, w, acts on the region if it's active or the entire buffer
;; otherwise.

;; Additionally, you can use M-x parenscript-compile-buffer-to-file to expand the
;; current buffer and save the generated code directly to a file.

;; *** Code evaluation commands

;; These commands first compile the Parenscript code to JavaScript and then
;; immediately send to it the browser to be evaluated:

;;    - parenscript-eval-sexp
;;    - parenscript-eval-last-expression
;;    - parenscript-eval-defun
;;    - parenscript-eval-region
;;    - parenscript-eval-buffer
;;    - parenscript-eval-dwim

;; ** Key bindings

;; The traditional set of code evaluation key bindings is a poor fit for
;; Parenscript,since they would shadow SLIME's equivalent commands and that's
;; probably not what you want. That leaves us without a clear convention to
;; follow, so by default we don't establish any key bindings at all. However,
;; the function parenscript-add-keys-with-prefix will add two-key key bindings for
;; all commands behind a prefix of your choice.

;; For example: (parenscript-add-keys-with-prefix "C-c C-e"). The key sequence for
;; parenscript-eval-region is "e r", so it's now bound to "C-c" "C-e er"

;; The full list of key bindings parenscript-add-keys-with-prefix will establish
;; is:

;;    - "e RET" -- parenscript-eval-sexp
;;    - "e e" -- parenscript-eval-last-expression
;;    - "e d" -- parenscript-eval-defun
;;    - "e r" -- parenscript-eval-region
;;    - "e b" -- parenscript-eval-buffer
;;    - "e SPC" -- parenscript-eval-dwim
;;    - "x RET" -- parenscript-expand-sexp
;;    - "x e" -- parenscript-expand-last-expression
;;    - "x d" -- parenscript-expand-defun
;;    - "x r" -- parenscript-expand-region
;;    - "x b" -- parenscript-expand-buffer
;;    - "x SPC" -- parenscript-expand-dwim

;; Evaluation commands begin with an "e", expansion commands with "x". The
;; second letter is generally mnemonic but not always. The -sexp commands use
;; RET in correspondence to slime-expand-1, and the -dwim commands use the
;; space bar because it's easy and comfortable to hit.

;; Please consider these keys provisional, and let me know if you have any
;; ideas for improving the arrangement.

;; If you really want to shadow SLIME's key bindings in buffers where
;; parenscript-mode is active you could do something like this:

(defvar parenscript-mode-map (make-sparse-keymap)
  "Keymap for parenscript-mode.
This keymap is initialized empty. You can optionally use
`parenscript-add-keys-with-prefix' to add bindings for all commands
on two-key sequences behind a prefix of your choice.")

(defvar parenscript-expansion-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'parenscript-send-expanded-code)
    (define-key map (kbd "w") 'parenscript-kill-ring-save-dwim)
    (define-key map (kbd "s") 'write-file)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for `parenscript-expansion-mode' buffers.")

(defvar parenscript-scratch-mode-map (make-sparse-keymap)
  "Keymap for the *parenscript-scratch* buffer.")

(defvar parenscript-expansion-major-mode 'javascript-mode
  "The major mode to enable in expansion buffers.
Note that are currently serious problems when using js2-mode in
expansion buffers. Avoid it for the time being.")

;;;; Code expansion

(defun parenscript-wrap-in-ps-form (string)
  "Return Parenscript STRING wrapped in a PS:PS form."
  (format "(ps:ps %s)" string))

(defun parenscript-call-with-expansion (fn string)
  "Call FN on the result of expanding Parenscript STRING.

Note that FN will be called asynchronously and its return value
discarded; it must work through side effects alone.

See also `parenscript-with-expansion'."
  (let ((string (parenscript-wrap-in-ps-form string)))
    (slime-eval-async `(swank:eval-and-grab-output ,string)
      #'(lambda (result)
          (funcall fn (read (cadr result))))
      (slime-current-package))))

(defmacro parenscript-with-expansion (name-and-string &rest body)
  "Expand a Parenscript string and execute BODY.

NAME-AND-STRING should be a two-item list, with the second item
the string to be expanded and the first item the name to which to
bind the result of the expansion. Note that BODY will be executed
asynchronously and its return value discarded; it must work
through side effects alone.

See also `parenscript-call-with-expansion'."
  (let ((name (car name-and-string))
        (string (cadr name-and-string))
        (rv (make-symbol "rv")))
    `(slime-eval-async
         `(swank:eval-and-grab-output ,(parenscript-wrap-in-ps-form ,string))
       #'(lambda (,rv)
           (let ((,name (read (cadr ,rv))))
             ,@body))
       (slime-current-package))))

(put 'parenscript-with-expansion 'lisp-indent-function 1)

(defun parenscript-expand (string)
  "Display the JavaScript generated from Parenscript STRING.

The resulting JavaScript is displayed in a temporary buffer. The
buffer's major mode is determined by the variable
`parenscript-expansion-major-mode' (`javascript-mode' by default).
`parenscript-expansion-mode' is enabled as an additional minor mode."
  (parenscript-with-expansion (code string)
    (slime-with-popup-buffer ("*Parenscript generated JavaScript*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert code)
      (funcall parenscript-expansion-major-mode)
      (parenscript-expansion-mode 1)
      (font-lock-fontify-buffer)
      (goto-char (point-min))
      (setq buffer-read-only t)
      (pop-to-buffer (current-buffer)))))

(defun parenscript-compile-buffer-to-file ()
  "Compile the current buffer and write the result.
Prompts for the destination. If a file already exists at the
destination it's overwritten."
  (interactive)
  (when (and (buffer-modified-p)
             (y-or-n-p "Save buffer? "))
    (save-buffer))
  (let* ((this buffer-file-name)
         (dir (and this (file-name-directory this)))
         (initial (and this (concat (file-name-base this) ".js")))
         (destination (read-file-name "Destination: " dir nil nil initial nil))
         (string (buffer-substring-no-properties (point-min) (point-max))))
    (parenscript-with-expansion (code string)
      (with-temp-buffer
        (erase-buffer)
        (insert code)
        (write-region 1 (point-max) destination)))))

(defun parenscript-expand-sexp ()
  "Display the expansion of the form at point."
  (interactive)
  (parenscript-expand (slime-sexp-at-point)))

(defun parenscript-expand-last-expression ()
  "Display the expansion of the expression preceding point."
  (interactive)
  (parenscript-expand (slime-last-expression)))

(defun parenscript-expand-defun ()
  "Display the expansion of the current toplevel form."
  (interactive)
  (parenscript-expand (slime-defun-at-point)))

(defun parenscript-expand-region (beg end)
  "Display the expansion of the currently active region."
  (interactive "r")
  (parenscript-expand (buffer-substring-no-properties beg end)))

(defun parenscript-expand-buffer ()
  "Display the expansion of the current buffer."
  (interactive)
  (parenscript-expand-region (point-min) (point-max)))

(defun parenscript-expand-dwim ()
  "Display the expansion of the active region or toplevel form.
If the region is active this is equivalent to invoking
`parenscript-expand-region', otherwise it's equivalent to
`parenscript-expand-defun'."
  (interactive)
  (if (region-active-p)
      (parenscript-expand-region (region-beginning) (region-end))
    (parenscript-expand-defun)))

(defun parenscript-eval (string)
  ;; TODO 2015-01-22T02:00:40+00:00 Gabriel Laddel
  "Compile Parenscript STRING and evaluate it in the browser.

The code is first compiled to JavaScript in the CL image and then
sent to the browser via `skewer-eval'."
  (parenscript-with-expansion (code string)
    (cl-message "tried to eval ~S" code)))

(defun parenscript-eval-sexp ()
  "Evaluate the expression at point as Parenscript."
  (interactive)
  (parenscript-eval (slime-sexp-at-point)))

(defun parenscript-eval-last-expression ()
  "Evaluate the expression preceding point as Parenscript."
  (interactive)
  (parenscript-eval (slime-last-expression)))

(defun parenscript-eval-defun ()
  "Evaluate the current toplevel form as Parenscript."
  (interactive)
  (parenscript-eval (slime-defun-at-point)))

(defun parenscript-eval-region (beg end)
  "Evaluate the currently active region as Parenscript."
  (interactive "r")
  (parenscript-eval (buffer-substring-no-properties beg end)))

(defun parenscript-eval-buffer ()
  "Evaluate the current buffer as Parenscript."
  (interactive)
  (parenscript-eval-region (point-min) (point-max)))

(defun parenscript-eval-dwim ()
  "Evaluate the active region or toplevel form.
If the region is active this is equivalent to invoking
`parenscript-eval-region', otherwise it's equivalent to
`parenscript-eval-defun'."
  (if (region-active-p)
      (parenscript-eval-region (region-beginning) (region-end))
    (parenscript-eval-defun)))

;;;; Expansion buffer commands

(defun parenscript-send-expanded-code ()
  "Send the expanded code to the browser.
For use from `parenscript-expansion-mode' buffers."
  (interactive)
  (skewer-eval
   (buffer-substring-no-properties (point-min) (point-max))
   #'skewer-post-minibuffer))

(defun parenscript-add-keys-with-prefix (p)
  "Add keybindings for `parenscript-mode' commands behind prefix P."
  (let ((map parenscript-mode-map)
        (prefix #'(lambda (keys) (read-kbd-macro (concat p " " keys)))))
    ;; Evaluation commands
    (define-key map (funcall prefix "e C-m") 'parenscript-eval-sexp)
    (define-key map (funcall prefix "e e")   'parenscript-eval-last-expression)
    (define-key map (funcall prefix "e d")   'parenscript-eval-defun)
    (define-key map (funcall prefix "e r")   'parenscript-eval-region)
    (define-key map (funcall prefix "e b")   'parenscript-eval-buffer)
    (define-key map (funcall prefix "e SPC") 'parenscript-eval-dwim)
    ;; Expansion commands
    (define-key map (funcall prefix "x C-m") 'parenscript-expand-sexp)
    (define-key map (funcall prefix "x e")   'parenscript-expand-last-expression)
    (define-key map (funcall prefix "x d")   'parenscript-expand-defun)
    (define-key map (funcall prefix "x r")   'parenscript-expand-region)
    (define-key map (funcall prefix "x b")   'parenscript-expand-buffer)
    (define-key map (funcall prefix "x SPC") 'parenscript-expand-dwim)))

(defun parenscript-scratch-buffer ()
  "Return the scratch buffer, creating it if necessary."
  (let ((name "*parenscript-scratch*"))
    (or (get-buffer name)
        (with-current-buffer (get-buffer-create name)
          (lisp-mode)
          (slime-mode t)
          (parenscript-mode 1)
          (parenscript-scratch-mode 1)
          (current-buffer)))))

(defun parenscript-switch-to-scratch-buffer ()
  "Jump to the *parenscript-scratch* buffer."
  (set-buffer (parenscript-scratch-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t)))

(defun parenscript-scratch ()
  "Jump to the *parenscript-scratch* buffer."
  (interactive)
  (parenscript-switch-to-scratch-buffer))

(defun parenscript-mode-buffer-p (buffer)
  "Return t if `parenscript-mode' is active in BUFFER."
  (with-current-buffer buffer
    (bound-and-true-p parenscript-mode)))

(defun parenscript-recently-visited-parenscript-buffer ()
  "Return the most recently visited `parenscript-mode' buffer.
Only considers buffers that are not already visible."
  (or (-first #'(lambda (b) (and (parenscript-mode-buffer-p b)
                                 (null (get-buffer-window b 'visible))))
              (buffer-list))
      (error "Can't find unshown buffer in parenscript-mode")))

(defun parenscript-add-slime-selector-methods ()
  "Add methods to `slime-selector' for `parenscript-mode' buffers.
Allows access to the most recently visited buffer with
parenscript-mode active via \"p\" and to the *parenscript-scratch* buffer
via \"P\"."
  (interactive)
  (def-slime-selector-method ?p
    "most recently visited buffer using parenscript-mode."
    (parenscript-recently-visited-parenscript-buffer))
  (def-slime-selector-method ?P
    "*parenscript-scratch* buffer."
    (parenscript-scratch-buffer)))

;;;; The minor modes

;;;###autoload
(define-minor-mode parenscript-mode
  "Minor mode for interactively evaluating Parenscript forms."
  :lighter " tri"
  :keymap  parenscript-mode-map)

;;;###autoload
(define-minor-mode parenscript-scratch-mode
  "Mode for parenscript-mode scratch buffer."
  :lighter nil
  :keymap  parenscript-scratch-mode-map)

(define-minor-mode parenscript-expansion-mode
  "Minor mode for displaying the code generated by Parenscript."
  :lighter nil
  :keymap  parenscript-expansion-mode-map)
