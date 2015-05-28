(in-package :paren-test)

(defun run-paren-system (name)
  (with-open-file (stream "/tmp/paren.js" :direction :output :if-exists :supersede)
    (paren-files:compile-script-system name :output-stream stream))
  (trivial-shell:shell-command
   "js -f /tmp/paren.js"))