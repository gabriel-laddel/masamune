;;; persistent-soft-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (persistent-soft-store persistent-soft-flush persistent-soft-fetch
;;;;;;  persistent-soft-exists-p persistent-soft-location-destroy
;;;;;;  persistent-soft-location-readable persistent-soft) "persistent-soft"
;;;;;;  "persistent-soft.el" (21280 15907 804642 73000))
;;; Generated autoloads from persistent-soft.el

(let ((loads (get 'persistent-soft 'custom-loads))) (if (member '"persistent-soft" loads) nil (put 'persistent-soft 'custom-loads (cons '"persistent-soft" loads))))

(autoload 'persistent-soft-location-readable "persistent-soft" "\
Return non-nil if LOCATION is a readable persistent-soft data store.

\(fn LOCATION)" nil nil)

(autoload 'persistent-soft-location-destroy "persistent-soft" "\
Destroy LOCATION (a persistent-soft data store).

Returns non-nil on confirmed success.

\(fn LOCATION)" nil nil)

(autoload 'persistent-soft-exists-p "persistent-soft" "\
Return t if SYMBOL exists in the LOCATION persistent data store.

This is a noop unless LOCATION is a string and pcache is loaded.

Returns nil on failure, without throwing an error.

\(fn SYMBOL LOCATION)" nil nil)

(autoload 'persistent-soft-fetch "persistent-soft" "\
Return the value for SYMBOL in the LOCATION persistent data store.

This is a noop unless LOCATION is a string and pcache is loaded.

Returns nil on failure, without throwing an error.

\(fn SYMBOL LOCATION)" nil nil)

(autoload 'persistent-soft-flush "persistent-soft" "\
Flush data for the LOCATION data store to disk.

\(fn LOCATION)" nil nil)

(autoload 'persistent-soft-store "persistent-soft" "\
Under SYMBOL, store VALUE in the LOCATION persistent data store.

This is a noop unless LOCATION is a string and pcache is loaded.

Optional EXPIRATION sets an expiry time in seconds.

Returns a true value if storage was successful.  Returns nil
on failure, without throwing an error.

\(fn SYMBOL VALUE LOCATION &optional EXPIRATION)" nil nil)

;;;***

;;;### (autoloads nil nil ("persistent-soft-pkg.el") (21280 15907
;;;;;;  883065 604000))

;;;***

(provide 'persistent-soft-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; persistent-soft-autoloads.el ends here
