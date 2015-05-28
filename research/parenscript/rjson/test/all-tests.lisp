(in-package :rjson-tests)
(in-suite :rjson-tests)

(defclass person ()
  ((first-name :initarg :first-name :initform nil)
   (last-name :initarg :last-name :initform nil)
   (email :initarg :email :initform nil)
   (best-friend :initarg :fiend :initform nil :accessor best-friend))
  (:documentation "Our greatest test class."))

(stefil:deftest nil-is-null ()
  (stefil:is (equal (rjson:encode-rjson-to-string nil)
		    "{\"header\":{\"allocs\":[],\"inits\":[]},\"content\":null}")))

(stefil:deftest is-five ()
  (stefil:is (equal (rjson:encode-rjson-to-string 5)
		    "{\"header\":{\"allocs\":[],\"inits\":[]},\"content\":5}")))

(stefil:deftest is-five-str ()
  (stefil:is (equal (rjson:encode-rjson-to-string "five")
		    "{\"header\":{\"allocs\":[],\"inits\":[]},\"content\":\"five\"}")))