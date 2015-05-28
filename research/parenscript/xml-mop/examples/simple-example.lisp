(in-package :xml-mop)

(defclass abstract-root-response ()
  ((items :accessor response-items :initform nil :initarg :items
	  :subelement (:element-type items :multiple t)))
;	  :subelement ("Items" :case-sensitive t :element-type items :multiple nil)))
  (:metaclass element-class)
  (:allowed-elements price-element))

(defclass item-search-response (abstract-root-response)
  ()
  (:metaclass element-class)
  (:tags ("ItemSearchResponse" :primary t)))

(defclass items ()
  ()
  (:tags ("Items" :primary t))
  (:metaclass element-class))

(defclass price-element ()
  ((currency-code :accessor price-currency-code
		  :initarg :currency-code
;		  :attribute ("CurrencyCode" :primary t :case-sensitive t)
		  ))
  (:metaclass element-class)
  (:tags ("Price" :primary t :case-sensitive nil)
	 ("PriceElement" :case-sensitive nil)))

(with-open-file (stream "examples/simple-aws.xml")
  (parse-xml-stream stream (list (find-class 'item-search-response))))

;(defclass item-element ()
;  ((asin :accessor item-asin :initform "" :initarg :asin)
;   (detail-page-url :accessor item-detail-page-url
;		    :initform nil
;		    :subelement "DetailPageURL"))
;  (:metaclass element-class)


;(defelement price-element ()
;  (amount :accessor price-amount :initform 0)
;  (currency-code :accessor price-currency-code
;		 :initform ""
;		 :attr-string "CurrencyCode"
;		 
;		 :attr-string ("Code" t) ; case-sensitive attribute
;		 :attribute-function ()) ; there should be a way to specify an arbitrary function
;  
;)