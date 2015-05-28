(defpackage :my-amazon-ecs
  (:nicknames :ecs2)
  (:use :cl))

(in-package :my-amazon-ecs)
(import 'xml-mop::element-class)
(import 'xml-mop::element-text)

;;;; Class definitions for AWS responses ;;;;

;;; basic element classes that get reused in a lot of places

(defclass numerical-text-element ()
  ()
  (:metaclass element-class)
  (:documentation "An element that has text that reduces to a number"))
(defmethod xml-mop:element-value ((element numerical-text-element))
  (parse-number:parse-number (xml-mop:element-text element)))

(defclass simple-text-element () () (:metaclass element-class))
(defmethod xml-mop:element-value ((element simple-text-element))
  (xml-mop:element-text element))

(defclass date-element () () (:metaclass element-class))
(defmethod xml-mop:element-value ((element date-element))
  (net.telent.date:parse-time (xml-mop:element-text element)))
(defclass yes-no-element (simple-text-element) () (:metaclass element-class))

(defclass numerical-measurement-element (numerical-text-element)
  ((units :attribute "Units" :accessor distance-units :initform ""))
  (:metaclass element-class))

(defclass digital-distance-element (numerical-measurement-element) () (:metaclass element-class))
(defclass distance-element (numerical-measurement-element) () (:metaclass element-class))
(defclass weight-element (numerical-measurement-element) () (:metaclass element-class))

(defclass key-value-element ()
  ((arg-name :accessor argument-name :initform "" :initarg :name :attribute ("Name"))
   (arg-value :accessor argument-name :initform "" :initarg :name :attribute ("Value")))
  (:metaclass element-class)
  (:documentation "Arguments element in Amazon ECS response"))

(defclass price-element ()
  ((amount :accessor price-amount :initform 0 :initarg :amount
	   :subelement (numerical-text-element :alias "Amount"))
   (currency-code :accessor price-currency-code :initform 0 :initarg :currency-code
		  :subelement (simple-text-element :alias "CurrencyCode"))
   (formatted-price :accessor price-formatted :initform 0 :initarg :formatted-price
		   :subelement (simple-text-element :alias "FormattedPrice")))
  (:metaclass element-class)
  (:documentation "Parent of all elements that contain price information"))

(defclass image-element ()
  ((url :subelement (simple-text-element :alias "URL") :accessor image-url :initform "")
   (width :subelement (digital-distance-element :alias "Width") :accessor image-width :initform "")
   (height :subelement (digital-distance-element :alias "Height") :accessor image-height :initform ""))
  (:metaclass element-class))

;;; Root AWS responses

(defclass abstract-root-response ()
  ((xmlns :accessor response-xmlns :initform "" :initarg :xmlns
	  :attribute ("xmlns"))
   (operation-request :accessor response-operation-request :initform nil
		      :subelement (operation-request :alias "OperationRequest"))
   (items :accessor response-items :initform nil :initarg :items
	  :subelement (items :alias "Items")))
  (:metaclass element-class)
  (:documentation "root of most all operations response"))

(defclass item-search-response (abstract-root-response)
  ()
  (:metaclass element-class)
  (:tags ("ItemSearchResponse"))
  (:documentation "root of item search response"))

(defclass item-lookup-response (abstract-root-response)
  ()
  (:metaclass element-class)
  (:tags ("ItemLookupResponse"))
  (:documentation "root of item lookup responses"))

(defclass operation-request ()
  ((http-headers :accessor operation-http-headers :initform ()
		 :subelement (http-headers :alias "HTTPHeaders"))
   (request-id :accessor operation-requestid :initform "" :initarg requestid
	       :subelement (simple-text-element :alias "RequestId"))
   (arguments :accessor operation-arguments :initform () :initarg arguments
	      :subelement (operation-arguments))
   (request-processing-time :accessor request-processing-time :initform ()
			    :subelement (numerical-text-element :alias "RequestProcessingTime")))
  (:metaclass element-class)
  (:tags ("OperationRequest"))
  (:documentation "OperationResponse element in Amazon ECS response"))

(defclass operation-arguments ()
  ((arguments :accessor operation-arguments :initform nil
	      :subelement (key-value-element :alias "Argument" :multiple t)))
  (:metaclass element-class)
  (:tags ("Arguments")))

(defclass http-headers ()
  ((headers :accessor headers :initform () :initarg :headers
	    :subelement (key-value-element :alias "Header" :multiple t)))
  (:metaclass element-class)
  (:tags ("HTTPHeaders"))
  (:documentation "HTTPHeaders element in Amazon ECS response"))

;;; Information pertaining to items

(defclass items ()
  ((request :accessor request :initform nil :initarg :request :subelement
	    (:element-type items-request-info :alias "Request"))
   (items :accessor items :initform () :initarg :items
	  :subelement (amazon-item :multiple t))
   (total-results :subelement (numerical-text-element :alias "TotalResults"))
   (total-pages :subelement (numerical-text-element :alias "TotalPages")))
  (:metaclass element-class)
  (:tags ("Items"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass items-request-info ()
  ((is-valid :accessor request-is-valid :initform nil
	    :subelement (yes-no-element :alias "IsValid"))
   (errors :accessor request-errors :initform nil)
   (item-search-request :accessor request-item-search-request :initform ()
			:subelement (item-search-request))
   (item-lookup-request :accessor item-lookup-request :initform ()
			:subelement (item-lookup-request)))
  (:metaclass element-class)
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass item-search-request ()
  ((keywords :accessor keywords :initform nil :initarg :keywords
	     :subelement (simple-text-element :alias "Keywords"))
   (merchantid :accessor merchant-id :initform nil :initarg :merchant-id
	       :subelement (simple-text-element :alias "MerchantId"))
   (response-group :accessor request-response-group :initform nil
	       :subelement (simple-text-element :alias "ResponseGroup"))
   (searchindex :accessor search-index :initform () :initarg :search-index
		:subelement (simple-text-element :alias "SearchIndex")))
  (:metaclass element-class)
  (:tags ("ItemSearchRequest"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass item-lookup-request ()
  ((item-id :accessor request-item-id :initform nil :initarg :item-id
	       :subelement (simple-text-element :alias "ItemId"))
   (response-groups :accessor response-groups :initform () :initarg :response-groups
	       :subelement (simple-text-element :alias "ResponseGroup" :multiple t))
   (merchant-id :accessor merchant-id :initform () :initarg :merchant-id
	       :subelement (simple-text-element :alias "MerchantId")))
  (:metaclass element-class)
  (:tags ("ItemLookupRequest"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass amazon-errors ()
  ((errors :accessor errors :initarg :errors :initform nil))
  (:metaclass element-class)
  (:tags ("Request")))

(defclass amazon-error ()
  ((code :accessor code :initarg :code :initform nil)
   (message :accessor message :initarg :message :initform ""))
  (:metaclass element-class)
  (:tags ("Request")))

;;; Amazon Item
(defgeneric title (item-like-thing)
  (:documentation "Gives the title of an item-like thing, e.g. ItemAttributes or Item"))
(defgeneric author (item-like-thing)
  (:documentation "Gives the author of an item-like thing, e.g. ItemAttributes or Item"))
(defgeneric isbn (item-like-thing)
  (:documentation "Gives the isbn of an item-like thing, e.g. ItemAttributes or Item"))

(defclass amazon-item ()
  ((item-attributes :accessor item-attributes :initform nil :initarg :item-attributes
		    :subelement (item-attributes))
   (asin :accessor item-asin :initform "" :initarg :asin :subelement (simple-text-element :alias "ASIN"))
   (detail-page-url :accessor item-detail-page-url :initform "" :initarg :detail-page-url
		    :subelement (simple-text-element :alias "DetailPageURL"))
   (sales-rank :subelement (numerical-text-element :alias "SalesRank"))
   (large-image :subelement (image-element :alias "LargeImage") :accessor item-large-image)
   (small-image :subelement (image-element :alias "SmallImage") :accessor item-small-image)
   (medium-image :subelement (image-element :alias "MediumImage") :accessor item-medium-image)
   (image-sets :subelement (image-set-collection :alias "ImageSets") :accessor item-image-sets)
   (alternate-versions :accessor alternate-versions :initform nil :initarg :alternate-versions)
   (offer-summary :accessor offer-summary :initform () :initarg :offer-summary
		  :subelement (offer-summary))
   (editorial-review :accessor offer-editorial-reviews :initform ()
		     :subelement (editorial-review-collection :alias "EditorialReviews"))
   (offers :accessor item-offers :initform ()
	   :subelement (offers :alias "Offers")))
  (:metaclass element-class)
  (:tags ("Item"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass image-set ()
  ((category :initform nil :accessor image-set-category :attribute "Category")
   (large-image :subelement (image-element :alias "LargeImage") :accessor image-set-large-image)
   (small-image :subelement (image-element :alias "SmallImage") :accessor image-set-small-image)
   (medium-image :subelement (image-element :alias "MediumImage") :accessor image-set-item-medium-image))
  (:metaclass element-class)
  (:documentation "Contains a set of images.  user contributed i guess?"))

(defclass image-set-collection ()
  ((image-sets :initform nil :accessor image-sets
	       :subelement (image-set :alias "ImageSet" :multiple t)))
  (:metaclass element-class)
  (:documentation "Contains a set of images.  user contributed i guess?"))
	     

(defclass item-price-description-mixin ()
  ((lowest-new-price :accessor lowest-new-price :initform nil
		     :subelement (price-element :alias "LowestNewPrice"))
   (lowest-collectible-price :accessor lowest-collectible-price :initform nil
			     :subelement (price-element :alias "LowestCollectiblePrice"))
   (lowest-used-price :accessor lowest-used-price :initform nil
		    :subelement (price-element :alias "LowestUsedPrice"))
   (lowest-refurbished-price :accessor lowest-refurbished-price :initform nil
			     :subelement (price-element :alias "LowestRefurbishedPrice")))
  (:metaclass element-class))

(defclass offer-summary (item-price-description-mixin)
  ((totalnew :accessor summary-total-new :initform nil :initarg :total-new
	     :subelement (numerical-text-element :alias "TotalNew"))
   (total-used :accessor lowest-used-price :initform nil :initarg :total-used
	       :subelement (numerical-text-element :alias "TotalUsed"))
   (total-collectible :accessor lowest-used-price :initform nil :initarg :total-collectible
		      :subelement (numerical-text-element :alias "TotalCollectible"))
   (total-refurbished :accessor lowest-used-price :initform nil :initarg :total-refurbished
		      :subelement (numerical-text-element :alias "TotalRefurbished")))
  (:metaclass element-class)
  (:tags ("OfferSummary"))
  (:documentation "Summary of offers for a particular item"))

(defclass creator ()
  ((role :attribute "Role"))
  (:metaclass element-class)
  (:tags "Creator")
  (:documentation "Summary of offers for a particular item"))


(defclass item-attributes (item-price-description-mixin)
  ((authors :accessor authors :initform nil :initarg :authors
	    :subelement (simple-text-element :alias "Author" :multiple t))
   (features :accessor item-features :initform nil :initarg :features
	    :subelement (simple-text-element :alias "Feature" :multiple t))
   (height :accessor item-height :initform nil :initarg :height
	   :subelement (distance-element :alias "Height"))
   (length :accessor item-length :initform nil :initarg :length
	   :subelement (distance-element :alias "Length"))
   (width :accessor item-width :initform nil
	  :subelement (distance-element :alias "Width"))
   (weight :accessor item-weight :initform nil
	   :subelement (weight-element :alias "Weight"))
   (package-dimensions :accessor item-package-dimensions :initform nil
		       :subelement (dimensional-element :alias "PackageDimensions"))
   (list-price :accessor item-list-price :initform nil
	       :subelement (price-element :alias "ListPrice"))
   (title :accessor item-title :initform nil
	  :subelement (simple-text-element :alias "Title"))
   (upc :accessor item-upc :initform nil
	:subelement (simple-text-element :alias "UPC"))
   (ean :accessor item-ean :initform nil
	:subelement (simple-text-element :alias "EAN"))
   (isbn :accessor item-isbn :initform nil
	 :subelement (simple-text-element :alias "ISBN"))
   (edition :accessor item-edition :initform nil
	    :subelement (simple-text-element :alias "Edition"))
   (publication-date :accessor item-publication-date :initform nil
		     :subelement (date-element :alias "PublicationDate"))
   (release-date :accessor item-publication-date :initform nil
		 :subelement (date-element :alias "ReleaseDate"))
   (publisher :accessor item-publisher :initform nil
	      :subelement (simple-text-element :alias "Publisher"))
   (studio :accessor item-studio :initform nil
	   :subelement (simple-text-element :alias "Studio"))
   (label :accessor item-label :initform nil
	    :subelement (simple-text-element :alias "Label"))
   (number-of-pages :accessor item-number-of-pages :initform nil
	    :subelement (numerical-text-element :alias "NumberOfPages"))
   (reading-level :subelement (simple-text-element :alias "ReadingLevel"))
   (binding :accessor item-binding :initform nil
	    :subelement (simple-text-element :alias "Binding"))
   (dewey-decimal-number :accessor item-dewey-decimal-number :initform nil
			 :subelement (simple-text-element :alias "DeweyDecimalNumber"))
   (creators :accessor creators :initform nil
	     :subelement (creator :alias "Creator" :multiple t))
   (actors :accessor actors :initform () :initarg :actors)
   (directors :accessor directors :initform nil :initarg :directors)
   (number-of-items :accessor item-number-of-items :initform nil
		    :subelement (numerical-text-element :alias "NumberOfItems"))
   (manufacturer :accessor manufacturer :initform "" :initarg :manufacturer
		 :subelement (simple-text-element :alias "Manufacturer"))
   (product-group :accessor item-product-group :initform nil
		  :subelement (simple-text-element :alias "ProductGroup")))

  (:metaclass element-class)
  (:tags "ItemAttributes")
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass dimensional-element ()
  ((height :accessor dimension-height :initform nil
	   :subelement (distance-element :alias "Height"))
   (length :accessor dimension-length :initform nil
	   :subelement (distance-element :alias "Length"))
   (width :accessor dimension-width :initform nil
	  :subelement (distance-element :alias "Width"))
   (weight :accessor dimension-weight :initform nil
	   :subelement (weight-element :alias "Weight")))
  (:metaclass element-class))


(defclass offers ()
  ((total-offers :accessor offers-total-offers :initform nil
		 :subelement (numerical-text-element :alias "TotalOffers"))
   (total-offer-pages :accessor offers-total-pages :initform nil
		      :subelement (numerical-text-element :alias "TotalOfferPages"))
   (offers :accessor offers :initform ()
	   :subelement (offer :alias "Offer" :multiple t)))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass offer ()
  ((merchant :accessor offer-merchant :initform nil
	     :subelement (merchant :alias "Merchant"))
   (offer-attributes :accessor offer-attributes :initform nil
		     :subelement (offer-attributes :alias "OfferAttributes"))
   (seller :accessor seller :initform nil :initarg :seller)
   (offer-listing :accessor offer-listing :initform ()
		  :subelement (offer-listing :alias "OfferListing")))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass vendor-like-mixin ()
  ((average-feedback-rating :accessor average-feedback-rating :initform nil
			    :subelement (numerical-text-element :alias "AverageFeedbackRating"))
   (total-feedback :accessor total-feedback :initform nil
		   :subelement (numerical-text-element :alias "TotalFeedback")))
  (:metaclass element-class)
  (:documentation "Mixed into seller and vendor to provide shared slots for the most part"))

(defclass merchant (vendor-like-mixin)
  ((merchant-id :accessor merchant-id :initform nil
		:subelement (simple-text-element :alias "MerchantId"))
   (glancepage :accessor glance-page :initform nil
	       :subelement (simple-text-element :alias "GlancePage")))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass seller (vendor-like-mixin)
  ((sellerid :accessor seller-id :initform nil :initarg :seller-id))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass offer-attributes ()
  ((condition :accessor offer-condition :initform nil
	   :subelement (simple-text-element :alias "Condition"))
   (conditionnote :accessor condition-note :initform nil
	   :subelement (simple-text-element :alias "ConditionNote"))
   (will-ship-expedited :accessor will-ship-expedited :initform nil
	   :subelement (simple-text-element :alias "WillShipExpedited"))
   (will-ship-international :accessor will-ship-international :initform nil
			    :subelement (simple-text-element :alias "WillShipInternational"))
   (subcondition :accessor subcondition :initform nil
		 :subelement (simple-text-element :alias "SubCondition")))
  (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass offer-listing ()
  ((offerlistingid :accessor offer-listing-id :initform nil
		   :subelement (simple-text-element :alias "OfferListingId"))
   (price :accessor price :initform nil
	  :subelement (price-element :alias "Price"))
   (availability :accessor availability :initform nil
		 :subelement (simple-text-element :alias "Availability"))
;   (availabilityattributes :accessor availability-attributes :initform nil
;   :subelement (simple-text-element :alias "OfferListingId"))
   (amount-saved :accessor amount-saved :initform nil
		 :subelement (price-element :alias "AmountSaved"))
   (percentage-saved :accessor percentage-saved :initform nil
		     :subelement (simple-text-element :alias "PercentageSaved"))
   (exchange-id :accessor exchange-id :initform nil 
		:subelement (simple-text-element :alias "ExchangeId"))
   (quantity :accessor quantity :initform nil
	     :subelement (numerical-text-element :alias "Quantity"))
   (eligible-for-saver-shipping :accessor eligible-for-saver-shipping? :initform nil
				:subelement (yes-no-element :alias "IsEligibleForSuperSaverShipping")))
   (:metaclass element-class)
  (:documentation "Summary of offers for a particular item"))

(defclass editorial-review ()
  ((source :accessor review-source :initform nil
	   :subelement (simple-text-element :alias "Source"))
   (content :accessor review-content :initform nil
	   :subelement (simple-text-element :alias "Content")))
  (:metaclass element-class))    

(defclass editorial-review-collection ()
  ((reviews :accessor reviews  :initform nil
	    :subelement (editorial-review :alias "EditorialReview" :multiple t)))
  (:metaclass element-class))