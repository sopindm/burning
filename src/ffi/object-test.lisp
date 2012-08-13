(in-package :burning-ffi-test)

(in-case object-test)

(deftest simple-object
  (let ((object (make-holder)))
    (hld-set-holded object 7)
    (!= (hld-holded object) 7)
    (hld-set-holded object 12)
    (!= (hld-holded object) 12)))

(deftest two-destructors
  (!error (burning-ffi::load-ffi-actions '(:class "wrong_class" (:destructor "dst1") (:destructor "dst2")))
	  "Cannot define more than one destructor for class WRONG-CLASS."))

(deftest with-object-test
  (!= (hld-holded (with-object (holder (make-holder))
		    (hld-set-holded holder 12)
		    holder))
      -1)
  (let ((objects (with-objects ((holder1 (make-holder))
				(holder2 (make-holder)))
		   (list holder1 holder2))))
    (!= (hld-holded (first objects)) -1)
    (!= (hld-holded (second objects)) -1)))

(deftest inheritance-test
  (let ((base-object (make-holder))
	(derived-object (make-mega-holder)))
    (!t (typep base-object 'int-holder))
    (!null (typep base-object 'mega-holder))
    (!t (typep derived-object 'int-holder))
    (!t (typep derived-object 'mega-holder))))
