(in-package :closless)

(defclass <person> ()
  (name address))

(defclass <employee> (<person>)
  (employer))

(defgeneric <display>)

;;
;; "debug" facilities
;;
(defun display-with-tabs (text tab-count)
  (print (format NIL "~a~a" (make-string tab-count :initial-element #\Tab) text)))
(defun display-with-tab (text) (display-with-tabs text 1))
(defun display-with-2-tabs (text) (display-with-tabs text 2))
(defun newline () (print ""))

;;;
;;; Tests for auxiliary methods
;;; 

;;
;; Primary & auxiliary methods for <person> and <employee>
;; 
(defmethod <display> ((<person> receiver) args)
  (display-with-2-tabs "<person> Primary")
  (display-with-2-tabs "<person> Next method exists?")
  (display-with-2-tabs (next-method-p))
  (display-with-2-tabs "<person> Actual output:")
  (display-with-2-tabs (slot-value receiver 'name))
  (display-with-2-tabs (slot-value receiver 'address)))

(defmethod <display> ((<employee> receiver) args)
  (display-with-2-tabs "<employee> Primary")
  (display-with-2-tabs "<employee> Next method exists?")
  (display-with-2-tabs (next-method-p))
  (display-with-2-tabs "<employee> Calling next most specific primary method")
  (call-next-method)
  (display-with-2-tabs "<employee> Actual output:")
  (display-with-2-tabs (slot-value receiver 'employer)))

(defmethod <display> :before ((<person> receiver) args)
  (display-with-tab "<person> Before"))

(defmethod <display> :before ((<employee> receiver) args)
  (display-with-tab "<employee> Before"))

(defmethod <display> :around ((<person> receiver) args)
  (print "<person> Around")
  (print "Next method exists?")
  (print (next-method-p))
  (call-next-method)
  (print "<person> Around"))

(defmethod <display> :around ((<employee> receiver) args)
  (print "<employee> Around")
  (print "Next method exists?")
  (print (next-method-p))
  (print "Calling next most specific :around method")
  (call-next-method)
  (print "<employee> Around"))

(defmethod <display> :after ((<person> receiver) args)
  (display-with-tab "<person> After"))

(defmethod <display> :after ((<employee> receiver) args)
  (display-with-tab "<employee> After"))

;;
;; Testing that the specificity order is correct for primary methods 
;; as well as the auxiliary methods :around, :before and :after.
;; 
(defun test-1 ()
  (print "Test case #1")
  (newline)
  (let ((p (make-object :class <person>)))
    (setf (slot-value p 'name) "Pascal")
    (setf (slot-value p 'address) "Brussels")
    (call-generic-function <display> p))
  (newline)
  (newline)
  (newline)
  (print "Test case #2")
  (newline)
  (let ((p (make-object :class <employee>)))
    (setf (slot-value p 'name) "Pascal")
    (setf (slot-value p 'address) "Brussels")
    (setf (slot-value p 'employer) "VUB")
    (call-generic-function <display> p))
  :done)

;;
;; Testing that the return values is correctly 
;; propagated "back" from the primary method.
;; 
(defvar <calculation>
  (make-class :direct-slots '(value1 value2)))

(defgeneric <addition>)

(defmethod <addition> ((<calculation> receiver) args)
  (let ((val1 (slot-value receiver 'value1))
        (val2 (slot-value receiver 'value2)))
    (print "Going to add the following 2 numbers:")
    (print val1)
    (print val2)
    (+ val1 val2)))

(defmethod <addition> :around ((<calculation> receiver) args)
  (let ((val (call-next-method)))
    (assert (eq val 7))
    (print "Value received in :around (should be 7):")
    (print val)
    val))

(defun test-2 ()
  (newline)
  (newline)
  (newline)
  (print "Test case #3")
  (newline)
  (let ((c (make-object :class <calculation>)))
    (setf (slot-value c 'value1) 5)
    (setf (slot-value c 'value2) 2)
    (let ((return-val (call-generic-function <addition> c)))
      (assert (eq 7 return-val))))
  :done)

;;;
;;; Tests for multiple dispatch
;;;

;;
;; Test multiple dispatch:
;; we want the method with the most specific receiver
;; and most specific args to be dispatched.
;; 

(defclass <animal> () ())
(defclass <dog> (<animal>) ())
(defclass <cat> (<animal>) ())
(defclass <pig> (<animal>) ())

(defclass <food> () ())
(defclass <dog-food> (<food>) ())
(defclass <cat-food> (<food>) ())
(defclass <pig-food> (<food>) ())

(defgeneric <eat>)

(defmethod <eat> ((<dog> dog) (<dog-food> dog-food))
  (string "I am a dog and I eat dog food!"))

(defmethod <eat> ((<dog> dog) (<cat-food> cat-food))
  (string "I am a dog so I cannot eat cat food!"))

(defmethod <eat> ((<dog> dog) (<food> food))
  (string "I am a dog, this food I cannot eat!"))

(defmethod <eat> ((<cat> cat) (<cat-food> cat-food))
  (string "I am a cat and I eat cat food!"))

(defmethod <eat> ((<cat> cat) (<dog-food> dog-food))
  (string "I am a cat so I cannot eat dog food!"))

(defmethod <eat> ((<cat> cat) (<food> food))
  (string "I am a cat, this food I cannot eat!"))

(defmethod <eat> ((<animal> animal) (<food> food))
  (string "I am some animal, and I eat some food!"))

(defun test-3 ()
  (newline)
  (newline)
  (newline)
  (print "Test case #4")
  (let ((d    (make-object :class <dog>))
         (d-f (make-object :class <dog-food>))
         (c   (make-object :class <cat>))
         (c-f (make-object :class <cat-food>))
         (p	  (make-object :class <pig>))
         (p-f (make-object :class <pig-food>)))
    (assert (string= 
              (call-generic-function <eat> d d-f)
              (string "I am a dog and I eat dog food!")))
    (assert (string=
              (call-generic-function <eat> d c-f)
              (string "I am a dog so I cannot eat cat food!")))
    (assert (string=
              (call-generic-function <eat> d p-f)
              (string "I am a dog, this food I cannot eat!")))
    (assert (string=
              (call-generic-function <eat> c c-f)
              (string "I am a cat and I eat cat food!")))
    (assert (string=
              (call-generic-function <eat> c d-f)
              (string "I am a cat so I cannot eat dog food!")))
    (assert (string=
              (call-generic-function <eat> c p-f)
              (string "I am a cat, this food I cannot eat!")))
    (assert (string=
              (call-generic-function <eat> p p-f)
              (string "I am some animal, and I eat some food!")))
    (print "All tests succeed!")
    :done))

;;
;; Testing that multiple dispatch also works when 
;; there is a class (i.e. <B>) in between <A> and 
;; <C> in the class inheritance, also check that
;; it works on aux methods.
;;

(defclass <A> () ())
(defclass <B> (<A>) ())
(defclass <C> (<B>) ())
(defclass <D> () ())

(defgeneric <hello-world>)

(defmethod <hello-world> ((<A> A) (<B> B))
  (error "<hello-world> <A> <B> should never be called!"))

(defmethod <hello-world> ((<A> A) (<C> C))
  t)

(defmethod <hello-world> :around ((<A> A) (<D> D))
  (error "<hello-world> :around <A> <D> should never be called!"))

(defmethod <hello-world> :around ((<A> A) (<B> B))
  (print "<A> <B> should be called second")
  (let ((res (call-next-method))) ; go on with primary method(s)
    (print "Received T?:")
    (print res)))

(defmethod <hello-world> :around ((<A> A) (<C> C))
  (print "<A> <C> should be called first")
  (call-next-method)) ; call next :around, i.e. <A> <B>

(defun test-4 ()
  (newline)
  (newline)
  (newline)
  (print "Test case #5")
  (let ((A (make-object :class <A>))
         (C (make-object :class <C>)))
    (assert (eq t (call-generic-function <hello-world> A C)))
    (print "Test succeed")
    :done))
