(in-package :closless)

(defvar *object*)

(defstruct class
  (direct-superclass *object*)
  (direct-slots '()))

(unless (boundp '*object*)
  (setf *object* (make-class :direct-superclass nil)))

(defun class-all-superclasses (class)
  (loop for c = class then (class-direct-superclass c)
        while c
        collect c))

(defun subclassp (class1 class2)
  (member class2 (class-all-superclasses class1)))

(defstruct object
  (class *object*)
  (slots (make-hash-table)))

(defun slot-value (object slot-name)
  (gethash slot-name (object-slots object)))

(defun (setf slot-value) (value object slot-name)
  (setf (gethash slot-name (object-slots object))
        value))

(defun instancep (object class)
  (subclassp (object-class object) class))

(defstruct generic-function
  (methods '())
  (before-methods '())
  (after-methods '())
  (around-methods '()))

(defstruct method
  (specializer *object*)
  (function (error "No method function provided."))
  (args-specializers '())) ; ((<class_1> obj_1) (<class_2> obj_2) ... (<class_n> obj_n))

(defun function-name (fn)
  (nth-value 2 (function-lambda-expression fn)))

(defun helper-find-method (gf specializer methods-accessor)
  (loop for method in (funcall methods-accessor gf)
     when (eql specializer (method-specializer method))
     return method))

(defun helper-remove-method (gf method methods-accessor)
  (funcall 
    (fdefinition (list 'setf (function-name methods-accessor)))
    (remove method (funcall methods-accessor gf))
    gf))

(defun helper-add-method (gf method methods-accessor)
  ; (push item place) <=> (setf place (cons item place))
  (funcall 
    (fdefinition (list 'setf (function-name methods-accessor)))
    (cons method (funcall methods-accessor gf))
    gf))
    
(defun instancep-args (actual-args formal-args)
  (if (eq actual-args NIL)
    t ; done
    (if (listp (car formal-args))
      (and 
        (instancep (car actual-args) (eval (caar formal-args))) ; check type
        (instancep-args (cdr actual-args) (cdr formal-args))) ; recurse
      t))) ; no type hint
  
(defun helper-compute-applicable-methods (gf receiver methods-accessor)
  (loop for method in (funcall methods-accessor gf)
    when (and 
           (instancep receiver (method-specializer method))
           (instancep-args *args* (method-args-specializers method)))
    collect method))

;; For every actual arg, we want to match with the "best" possible formal arg, 
;; i.e. if there exist another method which lies between the current candidate, 
;; and the actual arg (in terms of type), then we want to use that method as the 
;; new best candidate, because it is more specific. We do this recursively for 
;; all parameters.
(defun check-args-specializers (args method-specializers candidate-specializers)
  (if (eq args NIL)
    t
    (and
      (subclassp ; check if the method is a better candidate
        (eval (caar method-specializers))
        (eval (caar candidate-specializers)))
      (check-args-specializers ; recurse
        (cdr args) 
        (cdr method-specializers)
        (cdr candidate-specializers)))))
     
(defun has-more-specific-args (method candidate)
  (check-args-specializers 
    *args* 
    (method-args-specializers method)
    (method-args-specializers candidate)))

(defun helper-select-most-specific-method (methods)
  (loop with candidate = (first methods)
    for method in (rest methods)
    when (and 
           (subclassp ; check receiver
             (method-specializer method)
             (method-specializer candidate))
           (has-more-specific-args method candidate)) ; check args
    do (setq candidate method)
    finally (return candidate)))

(defun find-method (gf specializer)
  (helper-find-method gf specializer #'generic-function-methods))

(defun remove-method (gf method)
  (helper-remove-method gf method #'generic-function-methods))

(defun add-method (gf method)
  (helper-add-method gf method #'generic-function-methods))

(defun compute-applicable-methods (gf receiver)
  (helper-compute-applicable-methods gf receiver #'generic-function-methods))

(defun select-most-specific-method (methods)
  (helper-select-most-specific-method methods))

(defun method-qualifier->method-accessor (method-qualifier)
  (cond
    ((eq method-qualifier ':after)  #'generic-function-after-methods)
    ((eq method-qualifier ':before) #'generic-function-before-methods)
    ((eq method-qualifier ':around) #'generic-function-around-methods)
    (t (error "Invalid method-qualifier" method-qualifier))))

(defun add-auxiliary-method (gf method method-qualifier)
  (let ((method-accessor (method-qualifier->method-accessor method-qualifier)))
    (helper-add-method gf method method-accessor)))

(defun compute-applicable-auxiliary-methods (gf receiver method-qualifier)
  (let ((method-accessor (method-qualifier->method-accessor method-qualifier)))
    (helper-compute-applicable-methods gf receiver method-accessor)))

(defparameter *applicable-methods* NIL)
(defparameter *receiver* NIL)
(defparameter *round* NIL)
(defparameter *args* NIL)
(defparameter *gf* NIL)

(defmacro next-method-p ()
  `(and 
     (not (or (eq *round* :before) (eq *round* :after))) ; only supported for :around methods and primary methods, as per spec
     (not (eq *applicable-methods* NIL))))

(defun order-methods-by-specificity (lst)
  (if (eq lst '())
    '()
    (let* ((most-specific-method (select-most-specific-method lst))
           (new-lst (remove most-specific-method lst)))
      (cons most-specific-method (order-methods-by-specificity new-lst)))))

(defun execute-most-specific-applicable-method ()
  (let* ((most-specific-method (select-most-specific-method *applicable-methods*))
         (*applicable-methods* (remove most-specific-method *applicable-methods*)))
    (funcall (method-function most-specific-method) *receiver* *args*)))

(defun execute-round-by-specificity ()
  (let ((methods (compute-applicable-auxiliary-methods *gf* *receiver* *round*)))
    (loop for method in (order-methods-by-specificity methods) do
      (funcall (method-function method) *receiver* *args*))))

(defun execute-after-round ()
  (let ((*round* :after))
    (execute-round-by-specificity)))

(defun execute-before-round ()
  (let ((*round* :before))
    (execute-round-by-specificity) ; ignore return value
    (execute-next-round))) ; execute primary methods

(defun execute-primary-round ()
  (let ((*round* :primary)
        (*applicable-methods* (compute-applicable-methods *gf* *receiver*)))
    (let ((res (call-next-method)))
      (execute-next-round) ; execute :after methods, ignore return value
      res))) ; return value of the call to the primary method is returned

(defun execute-next-round ()
  (cond
    ((eq *round* :around)
      (execute-before-round))
    ((eq *round* :before)
      (execute-primary-round))
    ((eq *round* :primary)
      (execute-after-round))
    (t (no-next-method)))) ; as per spec

; not sure if we need to implement this
;(defmacro no-next-method ()
;  (error "No next method to execute after round:" *round*))

(defmacro call-next-method () 
  `(if (or (eq *round* :before) (eq *round* :after))
     (error "call-next-method not supported for " *round*)
     (if (eq *applicable-methods* NIL)
       (execute-next-round) ; either execute :before methods, or :after methods, depending on the current state of *round*
       (execute-most-specific-applicable-method)))) ; execute most specific method; enduser's responsability to use call-next-method accordingly

(defun call-generic-function (gf receiver &rest args)
  (let* ((*gf* gf)
         (*args* args)
         (*round* :around)
         (*receiver* receiver)
         (*applicable-methods* (compute-applicable-auxiliary-methods gf receiver *round*)))
    (call-next-method))) ; start the whole "call stack" by executing the :around methods first

;; Normalize the parameter list, i.e. remove type hints.
;; e.g. ((<class_1> obj_1) (<class_2> obj_2) ... (<class_n> obj_n))
;;      <=>
;;      (obj_1 obj_2 ... obj_n)
(defun normalize-args (args)
  (mapcar #'(lambda (arg)
              (if (listp arg)
                (cadr arg)
                arg))
    args))

(defmacro defmethod-auxiliary-method (function-name auxiliary-method-type function-args body)
  `(add-auxiliary-method
     ,function-name
     (make-method
       :specializer ,(caar function-args)
       :function (lambda ,(normalize-args function-args) ,@body)
       :args-specializers ',(cdr function-args))
     ,auxiliary-method-type))

(defmacro defmethod-primary-method (function-name function-args body)
  `(add-method 
     ,function-name
     (make-method
       :specializer ,(caar function-args)
       :function (lambda ,(normalize-args function-args) ,@body)
       :args-specializers ',(cdr function-args))))

(defmacro defmethod (first-param second-param third-param &rest args)
  `(if ,(listp second-param)
     (defmethod-primary-method ,first-param ,second-param ,(cons third-param args))
     (defmethod-auxiliary-method ,first-param ,second-param ,third-param ,args)))

(defmacro defgeneric (name)
  `(defparameter ,name (make-generic-function)))

;  `(progn
;     (defparameter ,name (make-generic-function))
;     #'(lambda (&rest args)
;         (funcall #'call-generic-function ,name args))))

(defmacro defclass (name superclass &rest args)
  (if (eq superclass '())
    `(defparameter ,name
       (make-class :direct-slots ',@args))
    `(defparameter ,name
       (make-class
         :direct-superclass ,(car superclass) ; what do with cdr? (e.g. multiple inheritance)
         :direct-slots ',@args))))
