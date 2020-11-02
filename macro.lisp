;; -*- lisp -*-

;; This file is part of CLGO.
;; Copyright (c) 2020 Massimiliano Ghilardi
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the Lisp Lesser General Public License
;; (http://opensource.franz.com/preamble.html), known as the LLGPL.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the Lisp Lesser General Public License for more details.


(in-package clgo)


(deftype _       () 'cl:t)
(deftype package () 'cl:package)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro eval-always (&body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@body)))

;; declare function (error ...) as an alias for (cl:error ...)
(eval-always
  (setf (symbol-function 'error) #'cl:error))

(defmacro macro (name (&rest args) &body body)
  `(eval-always
     (defmacro ,name (,@args) ,@body)
     (values)))


(eval-always
  (defun tokens_to_string (tokens)
    (declare (cl:type list tokens))
    "convert a list of tokens into a single string.
Each token must be a symbol or a string."
    (apply #'concatenate 'cl:string
           (loop for tok in tokens
                 collect (if (symbolp tok) (symbol-name tok)
                             tok)))))

(eval-always
  (defun new_gensym (&rest tokens)
    "create a new uninterned symbol:
its name is the concatenation of the tokens plus a numeric suffix.
each token must be a symbol or a string."
    (gensym (tokens_to_string tokens))))

(eval-always
  (defun intern_symbol (&rest tokens)
    "create a new symbol and intern it in current package:
the symbol's name is the concatenation of the tokens.
each token must be a symbol or a string."
    (intern (tokens_to_string tokens))))


(macro package (name)
  `(in-package ,(intern (symbol-name name) :keyword)))

(macro with-gensyms (syms &body body)
  `(let ,(loop for sym in syms
               collect `(,sym (new_gensym ',sym "_")))
     ,@body))
    
(eval-always
  (defun split-list-pairs (pairs)
    (declare (cl:type list pairs))
    (loop for (name typ) in pairs
	  collect name into names
	  collect typ into types
	  finally (return-from nil
		    (values (the list names) (the list types))))))
  
(macro func (name (&rest args-and-types) (&rest ret-types) &body body)
  (multiple-value-bind (arg-names arg-types) (split-list-pairs args-and-types)
    (with-gensyms (results)
      `(eval-always
         (declaim (ftype (function (,@arg-types) (values ,@ret-types &optional)) ,name))
         (defun ,name (,@arg-names)
         #|
         (declare ,@(loop for (arg-name arg-type) in args-and-types
                          collect `(cl:type ,arg-type ,arg-name)))
         (the (values ,@ret-types &optional)
              (block ,name ,@body))
         |#
         (macrolet ((return (&rest ,results)
                            `(return-from ,',name (values ,@,results))))
           ,@body))
         (values)))))

(macro const (&rest args)
  `(eval-always
     ,@(loop for arg in args
             collect `(defconstant ,@arg))
     (values)))

(macro const_once (&rest args)
  `(eval-always
     ,@(loop for (name value) in args
             collect `(defconstant ,name
                        (if (boundp ',name)
                            (symbol-value ',name)
                            ,value)))
     (values)))

(macro var (&rest args)
  `(eval-always
     ,@(loop for arg in args
             collect `(defvar ,@arg))
     (values)))
           

(const_once (%[] #()))

; for simplicity, map false => nil and true => cl:t
(const
    (true       cl:t)
    (false      nil))


(func zero_value ((typ (or symbol cons))) (_ boolean)
  (values
   (cond
     ((member typ '(int int8 int16 int32 int64
                    uint uint8 uint16 uint32 uint64 uintptr
                    byte rune))
      0)
     ((member typ '(bool cltype kind symbol _))
      nil) ;; i.e. false if bool, kind_invalid if kind
     ((eq typ 'float64)       0.0d0)
     ((eq typ 'complex64)     #c(0.0f0 0.0f0))
     ((eq typ 'complex128)    #c(0.0d0 0.0d0))
     ((eq typ 'string)        "")
     ((eq typ 'chandir)       'chandir.both)
     ((eq typ 'simple-vector) %[])
     ((and (consp typ) (eq (first typ) 'opt)) nil)
     (cl:t (return nil nil)))
   cl:t))
     

(func slot_zero_value ((struct_name symbol) (slot_name symbol) (typ (or symbol cons))) (_)
  (multiple-value-bind (zero ok) (zero_value typ)
    (if ok
        zero
        `(error "~S: missing initialization for slot (~S ~S ~S)"
                ',struct_name ',slot_name :type ',typ))))

(macro %struct (name &rest slots)
  "each slot must be (slot_name slot_type) or (slot_type) - the latter creates an embedded slot"
  (let ((embedded_types (loop for slot in slots
                              when (null (rest slot))
                                collect (first slot)))
        (named_slots (loop for slot in slots
                           for slot_name = (first  slot)
                           for slot_type = (second slot)
                           for slot_init = (rest (rest slot))
                           when slot_type
                             collect
                           `(,slot_name
                             :type ,slot_type
                             :initarg ,(intern (symbol-name slot_name) :keyword)
                             :initform ,(if slot_init (first slot_init)
                                            (slot_zero_value name slot_name slot_type))))))
    `(defclass ,name ,embedded_types ;; use inheritance to emulate embedding
         ,named_slots)))

;; approximate Go named types - currently only works
;; for (type foo (struct ...))
;; and (type foo existing_type)
(macro type (name underlying)
  (cond
    ;; (type name (struct ...))
    ((and (consp underlying) (eq 'struct (first underlying)))
     (let ((underlying_name (intern_symbol 'underlying_ name)))
       `(progn
          (%struct ,underlying_name ,@(rest underlying))
          (defclass ,name (,underlying_name) ()))))
    (true
     `(deftype ,name () ',underlying))))
      

(macro new (typ &rest initargs &key &allow-other-keys)
  "create a new object of type 'typ.
equivalent to (make-instance 'typ ...)"
  `(make-instance ',typ ,@initargs))

(macro -> (object slot_name &rest nested_slot_names)
  "read object slot.
Common Lisp syntax (slot-value object 'slot_name) is really verbose
so we shorten it to (-> object slot_name)
 
Traversing multiple nested slots in Common Lisp is even more verbose:
  (slot-value (slot-value object 'slot_name1) 'slot_name2)
so we shorten in to (-> object slot_name1 slot_name2 ...)"
  (if nested_slot_names
      `(-> (slot-value ,object ',slot_name) ,@nested_slot_names)
      `(slot-value ,object ',slot_name)))

;; no need to define an expansion for (setf (-> object slot_name) new_value)
;; because the default expansion is correct


(macro += (place delta)
  "decrement PLACE by delta. does not return a value"
  `(progn
     (incf ,place ,delta)
     (values)))

(macro -= (place delta)
  "increment PLACE by delta. does not return a value"
  `(progn
     (decf ,place ,delta)
     (values)))

(macro ++ (place)
  "increment PLACE by 1. does not return a value"
  `(+= ,place 1))

(macro -- (place)
  "decrement PLACE by 1. does not return a value"
  `(-= ,place 1))

(macro for (init_or_pred pred_or_body &optional post &body body)
  "loop executing body zero or more times.
The short syntax (for pred body) performs the following loop:
  1. evaluate pred
  2. if false, break the loop
  3. execute body then repeat from 1.
The long syntax (for init pred post &body body) is similar:
  0. evaluate init
  1. evaluate pred
  2. if false, break the loop
  3. execute body
  4. execute post then repeat from 1.
In the long syntax, init must be (var (x0 init0) ...) or a statement"
  (if (null post)
      ;; short syntax
      (let ((pred init_or_pred)
            (body pred_or_body))
        `(do nil ((not ,pred) (values))
           (progn ,@body))
           (values))
      ;; long syntax
      (let ((init init_or_pred)
            (pred pred_or_body))
        ;; (for (var (x0 init0) (x1 init1) ...) pred post ...)
        (if (and (consp init) (eq 'var (first init)))
            `(do ,(rest init) ((not ,pred) (values))
               (progn ,@body ,post))
            ;; (for init-statement pred post ...)
            `(progn
               ,init
               (do nil ((not ,pred) (values))
                 (progn ,@body ,post)))))))


(macro break ()
  "exit from innermost (for) or (switch)"
  `(return-from nil (values)))

(macro range (for (&optional key value) type container &body body)
  "loop on the items of container (which is evaluated only once).
type must be one of :vector :list :map
for vectors and lists, key is set to the iteration index."
  (ecase for (for))
  (let* ((keyp   (and key   (not  (eq '_ key))))
         (valuep (and value (not  (eq '_ value))))
         (keyx   (if keyp   key   (new_gensym 'key_)))
         (valuex (if valuep value (new_gensym 'value_))))
    (ecase type
      (:vector `(loop ,@(when keyp `(for ,key from 0))
                      for ,valuex across ,container
                      do (progn ,@body)
                      finally (break)))
      (:list `(loop ,@(when keyp `(for ,key from 0))
                    for ,valuex in ,container
                    do (progn ,@body)
                    finally (break)))
      (:map (with-gensyms (iter ok)
              `(with-hash-table-iterator (,iter ,container)
                 (loop
                   (multiple-value-bind (,ok ,keyx ,valuex) (,iter)
                     (unless ,ok (break))
                     ,@body))
                 (values)))))))



;; Go is case-sensitive => provide functions and macros
;; to temporarily make Common Lisp case sensitive too
;; and to restore the previous settings.
;;
;; the package clgo itself does NOT need to change case sensitivity:
;; such feature is provided for user convenience.
(var (%saved-readtable-case))

(func set-readtable-case-sensitive () (keyword)
  (setf %saved-readtable-case (readtable-case *readtable*))
  (return (setf (readtable-case *readtable*) :invert)))


(func restore-readtable-case () (symbol)
  (return (setf (readtable-case *readtable*) %saved-readtable-case)))

(macro with-readtable-case-sensitive (&body body)
  `(let ((%saved-readtable-case (readtable-case *readtable*)))
     (unwind-protect
          (progn
            (setf (readtable-case *readtable*) :invert)
            ,@body)
       (restore-readtable-case))))
