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


(in-package :clgo)


(deftype _ () cl:t)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro eval-always (&body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@body)))

(defmacro macro (name (&rest args) &body body)
  `(eval-always
     (defmacro ,name (,@args) ,@body)
     (values)))

(defmacro with-gensyms (syms &body body)
  `(let ,(loop for sym in syms
               collect `(,sym (gensym (concatenate 'string (symbol-name ',sym) "."))))
     ,@body))
    
(eval-always
  (defun split-list-pairs (pairs)
    (declare (type list pairs))
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
                          collect `(type ,arg-type ,arg-name)))
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

(macro const-once (&rest args)
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



(declaim (inline htable@))
(func htable@ ((h hash-table) (key _)) (_ boolean)
  (gethash key h))

(declaim (inline htable!))
(func htable! ((h hash-table) (key _) (value _)) (_)
  (setf (gethash key h) value))
