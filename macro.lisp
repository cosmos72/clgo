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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro eval-always (&body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@body)))

(defmacro macro (name (&rest args) &body body)
  `(eval-always
     (defmacro ,name (,@args) ,@body)
     (values)))

(macro func (name (&rest args) &body body)
  `(eval-always
     (defun ,name (,@args) ,@body)
     (values)))

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
(defvar %saved-readtable-case)

(func set-readtable-case-sensitive ()
  (setf %saved-readtable-case (readtable-case *readtable*))
  (setf (readtable-case *readtable*) :invert))

(func restore-readtable-case ()
  (setf (readtable-case *readtable*) %saved-readtable-case))

(macro with-readtable-case-sensitive (&body body)
  `(let ((%saved-readtable-case (readtable-case *readtable*)))
     (unwind-protect
          (progn
            (setf (readtable-case *readtable*) :invert)
            ,@body)
       (restore-readtable-case))))



(declaim (inline htable@))
(func htable@ (hash-table key)
  (gethash key hash-table)) 

(declaim (inline htable!))
(func htable! (hash-table key value)
  (setf (gethash key hash-table) value))
