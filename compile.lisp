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

(func find-obj (scope name)
  (declare (type (or null goscope) scope)
           (type symbol name))
  (loop while scope
        do (let* ((objs (goscope-objs scope))
                  (obj  (htable@ objs name)))
             (if obj
                 (return-from find-obj (the goobj obj))
                 (setf scope (goscope-parent scope))))))

(func find-type (scope name)
  (declare (type (or null goscope) scope)
           (type symbol name))
  (loop while scope
        do (let* ((types (goscope-types scope))
                  (typ   (htable@ types name)))
             (if typ
                 (return-from find-type (the type.go typ))
                 (setf scope (goscope-parent scope))))))

(func decl-obj (scope obj)
  (declare (type goscope scope)
           (type goobj   obj))
  (htable! (goscope-objs scope) (goobj-name obj) obj)
  (values))

(func decl-type (scope name typ)
  (declare (type goscope scope)
           (type symbol  name)
           (type type.go typ))
  (htable! (goscope-types scope) name typ)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(func c-find-obj (c name)
  (declare (type gocompiler c)
           (type symbol name))
  (the (values (or null goobj) &optional)
       (find-obj (gocompiler-scope c) name)))

(func c-find-type (c name)
  (declare (type gocompiler c)
           (type symbol name))
  (the (values (or null type.go) &optional)
       (find-type (gocompiler-scope c) name)))

(func c-decl-obj (c obj)
  (declare (type gocompiler c)
           (type goobj obj))
  (the (values &optional)
       (decl-obj (gocompiler-scope c) obj)))

(func c-decl-type (c name typ)
  (declare (type gocompiler c)
           (type symbol name)
           (type type.go typ))
  (the (values &optional)
       (decl-type (gocompiler-scope c) name typ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

