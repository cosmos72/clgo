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

(func lookup-obj (scope name)
  (declare (type (or null goscope) scope)
           (type symbol name))
  (loop while scope
        do (let* ((objs (goscope-objs scope))
                  (obj  (htable@ objs name)))
             (if obj
                 (return-from lookup-obj (the goobj obj))
                 (setf scope (goscope-parent scope))))))

(func lookup-type (scope name)
  (declare (type (or null goscope) scope)
           (type symbol name))
  (loop while scope
        do (let* ((types (goscope-types scope))
                  (typ   (htable@ types name)))
             (if typ
                 (return-from lookup-type (the type.go typ))
                 (setf scope (goscope-parent scope))))))

                   
           
(func resolve-obj (c name)
  (declare (type gocompiler c)
           (type symbol name))
  (the (values (or null goobj) &optional)
       (lookup-obj (gocompiler-scope c) name)))

(func resolve-type (c name)
  (declare (type gocompiler c)
           (type symbol name))
  (the (values (or null type.go) &optional)
       (lookup-type (gocompiler-scope c) name)))
