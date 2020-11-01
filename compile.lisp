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

(func find_obj ((scope (or null goscope)) (name symbol)) ((or null goobj))
  (let ((sc scope))
    (loop while sc do
      (let* ((objs (goscope.objs sc))
             (obj  (map@ objs name)))
        (if obj
            (return obj)
            (setf sc (goscope.parent sc))))))
  (return nil))

(func find_type ((scope (or null goscope)) (name symbol)) ((or null type_go))
  (let ((sc scope))
    (loop while sc do
      (let* ((types (goscope.types sc))
             (typ   (map@ types name)))
        (if typ
            (return typ)
            (setf sc (goscope.parent sc))))))
  (return nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(func c_find_obj ((c gocompiler) (name symbol)) ((or null goobj))
  (return (find_obj (gocompiler.scope c) name)))

(func c_find_type ((c gocompiler) (name symbol)) ((or null type_go))
  (return (find_type (gocompiler.scope c) name)))

(func c_decl_obj ((c gocompiler) (obj goobj)) ()
  (decl_obj (gocompiler.scope c) obj))

(func c_decl_type ((c gocompiler) (name symbol) (typ type_go)) ()
  (decl_type (gocompiler.scope c) name typ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

