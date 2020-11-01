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

(func find-obj ((scope (or null goscope)) (name symbol)) ((or null goobj))
  (let ((sc scope))
    (loop while sc do
      (let* ((objs (goscope-objs sc))
             (obj  (htable@ objs name)))
        (if obj
            (return-from find-obj (the goobj obj))
            (setf sc (goscope-parent sc)))))))

(func find-type ((scope (or null goscope)) (name symbol)) ((or null type.go))
  (let ((sc scope))
    (loop while sc do
      (let* ((types (goscope-types sc))
             (typ   (htable@ types name)))
        (if typ
            (return-from find-type (the type.go typ))
            (setf sc (goscope-parent sc)))))))

(func decl-obj ((scope goscope) (obj goobj)) ()
  (htable! (goscope-objs scope) (goobj-name obj) obj)
  (values))

(func decl-type ((scope goscope) (name symbol) (typ type.go)) ()
  (htable! (goscope-types scope) name typ)
  (values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(func c-find-obj ((c gocompiler) (name symbol)) ((or null goobj))
  (find-obj (gocompiler-scope c) name))

(func c-find-type ((c gocompiler) (name symbol)) ((or null type.go))
  (find-type (gocompiler-scope c) name))

(func c-decl-obj ((c gocompiler) (obj goobj)) ()
  (decl-obj (gocompiler-scope c) obj))

(func c-decl-type ((c gocompiler) (name symbol) (typ type.go)) ()
  (decl-type (gocompiler-scope c) name typ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

