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

(deftype map (&optional key value)
  (declare (ignore key value))
  'hash-table)

(eval-always
  ;; declare function (make_map ...) as an alias for (make-hash-table ...)
  (setf (symbol-function 'make_map) #'make-hash-table)
  ;; declare function (map.len ...) as an alias for (hash-table-count ...)
  (setf (symbol-function 'map.len) #'hash-table-count))

;; get from map i.e. hash-table
(declaim (inline map@))
(func map@ ((m map) (key _)) (_ boolean)
  (gethash key m))

;; put into map i.e. hash-table
(declaim (inline map!))
(func map! ((m map) (key _) (value _)) (_)
  (return (setf (gethash key m) value)))

