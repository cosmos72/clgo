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


;;;; * CLGO

(in-package :cl-user)

(defpackage #:clgo

  (:nicknames #:go)

  (:use #:cl
        #:bordeaux-threads)

  ;; avoid conflict with symbols cl:byte cl:error cl:string cl:return cl:t
  (:shadow #:byte #:error #:string #:return #:t)

  (:export #:macro #:func))
