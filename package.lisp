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

  ;; avoid conflict with existing symbols in common-lisp package
  (:shadow #:byte #:break #:error #:map #:package #:return #:string #:t #:type
           #:++)

  (:export #:const #:for #:func #:macro #:package #:range #:return #:struct #:type #:var
           #:->
           #:int  #:int8  #:int16  #:int32  #:int64
           #:uint #:uint8 #:uint16 #:uint32 #:int64 #:uintptr
           #:bool #:byte #:float32 #:float64 #:complex64 #:complex128
           #:error))
