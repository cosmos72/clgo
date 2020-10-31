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


(in-package :cl-user)

(asdf:defsystem :clgo
  :name "CLGO"
  :version "0.0.0"
  :license "LLGPL"
  :author "Massimiliano Ghilardi"
  :description "Subset of Go language with Lisp syntax"

  :depends-on (:bordeaux-threads)

  :components
  ((:static-file "clgo.asd")

   (:file "package")
   (:file "macro"           :depends-on ("package"))
   (:file "type"            :depends-on ("macro"))
   (:file "type-func"       :depends-on ("type"))
   (:file "universe"        :depends-on ("type-func"))
   (:file "compile"         :depends-on ("universe")))


  :in-order-to ((asdf:test-op (asdf:test-op "clgo-test"))))
