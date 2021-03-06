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
   (:module :lisp
    :components
    ((:file "package")
     (:file "macro"           :depends-on ("package"))
     (:file "map"             :depends-on ("macro"))
     (:file "type"            :depends-on ("map"))
     (:file "type_func"       :depends-on ("type"))
     (:file "vm"              :depends-on ("type"))
     (:file "universe"        :depends-on ("type_func" "vm"))
     (:file "compiler"        :depends-on ("universe"))
     (:file "repl"            :depends-on ("compiler")))))


  :in-order-to ((asdf:test-op (asdf:test-op "clgo-test"))))
