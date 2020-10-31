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

(asdf:defsystem :clgo-test
  :name "CLGO-TEST"
  :version "0.0.0"
  :author "Massimiliano Ghilardi"
  :license "LLGPL"
  :description "test suite for CLGO"

  :depends-on (:bordeaux-threads
               :clgo
               :log4cl
               :fiveam)

  :components
  ((:module :test
    :components ((:file "package")
                 (:file "universe"       :depends-on ("package"))
                 (:file "run-suite"      :depends-on ("universe")))))
  
  :perform (asdf:test-op
            (o c)
            (eval (read-from-string "(fiveam:run! 'clgo-test:suite)"))))
