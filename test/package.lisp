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

(defpackage #:clgo-test

  (:nicknames #:gotest)

  (:use #:cl
        #:clgo
        #:bordeaux-threads
        #:fiveam
        #:log4cl)

  (:shadowing-import-from
   #:clgo
   #:byte #:error #:map #:package #:return #:string)

  (:export #:suite))


(in-package :clgo-test)

(fiveam:def-suite suite)

(defun configure-log4cl ()
  (log:config :clear :sane :this-console ;; :daily "log.txt"
              :pattern "%D{%H:%M:%S} %-5p  <%c{}{}{:downcase}> {%t} %m%n")
  (log:config :info))
