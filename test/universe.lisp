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


(in-package :clgo-test)

(def-suite universe_suite :in suite)
(in-suite universe_suite)

(def-test universe_content (:compile-at :definition-time)
  (let ((u go::*universe*))
    (is (= 19 (go::map.len (-> u go::types))))
    (is (= 3  (go::map.len (-> u go::objs))))))
