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

(def-suite universe-suite :in suite)
(in-suite universe-suite)

(def-test rerun (:compile-at :definition-time)
  (let ((u go::*universe*))
    (is (= 19 (hash-table-count (go::gopackage-types u))))
    (is (= 3 (hash-table-count (go::gopackage-objs u))))))
