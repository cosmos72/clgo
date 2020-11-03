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

(const (uintptr_max (1- (ash 1 _cpu_bits))))

;; truncate n to int
(func to_int ((n integer)) (int)
  (let ((u (logand n uintptr_max)))
    (if (>= n 0)
        u
        (- u uintptr_max 1))))
        
    
(type vm (struct
          (code    array (make-array '(10) :initial-element 0 :adjustable true :fill-pointer 0))))


(defun vm.tokens (vm &rest tokens)
  (declare (cl:type vm vm))
  (range for (_ token) :list tokens
    (vector-push-extend token (-> vm code)))
  (values))

(func vm.int ((vm vm) (value integer)) ()
  (cond
    ((typep value 'int16) (vm.tokens vm '_lit2s_ value))
    ((typep value 'int32) (vm.tokens vm '_lit4s_ value 0))
    (true                 (vm.tokens vm '_lit8s_ (to_int value) 0 0 0)))
  (return))


