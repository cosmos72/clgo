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

;; basic types
bool rune int float64 complex128 string

;; bool constants
false true

;; rune constants
#\x ;; Common Lisp syntax - at least temporarily

;; integer constants
[+-]?[0-9]+

;; float64 constants
[+-]?[0-9]+\.[0-9]+  ; followed by optional exponent: (e[+-]?[0-9]+)?

;; complex128 constants
(complex real imag) ;; where real and imag are float64 constants

;; string
"printable-ascii-sequence-except-doublequote-and-backslash"
;; backslash and doublequote can only appear in the following combinations
;; \\  \"  \'  \a \b \d \e \f \n \r \t

;; TBD: error

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; operators on basic types
== !=      ;; on all basic types
+          ;; on: rune int float64 complex128 string
- *  /     ;; on: rune int float64 complex128.
< <= > >=  ;; on  rune int float64
% & ^ | << >>  ;; on: rune int   ;; note: reading | on Common Lisp is tricky
and or not     ;; on: bool. they implement usual short-circuit logic

;; function or closure call
(funcexpr arg0 arg1 arg2 ... )

;; struct field access. equivalent to C/Go/Java obj.field1.field2
(-> obj field1 field2 ... )

;; string, slice and map indexing
([] obj index)

;; string and slice slicing
([] obj start end)
([] obj start end end-capacity)

;; type conversion
(newtype obj)

;; create a closure
(lambda ((arg0 type0) (arg1 type1) ...) (rettype0 ...)
  stmt
  ;; ...
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; statements ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; statements do not produce a value - they are used only for their side effects

;; operators on basic types
=      ;; on all basic types
+=        ;; on: rune int float64 complex128 string
++ --        ;; on: rune int float64 complex128. they are unary.
-= *= /=        ;; on: rune int float64 complex128.
%= &= |= ^= <<= >>= ;; on: rune int. again, reading | on Common Lisp is tricky

(if test then else)      ;; else is optional
(while test stmt ... )   ;; zero or more statements after test
(for init test post stmt ... ) ;; zero or more statements after post
(range (key value) container stmt ... )
(switch expr (case expr0 stmt ... ) (default stmt ... ) ) ;; zero or more cases
(go (funcexpr arg0 arg1 arg2 ... )) ;; execute funcexpr in new thread
;;                ;; note: funcexpr and arguments are evaluated in current thread
(return arg0 arg1 arg2 ... )
({} stmt ... )            ;; block: a group of zero or more statements
(break)                   ;; exit from innermost 'for 'case or 'while
;; TBD: fallthrough?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; declarations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; define a local or global constant. its type is inferred from value,
;; or can be set explicitly by replacing 'myvalue with '(mytype myvalue)
(const myname myvalue)
;; define multiple constants.
(const
  (myname1 myvalue1)
  (myname2 myvalue2)
  ;; ...
  )


;; define a local or global variable. its type is inferred from value,
;; or can be set explicitly by replacing 'myvalue with '(mytype myvalue)
(var myname myvalue)
;; define multiple variables.
(var
  (myname1 myvalue1)
  (myname2 myvalue2)
  ;; ...
  )

;; define a global function
(func name ((arg0 type0) ...) (rettype0 ...)
  stmt
  ;; ...
)

;; define a type
(type name othertype)
(type name (map key elem))
(type name (slice elem))
(type name (func ((arg0 type0) ...) (rettype0 ...)))
(type name (struct ([name1] type1) (name2 type2) ...))
;; TBD: interface
;; intentionally absent: array chan ptr      
