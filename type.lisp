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

; declare function (error ...) as an alias for (cl:error ...)
(eval-always
  (setf (symbol-function 'error) #'cl:error))

(const
    (chandir.recv     'chandir.recv)
    (chandir.send     'chandir.send)
    (chandir.both     'chandir.both))
    
(const
    (kind.invalid     nil)
    (kind.bool       'kind.bool)

    (kind.int        'kind.int)
    (kind.int8       'kind.int8)
    (kind.int16      'kind.int16)
    (kind.int32      'kind.int32)
    (kind.int64      'kind.int64)
    (kind.uint       'kind.uint)
    (kind.uint8      'kind.uint8)
    (kind.uint16     'kind.uint16)
    (kind.uint32     'kind.uint32)
    (kind.uint64     'kind.uint64)
    (kind.uintptr    'kind.uintptr)

    (kind.float32    'kind.float32)
    (kind.float64    'kind.float64)
    (kind.complex64  'kind.complex64)
    (kind.complex128 'kind.complex128)
    (kind.string     'kind.string)
    
    (kind.array      'kind.array)
    (kind.chan       'kind.chan)
    (kind.func       'kind.func)
    (kind.interface  'kind.interface)
    (kind.map        'kind.map)
    (kind.ptr        'kind.ptr)
    (kind.slice      'kind.slice)
    (kind.struct     'kind.struct))


; for simplicity, map false => nil and true => cl:t
(const
    (true       cl:t)
    (false      nil))


(const
    (%cpu-bytes (if (> (integer-length most-positive-fixnum) 32) 8 4))
    (%cpu-bits (ash %cpu-bytes 3)))


(const-once (%[] #()))


(deftype kind       () 'symbol)
(deftype chandir    () 'symbol)
(deftype int        () '(signed-byte    #.%cpu-bits))
(deftype int8       () '(signed-byte    8))
(deftype int16      () '(signed-byte    16))
(deftype int32      () '(signed-byte    32))
(deftype int64      () '(signed-byte    64))
(deftype uint       () '(unsigned-byte  #.%cpu-bits))
(deftype uint8      () '(unsigned-byte  8))
(deftype uint16     () '(unsigned-byte  16))
(deftype uint32     () '(unsigned-byte  32))
(deftype uint64     () '(unsigned-byte  64))
(deftype uintptr    () '(unsigned-byte  #.%cpu-bits))
(deftype bool       () 'boolean)
(deftype float32    () 'single-float)
(deftype float64    () 'double-float)
(deftype complex64  () '(complex single-float))
(deftype complex128 () '(complex double-float))
(deftype string     () 'cl:string)

(defstruct (type.go (:conc-name type-))
  (kind       nil    :type kind)
  (size       nil    :type (or null uintptr))
  (comparable true   :type boolean)
  (cl-type    nil    :type (or symbol list))
  ;; number of simple-vector slots needed by this type
  (cl-slots   1      :type (or null uintptr)))

(defstruct (type.array (:include type.go))
  (count      0      :type int)
  (elem       nil :type (or null type.go)))

(defstruct (type.basic (:include type.go))
  (name    (error "type.basic: missing name") :type symbol))

(defstruct (type.chan (:include type.go))
  (dir  chandir.both :type chandir)
  (elem       nil :type (or null type.go)))

(defstruct (type.func (:include type.go))
  (recv       nil :type (or null type.go))
  (params     %[] :type simple-vector)     ;; array of type.go
  (results    %[] :type simple-vector))    ;; array of type.go

(defstruct (type.interface (:include type.go))
  (embeddeds        %[] :type simple-vector)  ;; array of type.go
  (explicit-methods %[] :type simple-vector)  ;; array of gofunc
  (methods          %[] :type simple-vector)) ;; array of gofunc

(defstruct (type.map (:include type.go))
  (key        nil :type (or null type.go))
  (elem       nil :type (or null type.go)))

(defstruct (type.named (:include type.go))
  (name      (error "type.named: missing name") :type symbol)
  (underlying nil :type (or null type.go))
  (methods    %[] :type simple-vector)) ;; array of gofunc

(defstruct (type.ptr (:include type.go))
  (elem       (error "type.ptr: missing elem") :type type.go))

(defstruct (type.slice (:include type.go))
  (elem       nil :type (or null type.go)))

(defstruct (type.struct (:include type.go))
  (fields     %[]    :type simple-vector)) ;; array of reflect.field



(defstruct reflect.field
  (name       ""     :type string)
  (pkgpath    ""     :type string)
  (type       (error "reflect.field: missing type") :type type.go)
  (offset     0      :type uintptr)
  (index      0      :type int))



(defstruct goscope
  (objects    (make-hash-table :test 'equal) :type hash-table)
  (types      (make-hash-table :test 'equal) :type hash-table)
  (parent     nil :type (or null goscope)))

(defstruct (gopackage (:include goscope))
  (name       (error "gopackage: missing name") :type string)
  (path       (error "gopackage: missing path") :type string))




;; base type of goconst, gofunc, govar
(defstruct goobject
  (name       ""     :type string)
  (type       (error "goobject: missing type") :type (or null type.go))
  (scope      nil :type (or null goscope)))

(defstruct (goconst (:include goobject))
  (value      (error "goconst: missing value") :type cl:t))

(defstruct (govar   (:include goobject)))

(defstruct (gofunc  (:include goobject)))

