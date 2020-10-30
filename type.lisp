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

; avoid conflict with symbols cl:byte cl:nil cl:string cl:t
(eval-always
  (shadow '(byte error string t )))

(const
    (chandir.recv     'chandir.recv)
    (chandir.send     'chandir.send)
    (chandir.both     'chandir.both))
    
(const
    (kind.invalid    cl:nil)
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


; for simplicity, map false => cl:nil and true => cl:t
(const
    (true       cl:t)
    (false      cl:nil))


(const
    (%cpu-bytes (if (> (integer-length most-positive-fixnum) 32)
                    8
                    4))
    (%cpu-bits (ash %cpu-bytes 3)))

(const-once (%zerov #()))


(deftype kind    () 'symbol)
(deftype chandir () 'symbol)
(deftype int     () '(signed-byte   #.%cpu-bytes))
(deftype uintptr () '(unsigned-byte #.%cpu-bytes))
(deftype string  () 'cl:string)

(defstruct (type.go (:conc-name type-))
  (kind       cl:nil :type kind)
  (size       cl:nil :type (or null uintptr))
  (comparable true   :type boolean)
  (cl-type    cl:t   :type (or symbol list)))

(defstruct (type.array (:include type.go))
  (count      0      :type int)
  (elem       cl:nil :type (or null type.go)))

(defstruct (type.basic (:include type.go))
  (name       (error "gobasic: missing name") :type string))

(defstruct (type.chan (:include type.go))
  (dir  chandir.both :type chandir)
  (elem       cl:nil :type (or null type.go)))

(defstruct (type.func (:include type.go))
  (recv       cl:nil :type (or null type.go))
  (params     %zerov :type simple-vector)     ;; array of type.go
  (results    %zerov :type simple-vector))    ;; array of type.go

(defstruct (type.interface (:include type.go))
  (embeddeds        %zerov :type simple-vector)  ;; array of type.go
  (explicit-methods %zerov :type simple-vector)  ;; array of gofunc
  (methods          %zerov :type simple-vector)) ;; array of gofunc

(defstruct (type.map (:include type.go))
  (key        cl:nil :type (or null type.go))
  (elem       cl:nil :type (or null type.go)))

(defstruct (type.named (:include type.go))
  (name       (error "gonamed: missing name") :type string)
  (underlying cl:nil :type (or null type.go))
  (methods    %zerov :type simple-vector)) ;; array of gofunc

(defstruct (type.ptr (:include type.go))
  (elem       cl:nil :type (or null type.go)))

(defstruct (type.slice (:include type.go))
  (elem       cl:nil :type (or null type.go)))

(defstruct (type.struct (:include type.go))
  (fields     %zerov    :type simple-vector)) ;; array of gostructfield



(defstruct gostructfield
  (name       ""     :type string)
  (pkgpath    ""     :type string)
  (type       (error "gostructfield: missing type") :type type.go)
  (offset     0      :type uintptr)
  (index      0      :type int))



(defstruct goscope
  (objects    (make-hash-table :test 'equal) :type hash-table)
  (types      (make-hash-table :test 'equal) :type hash-table)
  (parent     cl:nil :type (or null goscope)))

(defstruct (gopackage (:include goscope))
  (name       (error "gopackage: missing name") :type string)
  (path       (error "gopackage: missing path") :type string))




;; base type of goconst, gofunc, govar
(defstruct goobject
  (name       ""     :type string)
  (type       (error "goobject: missing type") :type (or null type.go))
  (parent     cl:nil :type (or null goscope)))

(defstruct (goconst (:include goobject))
  (value      (error "goconst: missing value") :type cl:t))

(defstruct (govar   (:include goobject)))

(defstruct (gofunc  (:include goobject)))

