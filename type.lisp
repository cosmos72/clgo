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
    (kind_invalid     nil)
    (kind_bool       'kind_bool)

    (kind_int        'kind_int)
    (kind_int8       'kind_int8)
    (kind_int16      'kind_int16)
    (kind_int32      'kind_int32)
    (kind_int64      'kind_int64)
    (kind_uint       'kind_uint)
    (kind_uint8      'kind_uint8)
    (kind_uint16     'kind_uint16)
    (kind_uint32     'kind_uint32)
    (kind_uint64     'kind_uint64)
    (kind_uintptr    'kind_uintptr)

    (kind_float32    'kind_float32)
    (kind_float64    'kind_float64)
    (kind_complex64  'kind_complex64)
    (kind_complex128 'kind_complex128)
    (kind_string     'kind_string)
    
    (kind_array      'kind_array)
    (kind_chan       'kind_chan)
    (kind_func       'kind_func)
    (kind_interface  'kind_interface)
    (kind_map        'kind_map)
    (kind_ptr        'kind_ptr)
    (kind_slice      'kind_slice)
    (kind_struct     'kind_struct))


; for simplicity, map false => nil and true => cl:t
(const
    (true       cl:t)
    (false      nil))


(const
    (_cpu_bytes (if (> (integer-length most-positive-fixnum) 32) 8 4))
    (_cpu_bits (ash _cpu_bytes 3)))


(const-once (%[] #()))


(deftype kind       () 'symbol)
(deftype chandir    () 'symbol)
(deftype int        () '(signed-byte    #._cpu_bits))
(deftype int8       () '(signed-byte    8))
(deftype int16      () '(signed-byte    16))
(deftype int32      () '(signed-byte    32))
(deftype int64      () '(signed-byte    64))
(deftype uint       () '(unsigned-byte  #._cpu_bits))
(deftype uint8      () '(unsigned-byte  8))
(deftype uint16     () '(unsigned-byte  16))
(deftype uint32     () '(unsigned-byte  32))
(deftype uint64     () '(unsigned-byte  64))
(deftype uintptr    () '(unsigned-byte  #._cpu_bits))
(deftype bool       () 'boolean)
(deftype float32    () 'single-float)
(deftype float64    () 'double-float)
(deftype complex64  () '(complex single-float))
(deftype complex128 () '(complex double-float))
(deftype string     () 'cl:string)

(defstruct (type.go (:conc-name type_))
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


;; represents type of untyped constants: 
;; either bool, rune, int, float64, complex128 or string
(defstruct (untyped (:include type.basic)))




(defstruct reflect.field
  (name       ""     :type string)
  (pkgpath    ""     :type string)
  (type       (error "reflect.field: missing type") :type type.go)
  (offset     0      :type uintptr)
  (index      0      :type int))



(defstruct goscope
  (objs       (make-hash-table :test 'eq) :type hash-table)
  (types      (make-hash-table :test 'eq) :type hash-table)
  (parent     nil :type (or null goscope)))

(defstruct (goscope.file (:include goscope))
  (path       (error "goscope.file: missing path") :type string))

(defstruct (gopackage (:include goscope))
  (name       (error "gopackage: missing name") :type symbol)
  (path       (error "gopackage: missing path") :type string))

(defstruct gocompiler
  (scope      (error "gocompiler: missing scope") :type goscope))



;; base type of goconst, gofunc, govar
(defstruct goobj
  (name       nil    :type symbol)
  (type       (error "goobj: missing type") :type (or null type.go))
  (scope      nil    :type (or null goscope)))

(defstruct (goconst (:include goobj))
  (value      (error "goconst: missing value") :type cl:t))

(defstruct (govar   (:include goobj)))

(defstruct (gofunc  (:include goobj)))

