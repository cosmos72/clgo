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


(const_once (%[] #()))


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


(defstruct (type_go (:constructor new_type_go)  (:conc-name type.))
  (kind       nil  :type kind)
  (size       nil  :type (or null uintptr))
  (comparable true :type boolean)
  (cltype     nil  :type (or symbol list))
  ;; number of simple-vector slots needed by this type
  (clslots    1    :type (or null uintptr)))

(struct type_array type_go
  (count      int               :init 0)
  (elem       (or null type_go) :init nil))

(struct type_basic type_go
  (name       symbol))

(struct type_chan type_go
  (dir        chandir           :init chandir.both)
  (elem       (or null type_go) :init nil))

(struct type_func type_go
  (recv       (or null type_go) :init nil)
  (params     simple-vector     :init %[])    ;; array of type_go
  (results    simple-vector     :init %[]))   ;; array of type_go

(struct type_interface type_go
  (embeddeds        simple-vector :init %[])  ;; array of type_go
  (explicit-methods simple-vector :init %[])  ;; array of gofunc
  (methods          simple-vector :init %[])) ;; array of gofunc

(struct type_map type_go
  (key        (or null type_go)  :init nil)
  (elem       (or null type_go)  :init nil))

(struct type_named type_go
  (name       symbol)
  (underlying (or null type_go) :init nil)
  (methods    simple-vector     :init %[])) ;; array of gofunc

(struct type_ptr type_go
  (elem       type_go))

(struct type_slice type_go
  (elem       (or null type_go) :init nil))

(struct type_struct type_go
  (fields     simple-vector     :init %[])) ;; array of field


;; represents type of untyped constants: 
;; either bool, rune, int, float64, complex128 or string
(struct untyped type_basic)




(struct field nil
  (name       string  :init "")
  (pkgpath    string  :init "")
  (type       type_go)
  (offset     uintptr :init 0)
  (index      int     :init 0))



(struct goscope nil
  (objs       (map symbol goobj)   :init (make_map :test 'eq))
  (types      (map symbol type_go) :init (make_map :test 'eq))
  (parent     (or null goscope)    :init nil))

(struct gofile goscope
  (path       string))

(struct gopackage goscope
  (name       symbol)
  (path       string))

(struct gocompiler nil
  (scope      goscope))



;; base type of goconst, gofunc, govar
(struct goobj nil
  (name       symbol            :init nil)
  (type       (or null type_go))
  (scope      (or null goscope) :init nil))

(struct goconst goobj
  (value      _))

(struct govar   goobj)

(struct gofunc  goobj)

