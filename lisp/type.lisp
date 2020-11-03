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




(const
    (_cpu_bytes (if (> (integer-length most-positive-fixnum) 32) 8 4))
    (_cpu_bits (ash _cpu_bytes 3)))



(type kind       symbol)
(type chandir    symbol)
(type int        (signed-byte    #._cpu_bits))
(type int8       (signed-byte    8))
(type int16      (signed-byte    16))
(type int32      (signed-byte    32))
(type int64      (signed-byte    64))
(type uint       (unsigned-byte  #._cpu_bits))
(type uint8      (unsigned-byte  8))
(type uint16     (unsigned-byte  16))
(type uint32     (unsigned-byte  32))
(type uint64     (unsigned-byte  64))
(type uintptr    (unsigned-byte  #._cpu_bits))
(type bool       boolean)
(type float32    single-float)
(type float64    double-float)
(type complex64  (complex single-float))
(type complex128 (complex double-float))
(type string     cl:string)

(deftype opt (typ) `(or null ,typ))
(type cltype (or symbol list))

(type type_go (struct
               (kind       kind)
               (size       (opt uintptr))
               (comparable bool          true)
               (cltype     cltype)))

(type type_array (struct
                  (type_go)
                  (count      int)
                  (elem       (opt type_go))))

(type type_basic (struct
                  (type_go)
                  (name       symbol)))

(type type_chan (struct
                 (type_go)
                 (dir        chandir)
                 (elem       (opt type_go))))

(type type_func (struct
                 (type_go)
                 (params     simple-vector)    ;; array of type_go
                 (results    simple-vector)  ;; array of type_go
                 (recv       bool)
                 (variadic   bool)))

(type type_interface (struct
                      (type_go)
                      (embeddeds        simple-vector)   ;; array of type_go
                      (explicit-methods simple-vector)   ;; array of gofunc
                      (methods          simple-vector))) ;; array of gofunc

(type type_map (struct
                (type_go)
                (key        type_go)
                (elem       type_go)))

(type type_named (struct
                  (type_go)
                  (name       symbol)
                  (underlying (opt type_go))
                  (methods    simple-vector))) ;; array of gofunc

(type type_ptr (struct
                (type_go)
                (elem        type_go)))

(type type_slice (struct
                  (type_go)
                  (elem      type_go)))

(type type_struct (struct
                   (type_go)
                   (fields     simple-vector))) ;; array of field


;; represents type of untyped constants: 
;; either bool, rune, int, float64, complex128 or string
(defclass untyped (type_basic) ())


(type field (struct
             (name       string)
             (pkgpath    string)
             (type       type_go)
             (offset     uintptr)
             (index      int)))

(const
  (goconst   'goconst)
  (gofunc    'gofunc)
  (govar     'govar)

  (goscope   'goscope)
  (gofile    'gofile)
  (gopackage 'gopackage))


;; represents goscope, gofile, gopackage
(type goscope (struct
               (kind       symbol) ;; one of goscope gofile gopackage
               (objs       (map symbol goobj)   (make_map :test 'eq))
               (types      (map symbol type_go) (make_map :test 'eq))
               (parent     (opt goscope))
               (name       symbol)   ;; set only if kind in (gofile gopackage)
               (path       string))) ;; set only if kind in (gofile gopackage)

;; represents goconst, gofunc, govar
(type goobj (struct
             (kind       symbol) ;; one of: goconst gofunc govar
             (name       symbol)
             (type       (opt type_go))
             (value      any)
             (scope      (opt goscope))))

