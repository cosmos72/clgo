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

(const_once
 (bool    (type_basic 'bool    kind_bool   1            'boolean))
 (int     (type_basic 'int     kind_int    _cpu_bytes  '(signed-byte #._cpu_bits)))
 (int8    (type_basic 'int8    kind_int8   1           '(signed-byte 8)))
 (int16   (type_basic 'int16   kind_int16  2           '(signed-byte 16)))
 (int32   (type_basic 'int32   kind_int32  4           '(signed-byte 32)))
 (int64   (type_basic 'int64   kind_int64  8           '(signed-byte 64)))
 (uint    (type_basic 'uint    kind_uint   _cpu_bytes  '(unsigned-byte #._cpu_bits)))
 (uintptr (type_basic 'uintptr kind_int64  _cpu_bytes  '(unsigned-byte #._cpu_bits)))
 (uint8   (type_basic 'uint8   kind_int8   1           '(unsigned-byte 8)))
 (uint16  (type_basic 'uint16  kind_int16  2           '(unsigned-byte 16)))
 (uint32  (type_basic 'uint32  kind_int32  4           '(unsigned-byte 32)))
 (uint64  (type_basic 'uint64  kind_int64  8           '(unsigned-byte 64)))
 (byte       uint8)
 (rune       uint32)
 (float32    (type_basic 'float32    kind_float32    4    'single-float))
 (float64    (type_basic 'float64    kind_float64    8    'double-float))
 (complex64  (type_basic 'complex64  kind_complex64  8   '(complex single-float)))
 (complex128 (type_basic 'complex128 kind_complex128 16  '(complex double-float)))
 (string     (type_basic 'string     kind_string (* 2 _cpu_bytes) 'cl:string))
 (error      (type_named
              'error
              (type_interface
               nil
               (vector (gofunc '|Error| (type_func nil (vector string) false false))))
              nil))

 (untyped_bool    (untyped 'bool       kind_bool       'boolean))
 (untyped_rune    (untyped 'rune       kind_int32      '(unsigned-byte 32)))
 (untyped_int     (untyped 'int        kind_int        'integer))
 (untyped_float   (untyped 'float64    kind_float64    'rational))
 (untyped_complex (untyped 'complex128 kind_complex128 '(complex rational))))
                         


(var (*universe*
      (let ((pkg (new gopackage :name '*universe* :path "")))
        (decl_objs pkg
                   (goconst 'false untyped_bool   false)
                   (goconst 'true  untyped_bool   true)
                   (goconst 'nil   nil            nil))
        (decl_types pkg
                    int  int8  int16  int32  int64
                    uint uint8 uint16 uint32 uint64 uintptr
                    float32 float64 complex64 complex128
                    string error)
        ;; add type aliases: byte and rune
        (let ((types (-> pkg types)))
          (map! types 'byte byte)
          (map! types 'rune rune))

        pkg)))


(func gopackage ((name symbol) (path string)) (gopackage)
  (return (new gopackage :name name :path path :parent *universe*)))

(func gocompiler ((scope goscope)) (gocompiler)
  (return (new gocompiler :scope scope)))

(func repl_gocompiler () (gocompiler)
  (return (gocompiler (gofile "repl.go" (gopackage 'main "main")))))



;; calls (gopackage) which requires *universe*
(var (*compiler* (repl_gocompiler)))
