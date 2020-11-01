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

(const-once
 (bool    (type.basic 'bool    kind.bool   1            'boolean))
 (int     (type.basic 'int     kind.int    %cpu-bytes  '(signed-byte #.%cpu-bits)))
 (int8    (type.basic 'int8    kind.int8   1           '(signed-byte 8)))
 (int16   (type.basic 'int16   kind.int16  2           '(signed-byte 16)))
 (int32   (type.basic 'int32   kind.int32  4           '(signed-byte 32)))
 (int64   (type.basic 'int64   kind.int64  8           '(signed-byte 64)))
 (uint    (type.basic 'uint    kind.uint   %cpu-bytes  '(unsigned-byte #.%cpu-bits)))
 (uintptr (type.basic 'uintptr kind.int64  %cpu-bytes  '(unsigned-byte #.%cpu-bits)))
 (uint8   (type.basic 'uint8   kind.int8   1           '(unsigned-byte 8)))
 (uint16  (type.basic 'uint16  kind.int16  2           '(unsigned-byte 16)))
 (uint32  (type.basic 'uint32  kind.int32  4           '(unsigned-byte 32)))
 (uint64  (type.basic 'uint64  kind.int64  8           '(unsigned-byte 64)))
 (byte       uint8)
 (rune       uint32)
 (float32    (type.basic 'float32    kind.float32    4    'single-float))
 (float64    (type.basic 'float64    kind.float64    8    'double-float))
 (complex64  (type.basic 'complex64  kind.complex64  8   '(complex single-float)))
 (complex128 (type.basic 'complex128 kind.complex128 16  '(complex double-float)))
 (string     (type.basic 'string     kind.string (* 2 %cpu-bytes) 'cl:string))
 (error      (type.named
              'error
              (type.interface
               nil
               (vector (gofunc '|Error| (type.func nil nil (vector string)))))
              nil))

 (untyped.bool    (untyped 'bool       kind.bool       'boolean))
 (untyped.rune    (untyped 'rune       kind.int32      '(unsigned-byte 32)))
 (untyped.int     (untyped 'int        kind.int        'integer))
 (untyped.float   (untyped 'float64    kind.float64    'rational))
 (untyped.complex (untyped 'complex128 kind.complex128 '(complex rational))))
                         


(var (*universe*
      (let* ((pkg (make-gopackage :name '*universe* :path ""))
             (objs (make-goobjs
                    pkg
                    (goconst 'false untyped.bool   false)
                    (goconst 'true  untyped.bool   true)
                    (goconst 'nil   nil            nil)))
             (types (make-types int  int8  int16  int32  int64
                                uint uint8 uint16 uint32 uint64 uintptr
                                float32 float64 complex64 complex128
                                string error)))
        ;; add type aliases: byte and rune
        (htable! types 'byte byte)
        (htable! types 'rune rune)

        (setf (gopackage-objs  pkg) objs
              (gopackage-types pkg) types)
        pkg)))


(func gopackage ((name symbol) (path string)) (gopackage)
  (make-gopackage :name name :path path :parent *universe*))

(func gocompiler ((scope goscope)) (gocompiler)
  (make-gocompiler :scope scope))

(func new-gocompiler () (gocompiler)
  (gocompiler (goscope.file "repl.go" (gopackage 'main "main"))))



;; calls (gopackage) which requires *universe*
(var (*compiler* (new-gocompiler)))
