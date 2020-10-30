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
 (bool    (type.basic "bool"    kind.bool   1            'boolean))
 (int     (type.basic "int"     kind.int    %cpu-bytes  '(cl:signed-byte #.%cpu-bits)))
 (int8    (type.basic "int8"    kind.int8   1           '(cl:signed-byte 8)))
 (int16   (type.basic "int16"   kind.int16  2           '(cl:signed-byte 16)))
 (int32   (type.basic "int32"   kind.int32  4           '(cl:signed-byte 32)))
 (int64   (type.basic "int64"   kind.int64  8           '(cl:signed-byte 64)))
 (uint    (type.basic "uint"    kind.uint   %cpu-bytes  '(cl:unsigned-byte #.%cpu-bits)))
 (uintptr (type.basic "uintptr" kind.int64  %cpu-bytes  '(cl:unsigned-byte #.%cpu-bits)))
 (uint8   (type.basic "uint8"   kind.int8   1           '(cl:unsigned-byte 8)))
 (uint16  (type.basic "uint16"  kind.int16  2           '(cl:unsigned-byte 16)))
 (uint32  (type.basic "uint32"  kind.int32  4           '(cl:unsigned-byte 32)))
 (uint64  (type.basic "uint64"  kind.int64  8           '(cl:unsigned-byte 64)))
 (byte       uint8)
 (float32    (type.basic "float32"    kind.float32    4    'cl:single-float))
 (float64    (type.basic "float64"    kind.float64    8    'cl:double-float))
 (complex64  (type.basic "complex64"  kind.complex64  8   '(cl:complex cl:single-float)))
 (complex128 (type.basic "complex128" kind.complex128 16  '(cl:complex cl:double-float)))
 (string     (type.basic "string"     kind.string (* 2 %cpu-bytes) 'cl:string))
 (error      (type.named
              "error"
              (type.interface
               nil
               (vector (gofunc "Error" (type.func nil nil (vector string))))))))
                         


(var (*universe*
      (let ((pkg (gopackage "" "")))
        (setf (gopackage-types pkg)
              (make-types int  int8  int16  int32  int64
                          uint uint8 uint16 uint32 uint64 uintptr
                          float32 float64 complex64 complex128
                          bool string error))
        (setf (gopackage-objects pkg)
              (make-goobjects
               (goconst "false" bool   false pkg)
               (goconst "true"  bool   true  pkg)
               (goconst "nil"   nil    nil   pkg)))
        pkg)))

