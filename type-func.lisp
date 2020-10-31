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

(func check-type.go ((typ _)) (type.go)
  (assert (typep typ 'type.go))
  typ)

(func check-types.go ((types (or null simple-vector))) (simple-vector)
  (when types
    (every #'check-type.go types))
  (or types %[]))



(func check-type.interface ((typ _)) (type.go)
  (etypecase typ
    (type.named (let ((u (type.named-underlying typ)))
                  (when u
                    (assert (typep u 'type.interface)))))
    (type.interface))
  typ)

(func check-types.interface ((types (or null simple-vector))) (simple-vector)
  (when types
    (every #'check-type.interface types))
  (or types %[]))



(func check-method ((method _)) (gofunc)
  (assert (typep method 'gofunc))
  method)

(func check-methods ((methods (or null simple-vector))) (simple-vector)
  (when methods
    (every #'check-method methods))
  (or methods %[]))


(func check-structfield ((field _)) (reflect.field)
  (assert (typep field 'reflect.field))
  field)

(func check-structfields ((fields (or null simple-vector))) (simple-vector))
  (when fields
    (every #'check-structfield fields))
  (or fields %[]))



(func type.array ((count int) (elem type.go)) (type.array)
  (make-type.array :kind kind.array
                   :comparable (type-comparable elem)
                   :cl-type 'simple-vector
                   :cl-slots (let ((elem-slots (type-cl-slots elem)))
                               (if (null elem-slots)
                                   nil
                                   (* count elem-slots)))
                   :count count :elem elem))

(func type.basic ((name symbol) (kind kind) (size uintptr) (cl-type (or symbol cons))) (type.basic)
  (make-type.basic :kind kind :size size :cl-type cl-type
                   :name name))

(func type.chan ((dir chandir) (elem type.go)) (type.chan)
  (make-type.chan :kind kind.chan #| TODO :cl-type |#
                  :dir dir :elem elem))

(func type.func ((recv (or null type.go)) (params (or null simple-vector)) (results (or null simple-vector))) (type.func)
    (make-type.func :kind kind.func :comparable false #| TODO :cl-type |#
                    :recv recv :params params :results results)))

(func type.interface ((embeddeds (or null simple-vector)) (methods (or null simple-vector))) (type.interface)
  (let ((embeddeds (check-types.interface embeddeds))
        (methods   (check-methods methods)))
    (make-type.interface :kind kind.interface #| TODO :cl-type |#
                         :embeddeds         embeddeds
                         :explicit-methods  methods
                         :methods           methods))) #| TODO compute |#

(func type.map ((key type.go) (elem type.go)) (type.map)
  (make-type.map :kind kind.map :comparable false :cl-type 'hash-table
                 :key key :elem elem))

(func type.named ((name symbol) (underlying (or null type.go)) (methods (or null simple-vector))) (type.named)
  (let ((u underlying)
        (methods (check-methods methods)))
    (if underlying
        (make-type.named :kind (type-kind u) :size (type-size u)
                         :comparable (type-comparable u) :cl-type (type-cl-type u)
                         :name name :underlying u :methods methods)
        (make-type.named :kind kind.invalid :comparable false
                         :name name :methods methods))))

(func type.ptr ((elem type.go)) (type.ptr)
  (make-type.ptr :kind kind.ptr #| TODO :cl-type |#
                 :elem elem))

(func type.slice ((elem type.go)) (type.slice)
  (make-type.slice :kind kind.slice :comparable false #| TODO :cl-type |#
                   :elem elem))


(func untyped ((name symbol) (kind kind) (cl-type (or symbol cons))) (untyped)
  (make-untyped :kind kind :cl-type cl-type :name name))


(func reflect.field-size ((field reflect.field)) ((or null uintptr))
  (type-size (reflect.field-type field)))

(func reflect.fields-size ((fields (or null simple-vector))) (or null uintptr)
  (let ((fields (check-structfields fields))
        (size 0))
    (loop for field across fields
          with field-size = (reflect.field-size field)
          do
             (if (null field-size)
                 (return-from reflect.fields-size nil)
                 (incf size field-size)))
    size))



(func reflect.field-cl-slots ((field reflect.field)) ((or null uintptr))
  (type-cl-slots (reflect.field-type field)))

(func reflect.fields-cl-slots (fields &optional (initial-n-slots 0))
  (declare (type (or null simple-vector) fields)
           (type uintptr initial-n-slots))
  (let ((fields (check-structfields fields))
        (n-slots initial-n-slots))
    (loop for field across fields
          with field-n-slots = (reflect.field-cl-slots field)
          do
             (if (null field-n-slots)
                 (return-from reflect.fields-cl-slots nil)
                 (incf n-slots field-n-slots)))
    n-slots))



(func type.struct ((fields (or null simple-vector))) (type.struct)
  (let ((fields (check-structfields fields)))
    (make-type.struct :kind kind.struct
                      :size (reflect.fields-size fields)
                      :cl-type 'simple-vector
                      ;; 1 initial slot contains struct type. useful for debugging
                      :cl-slots (reflect.fields-cl-slots fields 1)
                      :comparable (every (lambda (field)
                                           (type-comparable (reflect.field-type field)))
                                         fields)
                      :fields fields)))


(func type-name ((typ type.go)) (symbol)
  (etypecase typ
    (type.basic (type.basic-name typ))
    (type.named (type.named-name typ))))



(func goscope ((parent-scope (or null goscope))) (goscope)
  (make-goscope :parent parent-scope))

(func goscope.file ((path string) (pkg gopackage)) (goscope.file)
  (make-goscope.file :path path :parent pkg))




(func goconst (name typ value &optional scope)
  (declare (type symbol            name)
           (type (or null type.go) typ) ;; constant 'nil has nil type
           (type cl:t              value)
           (type (or null goscope) scope))
  (make-goconst :name name :type typ :value value :scope scope))

(func govar (name typ &optional scope)
  (declare (type symbol     name)
           (type type.go    typ)
           (type (or null goscope) scope))
  (make-govar :name name :type typ :scope scope))

(func gofunc (name typ &optional scope)
  (declare (type symbol     name)
           (type type.go    typ)
           (type (or null goscope) scope))
  (make-gofunc :name name :type typ :scope scope))




(func make-types (&rest types)
  (let ((h (make-hash-table :test 'eq)))
    (loop for typ of-type type.go in types
          do (htable! h (type-name typ) typ))
    (the hash-table h)))

(func make-goobjs (&rest objs)
  (let ((h (make-hash-table :test 'eq)))
    (loop for obj of-type goobj in objs
          do (htable! h (goobj-name obj) obj))
    (the hash-table h)))





(defmethod print-object ((typ type.go) stream)
  (format stream "#<~S>" (type-of typ)))

(defmethod print-object ((typ type.basic) stream)
  (format stream "#<~S ~S>" (type-of typ) (type.basic-name typ)))

(defmethod print-object ((typ type.named) stream)
  (format stream "#<~S ~S ~S>" (type-of typ) (type.named-name typ)
          (type-kind typ)))


(defmethod print-object ((obj goobj) stream)
  (format stream "#<~S ~S ~S ~S ~S ~S ~S>"
          (type-of obj)
          :name
          (goobj-name obj)
          :type
          (goobj-type obj)
          :scope
          (let ((scope (goobj-scope obj)))
            (if scope true false))))

(defmethod print-object ((obj goconst) stream)
  (format stream "#<~S ~S ~S ~S ~S ~S ~S ~S ~S>"
          (type-of obj)
          :name
          (goobj-name obj)
          :type
          (goobj-type obj)
          :value
          (goconst-value obj)
          :scope
          (let ((scope (goobj-scope obj)))
            (if scope true false))))

(defmethod print-object ((scope goscope) stream)
  (format stream "#<~S ~S ~S ~S ~S ~S ~S>"
          (type-of scope)
          :obj-count
          (hash-table-count (goscope-objs scope))
          :type-count
          (hash-table-count (goscope-types scope))
          :parent
          (let ((parent (goscope-parent scope)))
            (if parent true false))))

(defmethod print-object ((scope.file goscope.file) stream)
  (format stream "#<~S ~S ~S ~S ~S ~S ~S>"
          (type-of scope.file)
          :path
          (goscope.file-path scope.file)
          :obj-count
          (hash-table-count (goscope-objs scope.file))
          :type-count
          (hash-table-count (goscope-types scope.file))))

(defmethod print-object ((pkg gopackage) stream)
  (format stream "#<~S ~S ~S ~S ~S ~S ~S ~S ~S>"
          (type-of pkg)
          :name
          (gopackage-name pkg)
          :path
          (gopackage-path pkg)
          :obj-count
          (hash-table-count (goscope-objs pkg))
          :type-count
          (hash-table-count (goscope-types pkg))))
