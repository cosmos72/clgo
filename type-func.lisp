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

(func check-type_go ((typ _)) (type_go)
  (assert (typep typ 'type_go))
  (return typ))

(func check-types_go ((types (or null simple-vector))) (simple-vector)
  (when types
    (every #'check-type_go types))
  (return (or types %[])))



(func check-type_interface ((typ _)) (type_go)
  (etypecase typ
    (type_named (let ((u (type_named.underlying typ)))
                  (when u
                    (assert (typep u 'type_interface)))))
    (type_interface))
  (return typ))

(func check-types_interface ((types (or null simple-vector))) (simple-vector)
  (when types
    (every #'check-type_interface types))
  (return (or types %[])))



(func check-method ((method _)) (gofunc)
  (assert (typep method 'gofunc))
  (return method))

(func check-methods ((methods (or null simple-vector))) (simple-vector)
  (when methods
    (every #'check-method methods))
  (return (or methods %[])))


(func check-field ((field _)) (field)
  (assert (typep field 'field))
  (return field))

(func check-fields ((fields (or null simple-vector))) (simple-vector)
  (when fields
    (every #'check-field fields))
  (return (or fields %[])))



(func type_array ((count int) (elem type_go)) (type_array)
  (return
    (new_type_array :kind kind_array
                     :comparable (type.comparable elem)
                     :cltype 'simple-vector
                     :clslots (let ((elem-slots (type.clslots elem)))
                                 (if (null elem-slots)
                                     nil
                                     (* count elem-slots)))
                     :count count :elem elem)))

(func type_basic
    ((name symbol) (kind kind) (size uintptr) (cltype (or symbol cons)))
    (type_basic)
  (return
    (new_type_basic :kind kind :size size :cltype cltype
                     :name name)))

(func type_chan ((dir chandir) (elem type_go)) (type_chan)
  (return (new_type_chan :kind kind_chan #| TODO :cltype |#
                          :dir dir :elem elem)))

(func type_func
    ((recv (or null type_go)) (params (or null simple-vector)) (results (or null simple-vector)))
    (type_func)
  (let ((params  (check-types_go params))
        (results (check-types_go results)))
    (return (new_type_func :kind kind_func :comparable false #| TODO :cltype |#
                            :recv recv :params params :results results))))

(func type_interface
    ((embeddeds (or null simple-vector)) (methods (or null simple-vector)))
    (type_interface)
  (let ((embeddeds (check-types_interface embeddeds))
        (methods   (check-methods methods)))
    (return
      (new_type_interface :kind kind_interface #| TODO :cltype |#
                           :embeddeds         embeddeds
                           :explicit-methods  methods
                           :methods           methods)))) #| TODO compute |#

(func type_map ((key type_go) (elem type_go)) (type_map)
  (new_type_map :kind kind_map :comparable false :cltype 'hash-table
                 :key key :elem elem))

(func type_named
    ((name symbol) (underlying (or null type_go)) (methods (or null simple-vector)))
    (type_named)
  (let ((u underlying)
        (methods (check-methods methods)))
    (if underlying
        (return
          (new_type_named :kind (type.kind u) :size (type.size u)
                           :comparable (type.comparable u) :cltype (type.cltype u)
                           :name name :underlying u :methods methods))
        (return
          (new_type_named :kind kind_invalid :comparable false
                           :name name :methods methods)))))

(func type_ptr ((elem type_go)) (type_ptr)
  (return (new_type_ptr :kind kind_ptr #| TODO :cltype |#
                         :elem elem)))

(func type_slice ((elem type_go)) (type_slice)
  (return (new_type_slice :kind kind_slice :comparable false #| TODO :cltype |#
                           :elem elem)))


(func untyped ((name symbol) (kind kind) (cltype (or symbol cons))) (untyped)
  (return (new_untyped :kind kind :cltype cltype :name name)))


(func field.size ((field field)) ((or null uintptr))
  (return (type.size (field.type field))))

(func fields.size ((fields (or null simple-vector))) ((or null uintptr))
  (let ((fields (check-fields fields))
        (size 0))
    (loop for field across fields
          for field-size = (field.size field)
          do (if field-size
                 (incf size field-size)
                 (return nil)))
    (return size)))



(func field.clslots ((field field)) ((or null uintptr))
  (type.clslots (field.type field)))

(func fields.clslots
    ((fields (or null simple-vector)) (initial_n_slots uintptr))
    ((or null uintptr))
  (let ((fields (check-fields fields))
        (n-slots initial_n_slots))
    (loop for field across fields
          for field_n_slots = (field.clslots field)
          do
             (if field_n_slots
                 (incf n-slots field_n_slots)
                 (return nil)))
    n-slots))



(func type_struct ((fields (or null simple-vector))) (type_struct)
  (let ((fields (check-fields fields)))
    (new_type_struct :kind kind_struct
                      :size (fields.size fields)
                      :cltype 'simple-vector
                      ;; 1 initial slot contains struct type_ useful for debugging
                      :clslots (fields.clslots fields 1)
                      :comparable (every (lambda (field)
                                           (type.comparable (field.type field)))
                                         fields)
                      :fields fields)))


(func type.name ((typ type_go)) (symbol)
  (etypecase typ
    (type_basic (type_basic.name typ))
    (type_named (type_named.name typ))))



(func goscope ((parent-scope (or null goscope))) (goscope)
  (new_goscope :parent parent-scope))

(func gofile ((path string) (pkg gopackage)) (gofile)
  (new_gofile :path path :parent pkg))




(func goconst ((name symbol) (typ (or null type_go)) (value _)) (goconst)
  ;; constant 'nil has nil type
  (new_goconst :name name :type typ :value value))

(func govar ((name symbol) (typ type_go)) (govar)
  (new_govar :name name :type typ))

(func gofunc ((name symbol) (typ type_go)) (gofunc)
  (new_gofunc :name name :type typ))




(func decl_obj ((scope goscope) (obj goobj)) ()
  (setf (goobj.scope obj) scope)
  (map! (goscope.objs scope) (goobj.name obj) obj)
  (return))

(func decl_type ((scope goscope) (name symbol) (typ type_go)) ()
  (map! (goscope.types scope) name typ)
  (return))

(defun decl_types (scope &rest types)
  (declare (type goscope scope))
  (dolist (typ types)
    (decl_type scope (type.name typ) typ))
  (values))

(defun decl_objs (scope &rest objs)
  (declare (type goscope scope))
  (dolist (obj objs)
    (decl_obj scope obj))
  (values))





(defmethod print-object ((typ type_go) stream)
  (format stream "#<~S>" (type-of typ)))

(defmethod print-object ((typ type_basic) stream)
  (format stream "#<~S ~S>" (type-of typ) (type_basic.name typ)))

(defmethod print-object ((typ type_named) stream)
  (format stream "#<~S ~S ~S>" (type-of typ) (type_named.name typ)
          (type.kind typ)))


(defmethod print-object ((obj goobj) stream)
  (format stream "#<~S ~S ~S ~S ~S ~S ~S>"
          (type-of obj)
          :name
          (goobj.name obj)
          :type
          (goobj.type obj)
          :scope
          (let ((scope (goobj.scope obj)))
            (if scope true false))))

(defmethod print-object ((obj goconst) stream)
  (format stream "#<~S ~S ~S ~S ~S ~S ~S ~S ~S>"
          (type-of obj)
          :name  (goobj.name obj)
          :type  (goobj.type obj)
          :value (goconst.value obj)
          :scope (let ((scope (goobj.scope obj)))
                   (if scope true false))))

(defmethod print-object ((scope goscope) stream)
  (format stream "#<~S ~S ~S ~S ~S ~S ~S>"
          (type-of scope)
          :obj-count  (map.len (goscope.objs scope))
          :type_count (map.len (goscope.types scope))
          :parent     (let ((parent (goscope.parent scope)))
                        (if parent true false))))

(defmethod print-object ((scope.file gofile) stream)
  (format stream "#<~S ~S ~S ~S ~S ~S ~S>"
          (type-of scope.file)
          :path       (gofile.path scope.file)
          :obj-count  (map.len (goscope.objs scope.file))
          :type_count (map.len (goscope.types scope.file))))

(defmethod print-object ((pkg gopackage) stream)
  (format stream "#<~S ~S ~S ~S ~S ~S ~S ~S ~S>"
          (type-of pkg)
          :name       (gopackage.name pkg)
          :path       (gopackage.path pkg)
          :obj-count  (map.len (goscope.objs pkg))
          :type_count (map.len (goscope.types pkg))))
