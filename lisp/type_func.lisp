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

(func check-type_go ((typ any)) (type_go)
  (assert (typep typ 'type_go))
  (return typ))

(func check-types_go ((types (opt simple-vector))) (simple-vector)
  (when types
    (every #'check-type_go types))
  (return (or types %[])))



(func check-type_interface ((typ any)) (type_go)
  (etypecase typ
    (type_named (let ((u (-> typ underlying)))
                  (when u
                    (assert (typep u 'type_interface)))))
    (type_interface))
  (return typ))

(func check-types_interface ((types (opt simple-vector))) (simple-vector)
  (when types
    (every #'check-type_interface types))
  (return (or types %[])))



(func check-method ((method any)) (goobj)
  (assert (typep method 'goobj))
  (assert (eq gofunc (-> method kind)))
  (return method))

(func check-methods ((methods (opt simple-vector))) (simple-vector)
  (when methods
    (every #'check-method methods))
  (return (or methods %[])))


(func check-field ((field any)) (field)
  (assert (typep field 'field))
  (return field))

(func check-fields ((fields (opt simple-vector))) (simple-vector)
  (when fields
    (every #'check-field fields))
  (return (or fields %[])))



(func type_array ((count int) (elem type_go)) (type_array)
  (return
    (new type_array :kind kind_array
                    :comparable (-> elem comparable)
                    :cltype 'simple-vector
                    :count count :elem elem)))

(func type_basic
    ((name symbol) (kind kind) (size uintptr) (cltype (or symbol cons)))
    (type_basic)
  (return
    (new type_basic :kind kind :size size :cltype cltype
                    :name name)))

(func type_chan ((dir chandir) (elem type_go)) (type_chan)
  (return (new type_chan :kind kind_chan #| TODO :cltype |#
                         :dir dir :elem elem)))

(func type_func
    ((params (opt simple-vector)) (results (opt simple-vector))
     (recv bool) (variadic bool))
    (type_func)
  (let ((params  (check-types_go params))
        (results (check-types_go results)))
    (return (new type_func :kind kind_func :comparable false
                           :size _cpu_bytes :cltype 'function  
                           :params params :results results
                           :recv recv :variadic variadic))))

(func type_interface
    ((embeddeds (opt simple-vector)) (methods (opt simple-vector)))
    (type_interface)
  (let ((embeddeds (check-types_interface embeddeds))
        (methods   (check-methods methods)))
    (return
      (new type_interface :kind kind_interface #| TODO :cltype |#
                          :embeddeds         embeddeds
                          :explicit-methods  methods
                          :methods           methods)))) #| TODO compute |#

(func type_map ((key type_go) (elem type_go)) (type_map)
  (new type_map :kind kind_map :comparable false
                :size _cpu_bytes :cltype 'hash-table
                :key key :elem elem))

(func type_named
    ((name symbol) (underlying (opt type_go)) (methods (opt simple-vector)))
    (type_named)
  (let ((u underlying)
        (methods (check-methods methods)))
    (if underlying
        (return
          (new type_named :kind (-> u kind) :size (-> u size)
                          :comparable (-> u comparable) :cltype (-> u cltype)
                          :name name :underlying u :methods methods))
        (return
          (new type_named :kind kind_invalid :comparable false
                          :name name :methods methods)))))

(func type_ptr ((elem type_go)) (type_ptr)
  (return (new type_ptr :kind kind_ptr #| TODO :cltype |#
                        :elem elem)))

(func type_slice ((elem type_go)) (type_slice)
  (return (new type_slice :kind kind_slice :comparable false #| TODO :cltype |#
                          :elem elem)))


(func untyped ((name symbol) (kind kind) (cltype (or symbol cons))) (untyped)
  (return (new untyped :kind kind :cltype cltype :name name)))


(func field.size ((field field)) ((opt uintptr))
  (return (-> field type size)))

(func fields.size ((fields (opt simple-vector))) ((opt uintptr))
  (let ((fields (check-fields fields))
        (ret 0))
    (range for (_ field) :vector fields
      (let ((size (-> field size)))
        (if size
            (+= ret size)
            (return nil))))
    (return ret)))



(func type_struct ((fields (opt simple-vector))) (type_struct)
  (let ((fields (check-fields fields)))
    (new type_struct :kind kind_struct
                     :size (fields.size fields)
                     :cltype 'simple-vector
                     :comparable (every (lambda (field)
                                          (-> field type comparable))
                                        fields)
                     :fields fields)))




(func goscope ((parent_scope (opt goscope))) (goscope)
  (new goscope :kind goscope :parent parent_scope))

(func gofile ((path string) (pkg goscope)) (goscope)
  (new goscope :kind gofile :parent pkg :path path))




(func goconst ((name symbol) (typ (opt type_go)) (value any)) (goobj)
  ;; constant 'nil has nil type, thus allow typ == nil
  (new goobj :kind goconst :name name :type typ :value value))

(func govar ((name symbol) (typ type_go)) (goobj)
  (new goobj :kind govar :name name :type typ))

(func gofunc ((name symbol) (typ type_go)) (goobj)
  (new goobj :kind gofunc :name name :type typ))




(func scope.decl_obj ((scope goscope) (obj goobj)) ()
  (setf (-> obj scope) scope)
  (map! (-> scope objs) (-> obj name) obj)
  (return))

(func scope.decl_type ((scope goscope) (name symbol) (typ type_go)) ()
  (map! (-> scope types) name typ)
  (return))

(defun scope.decl_types (scope &rest types)
  (declare (cl:type goscope scope))
  (dolist (typ types)
    (scope.decl_type scope (-> typ name) typ))
  (values))

(defun scope.decl_objs (scope &rest objs)
  (declare (cl:type goscope scope))
  (dolist (obj objs)
    (scope.decl_obj scope obj))
  (values))





(defmethod print-object ((typ type_go) stream)
  (format stream "#<~S>" (type-of typ)))

(defmethod print-object ((typ type_basic) stream)
  (format stream "#<~S ~S>" (type-of typ) (-> typ name)))

(defmethod print-object ((typ type_named) stream)
  (format stream "#<~S ~S ~S>" (type-of typ) (-> typ name)
          (-> typ kind)))


(defmethod print-object ((obj goobj) stream)
  (if (eq goconst (-> obj kind))
      (format stream "#<~S ~S ~S ~S ~S ~S ~S ~S ~S>" (-> obj kind)
              :name  (-> obj name)
              :type  (-> obj type)
              :value (-> obj value)
              :scope (if (-> obj scope) true false))
      (format stream "#<~S ~S ~S ~S ~S ~S ~S>" (-> obj kind)
          :name  (-> obj name)
          :type  (-> obj type)
          :scope (if (-> obj scope) true false))))


(defmethod print-object ((scope goscope) stream)
  (case (-> scope kind)
    (gopackage
     (format stream "#<~S ~S ~S ~S ~S ~S ~S ~S ~S>" (-> scope kind)
          :name       (-> scope name)
          :path       (-> scope path)
          :obj_count  (map.len (-> scope objs))
          :type_count (map.len (-> scope types))))
    (gofile
     (format stream "#<~S ~S ~S ~S ~S ~S ~S>" (-> scope kind)
             :path       (-> scope path)
             :obj_count  (map.len (-> scope objs))
             :type_count (map.len (-> scope types))))
    (otherwise
     (format stream "#<~S ~S ~S ~S ~S ~S ~S>" (-> scope kind)
             :obj_count  (map.len (-> scope objs))
             :type_count (map.len (-> scope types))
             :parent     (if (-> scope parent) true false)))))
