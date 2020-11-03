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

(type gocompiler (struct
                  (scope      goscope)
                  (vm         vm)))

(func gocompiler ((parent gocompiler)) (gocompiler)
  (return (new gocompiler :scope (goscope (-> parent scope)) :vm (-> parent vm))))

(func repl_gocompiler () (gocompiler)
  (return (new gocompiler
               :scope (gofile "repl.go" (gopackage 'main "main"))
               :vm (new vm))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(func scope.find_obj ((scope (or null goscope)) (name symbol)) ((or null goobj))
  (let ((sc scope))
    (loop while sc do
      (let* ((objs (-> sc objs))
             (obj  (map@ objs name)))
        (if obj
            (return obj)
            (setf sc (-> sc parent))))))
  (return nil))

(func scope.find_type ((scope (or null goscope)) (name symbol)) ((or null type_go))
  (let ((sc scope))
    (loop while sc do
      (let* ((types (-> sc types))
             (typ   (map@ types name)))
        (if typ
            (return typ)
            (setf sc (-> sc parent))))))
  (return nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(func find_obj ((gc gocompiler) (name symbol)) ((or null goobj))
  (return (scope.find_obj (-> gc scope) name)))

(func find_type ((gc gocompiler) (name symbol)) ((or null type_go))
  (return (find_type (-> gc scope) name)))

(func decl_obj ((gc gocompiler) (obj goobj)) ()
  (scope.decl_obj (-> gc scope) obj))

(func decl_type ((gc gocompiler) (name symbol) (typ type_go)) ()
  (scope.decl_type (-> gc scope) name typ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun compile.tokens (gc &rest tokens)
  (declare (cl:type gocompiler gc))
  (apply #'vm.tokens (-> gc vm) tokens))

(func compile.int ((gc gocompiler) (value int)) ()
  (vm.int (-> gc vm) value))

(declaim (ftype (function (gocompiler any) (values &optional)) compile.any))

(func compile.const ((gc gocompiler) (c atom)) ()
  (return))

(func compile.sym ((gc gocompiler) (sym symbol)) ()
  (let ((obj (find_obj gc sym)))
    (ecase (-> obj kind)
      (goconst)
      (gofunc)
      (govar))
    (return)))

(func compile.tree ((gc gocompiler) (ast cons)) ()
  (return))

(func compile.any ((gc gocompiler) (ast any)) ()
  "compile given AST in current scope."
  (cond
    ((consp ast)     (compile.tree  gc (the cons ast)))
    ((constantp ast) (compile.const gc ast))
    ((symbolp ast)   (compile.sym   gc (the symbol ast)))
    (true            (error "~S: unsupported AST ~S: ~S" 'compile (type-of ast) ast)))
  (return))
     
