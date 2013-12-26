;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2013 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(asdf:defsystem #:cl-prj-notify
  :description "Tool to send notifications to Common Lisp projects."
  :license "BSD"
  :author "Anton Vodonosov <avodonosov@yandex.ru>"
  :version "0.0.1"
  :depends-on (#:drakma
               #:cl-json
               #:babel)
  :serial t
  :components
  ((:static-file "README.md")
   (:file "cl-prj-notify")
   (:static-file "list-github-projects.lisp")
   (:static-file "github-projects.sexp")
   (:static-file "LICENSE")))
