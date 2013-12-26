;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2013 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(defpackage #:list-github-projects
  (:use :cl))

(in-package #:list-github-projects)

(ql:quickload :optima)
(ql:quickload :optima.ppcre)
(ql:quickload :alexandria)

;; copied from quicklisp-controller
(defun pathname-project-name (pathname)
  (first (last (pathname-directory pathname))))

;;; To fetch the database we did:
;;;
;;; git clone https://github.com/quicklisp/quicklisp-projects.git
;;; (list-github-projects "github-projects/")

(defun list-github-projects (quicklisp-projects-dir)
  (let ((results nil))
    (dolist (source-file (directory (merge-pathnames "**/source.txt" quicklisp-projects-dir)))
      (optima:match (alexandria:read-file-into-string source-file)
        ((optima.ppcre:ppcre "git.*/github.com/(.*)/(.*).git" repo-owner repo)
         (push (list (pathname-project-name source-file)
                     repo-owner repo)
               results))))
    (nreverse results)))

(defun write-to-file (obj file)
  "Write to file the lisp object OBJ in a format acceptable to READ."
  (with-standard-io-syntax
    (with-open-file (out file
                         :direction :output
                         :element-type 'character
                         :external-format asdf:*utf-8-external-format*
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (pprint obj out)))
  obj)

#|
  git clone https://github.com/quicklisp/quicklisp-projects.git
|#

(write-to-file (list-github-projects "/Users/anton/projects/quicklisp/quicklisp-projects/")
               "/Users/anton/projects/cl-prj-notify/github-projects.sexp")