;;;; -*- Mode: LISP; Syntax: COMMON-LISP; indent-tabs-mode: nil; coding: utf-8; show-trailing-whitespace: t -*-
;;;; Copyright (C) 2013 Anton Vodonosov (avodonosov@yandex.ru)
;;;; See LICENSE for details.

(defpackage :cl-prj-notify
  (:use :cl)
  (:export #:notify-project
           #:*github-personal-token*))

(in-package :cl-prj-notify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utils
;;;

;; copy/paste from
;; http://www.gigamonkeys.com/book/practical-an-mp3-browser.html
(defmacro with-safe-io-syntax (&body body)
  `(with-standard-io-syntax
     (let ((*read-eval* nil))
       ,@body)))

(defun safe-read (&rest args)
  (with-safe-io-syntax (apply #'read args)))

(defparameter *utf-8-external-format* asdf:*utf-8-external-format*
  "See docstring for ASDF:*UTF-8-EXTERNAL-FORMAT*.")

(defun safe-read-file (file)
  (with-open-file (in file
                      :direction :input
                      :element-type 'character
                      :external-format *utf-8-external-format*
                      )
    (safe-read in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database of CL projects hosted on GitHub
;;;

(defparameter *github-projects*
  (safe-read-file (asdf:system-relative-pathname :cl-prj-notify "github-projects.sexp"))
  "Database of CL projects hosted on GitHub.

List of items, each item in the form (<project-name> <github-repository-owner> <github-repository-name>).
All item fields are strings. Project names are the same as Quicklisp uses.

The information is fetched from https://github.com/quicklisp/quicklisp-projects
using the list-quicklisp-projects.lisp script.")

(defun find-github-repo (project-name)
  "If repository for the specified project is found,
returns a list of two elements: github repository owner and github repository name.
Otherwise returns NIL.

PROJECT-NAME is a string, as Quicklisp uses."
  (cdr (assoc project-name *github-projects* :test #'string=)))

#|
(find-github-repo "babel")
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Open GitHub issues via GitHub API
;;;

(defparameter *github-personal-token* nil)

(defun open-github-issue (repo-owner repo title body &key (github-personal-token *github-personal-token*))
  (assert github-personal-token)
  (multiple-value-bind (body-bytes status-code headers uri stream must-close reason-phrase)
      (drakma:http-request (format nil "https://api.github.com/repos/~A/~A/issues" repo-owner repo)
                           :additional-headers `(("Authorization" . ,(format nil "token ~A" github-personal-token)))
                           :method :post
                           :force-binary t
                           :content (cl-json:encode-json-plist-to-string (list :title title :body body)))
    (declare (ignore uri stream must-close))
    (let ((body (when (arrayp body-bytes) (babel:octets-to-string body-bytes))))
      (when (/= status-code 201)
        (error "Error creating github issue. HTTP response: ~A ~A~%~A~%~A" status-code reason-phrase headers body))
      (assert body) ; if HTTP status is 201 Created, then we expect body to be present
      (let ((json-body (cl-json:decode-json-from-string body)))
        (list :github-issue :user repo-owner :repo repo :number (cdr (assoc :number json-body)))))))

#|
  (open-github-issue "avodonosov" "test" "test-issue" "Test issue body. *Some* _markdown_ [formatting](http://somewhere.com/)")
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;

(defun notify-project (&key prj-name type title body)
  (unless (eq type :bug)
    (error "Wrong notification type ~S. Currently only :BUG notifications are supported." type))
  (destructuring-bind (github-repo-owner github-repo-name)
      (find-github-repo prj-name)
    (unless (and github-repo-owner github-repo-name)
      (error "Github repository for ~A is not found (we currently only support github repositories)."
             prj-name))
    (open-github-issue github-repo-owner github-repo-name
                       title body)))

