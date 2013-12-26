# Tool to send notifications to Common Lisp projects.

## Motivation

   When cl-test-grid detects many regressions in some library,
   we sant to inform the library authors. Informing library
   authors taks lot of manual work: google for the library home
   page, learn where it's issue tracker or makling list,
   send the message.

   We want to authomate this.

   Also, other CL community members have similar needs.

   It looks like we need two types of notifications:
   - open bugs in issue trackers when we want library authros to 
     take some action
   - just a notification message, maybe to mailing list or author email

   We envision a function called like this:
   ```common-lisp

   (cl-prj-notify:notify-project :prj-name "some-project"
                                 :type :bug ;; or :message
                                 :title "regressions on quicklisp 2013-12-13"
                              :body
                              "Hello.

some-project has regressions on quicklisp 2013-12-13.

See some-projects test results from the last two quickisp versions:
http://common-lisp.net/project/cl-test-grid/library/gendl.html")

   ```

 => If possible, returns a reference to the notification submitted to the project.
    Something like '(:launchpad-ticket 995657) for launchpad tickets,
    or '(:github-issue :user "avodonosov" :repo "test" :number 160) for github issues,
    or '(mail-archive "http://some/archive/item.html")
    or just an URI "http://some/message.html"

    If it's not possible to return a reference,
    returns T.

    Signals an ERROR in case of problems.


## Status

   Half of CL projects in Quicklisp are hosted on GitHub.

   The function `notify-project` supports `:bug` notifications
   for these projects.

## Usage

   You need an GitHub Personal Access Tokens to create issues.
   Create one at https://github.com/settings/applications

   ```common-lisp
   (pushnew "pats/to/cl-prj-notify/"
            asdf:*central-registry* :test #'equal)

   (ql:quickload :cl-prj-notify)
   (setf cl-prj-notify:*github-personal-token* "your-gihub-token-00000000000")

   (cl-prj-notify:notify-project :prj-name "project-name"
                                 :type :bug
                                 :title "unexpected error"
                                 :body
                                 "Hello.
bla-bla-bla
bla-bla-bla")
   ```