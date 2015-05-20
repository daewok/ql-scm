(in-package :asdf-user)

(asdf:defsystem #:ql-scm
  :description "A Quicklisp extension that can pull from project SCMs directly."
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :depends-on (#:quicklisp
               (:feature (:not :asdf3) :uiop))
  :author "Eric Timmons <etimmons@mit.edu>"
  :license "MIT"
  :components ((:module "src"
                        :components
                        ((:file "package")
                         (:file "git")
                         (:file "scm-release")
                         (:file "scm-dist")
                         (:file "git-release")
                         (:file "ql-scm")))))
