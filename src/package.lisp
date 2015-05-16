(defpackage #:ql-scm/git
  (:documentation "Commands for interacting with a command line git.")
  (:use #:cl)
  (:export #:git-clone-bare
           #:git-archive
           #:git-rev-parse
           #:git-fetch))

(defpackage #:ql-scm/release
  (:documentation
   "Releases that use git as a storage backend.")
  (:use #:cl
        #:ql-dist
        #:ql-minitar
        #:ql-setup
        #:ql-util)
  (:export #:make-scm-release-from-line
           #:scm-release
           #:clonedp
           #:ensure-cloned
           #:ensure-repo-dir
           #:repo-location
           #:live-releasep
           #:update-repo
           #:raw-prefix))

(defpackage #:ql-scm/dist
  (:documentation
   "Dist backend using SCM.")
  (:use #:cl
        #:ql-dist
        #:ql-setup
        #:ql-http
        #:ql-scm/release
        #:ql-util)
  (:shadow #:find-dist)
  (:export #:scm-dist-enumeration-function
           #:scm-dist
           #:find-dist))

(defpackage #:ql-scm/release/git
  (:documentation
   "Releases that use git as a storage backend.")
  (:use #:cl
        #:ql-dist
        #:ql-scm/git
        #:ql-scm/release
        #:ql-minitar)
  (:export #:git-release))

(defpackage #:ql-scm
  (:documentation "The user accessible frontend to ql-scm.")
  (:use #:cl
        #:ql-scm/dist
        #:ql-setup
        #:ql-http
        #:ql-util)
  (:export #:enable-ql-scm
           #:find-dist
           #:install-dist))
