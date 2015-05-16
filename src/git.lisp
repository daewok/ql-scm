(in-package #:ql-scm/git)

(defun git (&rest args)
  "Execute git using the provided arguments. Returns three values. The first is
a list of all lines printed to stdout. The second is a list of all lines printed
to stderr. The third is the exit code."
  (uiop:run-program (cons "git" args) :output :lines :error-output :lines))

(defmacro with-dir ((directory) &body body)
  "Change to DIRECTORY and execute BODY. DIRECTORY must not point to a file in
the directory."
  `(uiop:with-current-directory ((uiop:ensure-directory-pathname ,directory))
     ,@body))

(define-condition git-repo-dirty ()
  ((message
    :initarg :message)
   (repo
    :initarg :repo))
  (:report (lambda (condition stream)
             (format stream "Repo: ~a~%Message: ~a" (slot-value condition 'repo) (slot-value condition 'message)))))

(declaim (inline assert-repo-clean))
(defun assert-repo-clean (repo &optional action)
  (assert (git-repo-clean? repo)
          ()
          'git-repo-dirty
          :message (format nil "Unable to ~a, repository dirty." (if action
                                                                     action
                                                                     "perform action"))
          :repo repo))


(defun clone-repo (remote local)
  "Clone the repository at REMOTE to LOCAL."
  (git "clone" remote (namestring local))
  (values))

(defun git-clone-bare (remote local)
  (git "clone" "--mirror" remote (namestring local)))

(defun git-rev-parse (repo rev)
  (with-dir (repo)
    (first (git "rev-parse" "--verify" rev))))

(defun git-archive (repo &key output treeish prefix)
  (with-dir (repo)
    (apply #'git `("archive"
                   ,@(when output
                           (list (format nil "--output=~A" (namestring output))))
                   ,@(when prefix
                           (list (format nil "--prefix=~A" prefix)))
                   ,@(when treeish
                           (list treeish))))))

(defun git-repo-clean? (repo)
  "Returns NIL if the repository is dirty (has staged or unstaged changes), T
otherwise."
  (with-dir (repo)
    (not (git "status" "--porcelain"))))

(defun git-repo-dirty? (repo)
  "Returns T if the repository is dirty (has staged or unstaged changes), NIL otherwise."
  (not (git-repo-clean? repo)))

(defun git-repo? (dir)
  (ignore-errors
    (git-repo-clean? dir)
    t))

(defun git-pull (repo)
  (assert-repo-clean repo)
  (with-dir (repo)
    (git "pull")
    (values)))

(defun git-fetch (repo)
  (with-dir (repo)
    (git "fetch")
    (values)))
