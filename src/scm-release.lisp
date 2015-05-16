(in-package #:ql-scm/release)

(defclass scm-release (release)
  ())

(defgeneric clonedp (object)
  (:documentation "Return T iff the object's repository has been cloned.")
  (:method ((release scm-release))
    (probe-file (repo-location release))))

(defgeneric ensure-cloned (object)
  (:documentation "Clone the object's repository if it hasn't been cloned
  already. Returns the pathname of the repository."))

(defgeneric repo-location (object)
  (:documentation "Return the pathname (folder) to the object's repo."))

(defgeneric live-releasep (object)
  (:documentation "T if the object represents a 'live' release.")
  (:method (object)
    nil))

(defgeneric update-repo (object)
  (:documentation "Fetch the latest version of the repository."))

(defgeneric raw-prefix (release)
  (:documentation "Read the raw prefix directly from the repo."))

(defun make-scm-line-instance (line &rest initargs)
  "Create an instance from words in an index file line. The last initarg collects all the trailing arguments, if any."
  (let* ((words (split-spaces line))
         ;;(class-string (second words))
         (arg-words (list* (first words)
                      (cddr words)))
         (class 'ql-scm/release/git:git-release)
         (args (mapcan #'list
                       (butlast initargs)
                       arg-words))
         (trailing (subseq arg-words (1- (length initargs)))))
    (apply #'make-instance class (first (last initargs)) trailing args)))

(defun make-scm-release-from-line (line dist)
  (let ((release
         (make-scm-line-instance line
                            :project-name
                            :archive-url
                            :ref-type
                            :ref
                            :prefix
                            :system-files)))
    (setf (dist release) dist)
    release))

(defun ensure-repo-dir (dist)
  (ensure-directories-exist (relative-to dist "repos/")))

(defmethod install ((release scm-release))
  (let ((tar (ensure-local-archive-file release))
        (output (relative-to (dist release)
                             (make-pathname :directory
                                            (list :relative "software"))))
        (tracking (install-metadata-file release)))
    (ensure-directories-exist output)
    (ensure-directories-exist tracking)
    (unpack-tarball tar :directory output)
    (ensure-directories-exist tracking)
    (let ((base-directory (base-directory release)))
      (with-open-file (stream tracking
                              :direction :output
                              :if-exists :supersede)
        (write-line (qenough base-directory) stream)))
    (let ((provided (provided-systems release))
          (dist (dist release)))
      (dolist (file (system-files release))
        (let ((system (find-system-in-dist (pathname-name file) dist)))
          (unless (member system provided)
            (error "FIND-SYSTEM-IN-DIST returned ~A but I expected one of ~A"
                   system provided))
          (let ((system-tracking (install-metadata-file system))
                (system-file (merge-pathnames file
                                              (base-directory release))))
            (ensure-directories-exist system-tracking)
            (unless (probe-file system-file)
              (error "Release claims to have ~A, but I can't find it"
                     system-file))
            (with-open-file (stream system-tracking
                                    :direction :output
                                    :if-exists :supersede)
              (write-line (qenough system-file)
                          stream))))))
    release))
