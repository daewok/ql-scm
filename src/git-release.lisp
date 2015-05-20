(in-package #:ql-scm/release/git)

(defclass git-release (scm-release)
  ((ref
    :accessor git-ref
    :initarg :ref)
   (ref-type
    :accessor git-ref-type
    :initarg :ref-type))
  (:documentation "A release that uses git as a storage backend."))

(defmethod update-repo ((release git-release))
  (git-fetch (repo-location release)))

(defmethod live-releasep ((release git-release))
  (or (string= "branch" (git-ref-type release))
      (string= "tag" (git-ref-type release))))

(defmethod ensure-cloned ((release git-release))
  (or
   (clonedp release)
   (progn
     (ensure-repo-dir (dist release))
     (git-clone-bare (archive-url release)
                     (repo-location release)))))

(defmethod local-archive-file ((release git-release))
  (relative-to (dist release)
               (make-pathname :directory '(:relative "archives")
                              :defaults (file-namestring
                                         (concatenate 'string
                                                      (prefix release)
                                                      ".tar")))))

(defmethod short-description ((release git-release))
  (format nil "~A.git.~A-~A" (project-name release)
          (git-ref-type release)
          (git-ref release)))

(defmethod raw-prefix ((release git-release))
  (ensure-cloned release)
  (format nil "~A.git-~A"
                (project-name release)
                (git-rev-parse (repo-location release)
                               (cond
                                 ((or (string= "commit" (git-ref-type release))
                                      (string= "branch" (git-ref-type release)))
                                  (concatenate 'string (git-ref release) "^{commit}"))
                                 ((string= "tag" (git-ref-type release))
                                  (concatenate 'string (git-ref release) "^{tag}"))
                                 (t
                                  (error "Unable to determine prefix for. ~A" release))))))

(defmethod prefix ((release git-release))
  ;; If the system is installed, read the prefix from the metadata file.
  (let ((metadata-file (install-metadata-file release)))
    (if (probe-file metadata-file)
        (with-open-file (stream metadata-file)
          (let* ((folder (read-line stream))
                 (prefix-with-slash (subseq folder
                                            (1+ (position #\/ folder :from-end t :end (1- (length folder)))))))
            (subseq prefix-with-slash 0 (1- (length prefix-with-slash)))))
        (raw-prefix release))))

(defmethod repo-location (release)
  (relative-to (dist release)
               (concatenate 'string "repos/"
                            (project-name release)
                            ".git/")))

(defmethod ensure-local-archive-file ((release git-release))
  (ensure-cloned release)
  (prog1 (ensure-directories-exist (local-archive-file release))
    (git-archive (repo-location release)
                 :prefix (format nil "~A/" (prefix release))
                 :output (local-archive-file release)
                 :treeish (git-ref release))))

(defmethod ql-bundle::unpack-release ((release git-release) target)
  (let ((*default-pathname-defaults* (truename
                                      (ensure-directories-exist target)))
        (archive (ensure-local-archive-file release)))
    (ql-minitar:unpack-tarball archive :directory "software/")
;;    (delete-file temp-tar)
    release))
