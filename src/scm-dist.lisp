(in-package #:ql-scm/dist)

(defclass scm-dist (dist)
  ((scm-release-index-url
    :accessor scm-release-index-url
    :initarg :scm-release-index-url))
  (:documentation "A distribution that can use either tarballs or scm
  repositories as a storage backend."))

(defun scm-dist-enumeration-function ()
  "Function used for producing a list of scm dist objects."
  (loop :for file :in (directory (qmerge "dists/*/scm-distinfo.txt"))
     :collect (ql-dist::make-dist-from-file file :class 'scm-dist)))

(defun ensure-scm-release-index-file (dist)
  (let ((pathname (relative-to dist "scm-releases.txt")))
    (or (probe-file pathname)
        (nth-value 1 (fetch (scm-release-index-url dist) pathname)))))

(defun ensure-scm-release-cdb-file (dist)
  (let* ((release-file (ensure-scm-release-index-file dist))
         (cdb-file (make-pathname :type "cdb" :defaults release-file)))
    (or (probe-file cdb-file)
        (ql-cdb:convert-index-file release-file
                                   :cdb-file cdb-file
                                   :index 0))))

;; The cache checking should be done in an around method defined by ql.
(defmethod find-release-in-dist (release-name (dist scm-dist))
  ;; Check to see if the system is backed by scm.
  (let* ((index (ql-dist::release-index dist))
         (release (gethash release-name index)))
    (or release
        (let ((line (ql-dist::cdb-lookup dist release-name
                                         (ensure-scm-release-cdb-file dist))))
          (when line
            (setf (gethash release-name index)
                  (make-scm-release-from-line line dist))))
        (call-next-method))))

(defmethod initialize-release-index ((dist scm-dist))
  (call-next-method)
  (let ((releases (ensure-scm-release-index-file dist))
        (index (ql-dist::release-index dist)))
    (ql-dist::call-for-each-index-entry
     releases
     (lambda (line)
       (let ((instance (make-scm-release-from-line line dist)))
         ;; Don't clobber anything previously loaded via CDB
         (unless (gethash (project-name instance) index)
           (setf (gethash (project-name instance) index) instance)))))
    (setf (ql-dist::release-index dist) index)))

(defun find-dist (name)
  (find name (all-dists) :test #'string= :key #'ql-dist:name))

(defun dist-has-live-release-installedp (dist)
  (some #'live-releasep (installed-releases dist)))

(defun installed-live-releases (dist)
  (remove-if-not #'live-releasep (installed-releases dist)))

(defmethod available-update ((dist scm-dist))
  (let ((url (ql-dist::distinfo-subscription-url dist))
        (target (qmerge "tmp/distinfo-update/scm-distinfo.txt"))
        (update-directory (qmerge "tmp/distinfo-update/")))
    (when (ql-impl-util:probe-directory update-directory)
      (ql-impl-util:delete-directory-tree (qmerge "tmp/distinfo-update/")))
    (when url
      (ensure-directories-exist target)
      (fetch url target :quietly t)
      (let ((new (ql-dist::make-dist-from-file target :class 'scm-dist)))
        (setf (base-directory new)
              (make-pathname :name nil
                             :type nil
                             :version nil
                             :defaults target))
        (when (or (and (string= (name dist) (name new))
                       (string/= (version dist) (version new)))
                  (dist-has-live-release-installedp dist))
          new)))))

(defmethod update-release-differences :before ((old-dist scm-dist)
                                               (new-dist scm-dist))
  ;; Update all live release repos in old-dist.
  (dolist (release (installed-live-releases old-dist))
    (update-repo release)))

(defmethod update-release-differences ((old-dist scm-dist)
                                       (new-dist scm-dist))
  ;;(values nil nil nil)
  (let ((old-releases (provided-releases old-dist))
        (new-releases (provided-releases new-dist))
        (new '())
        (updated '())
        (removed '())
        (old-by-name (make-hash-table :test 'equalp)))
    (dolist (release old-releases)
      (setf (gethash (name release) old-by-name)
            release))
    (dolist (new-release new-releases)
      (let* ((name (name new-release))
             (old-release (gethash name old-by-name)))
        (remhash name old-by-name)
        (cond ((not old-release)
               (push new-release new))
              ((and (installedp old-release)
                    (not (equal (prefix old-release)
                                (raw-prefix old-release))))
               (push (list old-release new-release) updated)))))
    (maphash (lambda (name old-release)
               (declare (ignore name))
               (push old-release removed))
             old-by-name)
    (values (nreverse new)
            (nreverse updated)
            (sort removed #'string< :key #'prefix))))

(defmethod clean ((dist scm-dist))
  (setf dist (find-dist (name dist)))
  (let* ((releases (installed-releases dist))
         (known-archives (mapcar 'local-archive-file releases))
         (known-directories (mapcar 'base-directory releases))
         (present-archives (mapcar 'truename
                                   (ql-impl-util:directory-entries
                                    (relative-to dist "archives/"))))
         (present-directories (mapcar 'truename
                                      (ql-impl-util:directory-entries
                                       (relative-to dist "software/"))))
         (garbage-archives
          (set-difference present-archives known-archives
                          :test 'equalp))
         (garbage-directories
          (set-difference present-directories known-directories
                          :test 'equalp)))
    (map nil 'delete-file garbage-archives)
    (map nil 'ql-impl-util:delete-directory-tree garbage-directories)))

(defmethod update-in-place ((old-dist scm-dist) (new-dist scm-dist))
  (flet ((remove-installed (type)
           (let ((wild (merge-pathnames (make-pathname :directory
                                                       (list :relative
                                                             "installed"
                                                             type)
                                                       :name :wild
                                                       :type "txt")
                                        (base-directory old-dist))))
             (dolist (file (directory wild))
               (delete-file file)))))
    (let ((reinstall-releases (installed-releases old-dist)))
      (remove-installed "systems")
      (remove-installed "releases")
      (delete-file-if-exists (relative-to old-dist "releases.txt"))
      (delete-file-if-exists (relative-to old-dist "systems.txt"))
      (delete-file-if-exists (relative-to old-dist "releases.cdb"))
      (delete-file-if-exists (relative-to old-dist "systems.cdb"))
      (replace-file (ql-dist::local-distinfo-file new-dist)
                    (ql-dist::local-distinfo-file old-dist))
      (setf new-dist (find-dist (name new-dist)))
      (dolist (old-release reinstall-releases)
        (let* ((name (name old-release))
               (new-release (find-release-in-dist name new-dist)))
          (if new-release
              (ensure-installed new-release)
              (warn "~S is not available in ~A" name new-dist)))))))









