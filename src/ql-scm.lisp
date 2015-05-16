(in-package #:ql-scm)

(defmethod ql-dist:installedp ((dist scm-dist))
  (let ((installed (find-dist (ql-dist:name dist))))
    (equalp (ql-dist:version installed) (ql-dist:version dist))))

(defun install-dist (url &key (prompt t) replace)
  (block nil
    (setf url (url url))
    (let ((temp-file (qmerge "tmp/install-dist-distinfo.txt")))
      (ensure-directories-exist temp-file)
      (delete-file-if-exists temp-file)
      (fetch url temp-file)
      (let* ((new-dist (ql-dist::make-dist-from-file temp-file :class 'ql-scm/dist::scm-dist))
             (old-dist (find-dist (ql-dist:name new-dist))))
        (when old-dist
          (if replace
              (ql-dist:uninstall old-dist)
              (restart-case
                  (error "A dist named ~S is already installed."
                         (ql-dist:name new-dist))
                (replace ()
                  :report "Replace installed dist with new dist"
                  (ql-dist:uninstall old-dist)))))
        (format t "Installing dist ~S version ~S.~%"
                (ql-dist:name new-dist)
                (ql-dist:version new-dist))
        (when (or (not prompt)
                  (press-enter-to-continue))
          (ensure-directories-exist (ql-dist:base-directory new-dist))
          (copy-file temp-file (ql-dist:relative-to new-dist "scm-distinfo.txt"))
          (ql-dist::ensure-release-index-file new-dist)
          (ql-dist::ensure-system-index-file new-dist)
          (ql-dist:enable new-dist)
          (setf (ql-dist:preference new-dist) (get-universal-time))
          (when old-dist
            (ql-dist::clear-dist-systems old-dist))
          (ql-dist::clear-dist-systems new-dist)
          new-dist)))))

(defun enable-ql-scm ()
  (pushnew 'scm-dist-enumeration-function ql-dist:*dist-enumeration-functions*)
  (values))

(eval-when (:load-toplevel :execute)
  (enable-ql-scm))

