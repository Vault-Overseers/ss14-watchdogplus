(defmacro with-cwd (dir &body body)
  "Like UIOP:WITH-CURRENT-DIRECTORY, but returns the value of evaluating BODY."
  (let ((rval-name (gensym)))
    `(let ((,rval-name))
       (uiop:call-with-current-directory ,dir (setf ,rval-name (lambda () ,@body))))))

(defun ensure-file-exist (name)
  (let ((path (pathname name)))
    (ensure-directories-exist (directory-namestring path))
    (close (open path :if-does-not-exist :create))))

(defun symlink (target link)
  (uiop:run-program (list "ln" (ecase (uiop:operating-system)
                                 (:BSD "-shf")
                                 (:LINUX "-snf"))
                          target link)))

(defun rmrf (path)
  (uiop:run-program (list "rm" "-rf" (namestring path))))

(defun backup (path)
  (let* ((tpath (trim-trailing-slash (namestring path)))
         (bname (backup-name tpath))
         (bpath (merge-pathnames (pathname bname) (pathname tpath))))
    ; Remove existing backup if exists
    (rmrf bpath)
    ; Rename path to path with .bak
    (handler-case
        (rename-file tpath bname)
      (file-error (e) (declare (ignore e))))))

(defun backup-name (path)
  (concatenate 'string (pathname-name path) ".bak"))

(defun trim-trailing-slash (path)
  (let ((pl (coerce path 'list)))
    (coerce (if (eq (car (last pl)) #\/)
        (without-last pl)
        pl) 'string)))

(defun without-last (l)
  (reverse (cdr (reverse l))))
