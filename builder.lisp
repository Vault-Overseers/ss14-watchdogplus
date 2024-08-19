(defvar *platform* "linux-x64")
(defvar *builds* nil)
(defvar *instances* nil)

(defun clear-builds ()
  (setf *builds* nil))

(defun clear-instances ()
  (setf *instances* nil) )

(define-condition build-already-defined (error)
  ((text :initarg :text :reader text)))

(defun defbuild (name &rest body)
  (when (assoc name *builds* :test #'string-equal)
    (error 'build-already-defined :text "The build ~a was already defined."))
  (push (cons name body) *builds*))

(define-condition instance-already-defined (error)
  ((text :initarg :text :reader text)))

(defun definstance (name &key build data)
  (when (assoc name *instances* :test #'string-equal)
    (error 'instance-already-defined :text "The instance ~a was already defined."))
  (find-build build)
  (push (cons name (list :build build :data (if data data name))) *instances*))

(defun read-config (path)
  (clear-builds)
  (clear-instances)
  (load path))

(define-condition build-not-found (error)
  ((text :initarg :text :reader text)))

(defun find-build (build)
  (let ((ret (assoc build *builds* :test #'string-equal)))
    (if ret
        ret
        (error 'build-not-found :text "No build with the name ~a was found."))))

(defun build-repo-path (build)
  (merge-pathnames (pathname (concatenate 'string build "/")) #P"git/"))

(defun release-path (build platform)
  (merge-pathnames (pathname (concatenate 'string build (format nil "/release/SS14.Server_~a.zip" platform))) #P"git/") )

(defun build-path (build)
  (merge-pathnames (pathname (concatenate 'string build "/")) #P"build/") )

(defun update-build (build)
  (destructuring-bind (name m url hash) (find-build build)
    (unless (equal :GIT m)
      (error 'program-error "Update type not supported"))
    (multiple-value-bind (repo-path created)
        (ensure-directories-exist (build-repo-path name))
      (when created
        (clone url hash repo-path))
      (update hash repo-path)
      (do-package repo-path *platform*)
      (backup-build build)
      (do-extract build))))

(defun nice (command)
  (cons "nice" command))

(defmacro with-status-as (message &body body)
  `(progn
     (format t "===> ~a... " ,message)
     ,@body
     (format t "done~%")))

(defun clone (url hash dest)
  (declare (ignore hash))
  (with-status-as (format nil "Cloning ~a" dest)
    (uiop:run-program (nice (list "git" "clone" "--depth=1" url (namestring dest))))))

(defun update (hash dest)
  (declare (ignore hash))
  (with-status-as (format nil "Updating ~a" dest)
    (uiop:call-with-current-directory
      dest
      (lambda ()
        (uiop:run-program (nice (list "git" "pull")))
        (uiop:run-program (nice (list "git" "submodule" "update" "--init" "--recursive")))))))

(defun do-package (dir platform)
  (with-status-as (format nil "Building ~a" dir)
    (uiop:call-with-current-directory
      dir
      (lambda ()
        ; todo: old packaging method python Tools/package_server_build.py --hybrid-acz
        (uiop:run-program (nice (list "dotnet" "run" "--project" "Content.Packaging" "server" "--hybrid-acz" "--platform" platform)))))))

(defun backup-build (build)
  (with-status-as (format nil "Backing up ~a" build)
    (let ((dest (ensure-directories-exist (build-path build))))
      (backup dest))))

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

(defun rmrf (path)
  (uiop:run-program (list "rm" "-rf" (namestring path))))

(defun backup-name (path)
  (concatenate 'string (pathname-name path) ".bak"))

(defun trim-trailing-slash (path)
  (let ((pl (coerce path 'list)))
    (coerce (if (eq (car (last pl)) #\/)
        (without-last pl)
        pl) 'string)))

(defun without-last (l)
  (reverse (cdr (reverse l))))

(defun do-extract (build)
  (with-status-as (format nil "Extracting ~a" build)
    (let ((dest (ensure-directories-exist (build-path build))))
      (uiop:run-program (nice (list "unzip" "-d" (namestring dest) (namestring (release-path build *platform*))))))))

(defun data-path (name)
  (pathname (format nil "data/~a/" name)))

(defun config-path (name)
  (pathname (format nil "config/~a.toml" name)))

(define-condition instance-not-found (error)
  ((text :initarg :text :reader text)))

(defun find-instance (build)
  (let ((ret (assoc build *instances* :test #'string-equal)))
    (if ret
        ret
        (error 'instance-not-found :text "No instance with the name ~a was found."))))

(defun prestart-instance (inst)
  (find-instance inst)
  (ensure-directories-exist (data-path inst))
  (ensure-file-exist (config-path inst)))

(defun ensure-file-exist (name)
  (let ((path (pathname name)))
    (ensure-directories-exist (directory-namestring path))
    (close (open path :if-does-not-exist :create))))

(defun prestart-all ()
  (loop for inst in *instances* do
        (prestart-instance (first inst))))

;(read-config "config.lisp")

;(update-build "ds14")

;(prestart-all)
