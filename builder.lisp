(defparameter *platform* "freebsd-x64")
(defvar *builds* nil)
(defvar *instances* nil)
(defvar *running-instances* nil)

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

(defun definstance (name &key build data port)
  (when (assoc name *instances* :test #'string-equal)
    (error 'instance-already-defined :text "The instance ~a was already defined."))
  (find-build build)
  (unless port
    (error "Instance ~a needs to specify a :PORT" name))
  (push (cons name (list :build build :data (if data data name) :port port)) *instances*))

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

(defun build-path (build &optional rev)
  (merge-pathnames (pathname (if rev
                                 (concatenate 'string build "/" rev "/")
                                 (concatenate 'string build "/"))) #P"build/") )

(defmacro with-cwd (dir &body body)
  "Like UIOP:WITH-CURRENT-DIRECTORY, but returns the value of evaluating BODY."
  (let ((rval-name (gensym)))
    `(let ((,rval-name))
       (uiop:call-with-current-directory ,dir (setf ,rval-name (lambda () ,@body))))))

(defun update-build (build)
  (destructuring-bind (name m url hash) (find-build build)
    (unless (equal :GIT m)
      (error 'program-error "Update type not supported"))
    (multiple-value-bind (repo-path created)
        (ensure-directories-exist (build-repo-path name))
      (when created
        (clone url hash repo-path))
      (let ((latest (update hash repo-path)))
        (if (has-build build latest)
            (format t "===> Revision ~a already built, skipping build~%" latest)
            (progn
              (do-package repo-path *platform*)
              (do-extract build latest)))
        ; symlink to latest build
        (with-cwd (build-path build)
          (uiop:run-program (list "ln" "-shf" (concatenate 'string latest "/") "latest")))))))

(defun has-build (build rev)
  (probe-file (build-path build rev)))

(defun nice (command)
  (cons "nice" command))

(defmacro with-status-as (message &body body)
  `(progn
     (format t "===> ~a... " ,message)
     (let ((ret (progn ,@body)))
       (format t "~a~%" (if ret ret "done"))
       ret)))

(defun clone (url hash dest)
  (declare (ignore hash))
  (with-status-as (format nil "Cloning ~a" dest)
    (uiop:run-program (nice (list "git" "clone" "--depth=1" url (namestring dest))))))

(defun update (hash dest)
  (declare (ignore hash))
  (with-status-as (format nil "Updating ~a from Git" dest)
    (with-cwd dest
      (uiop:run-program (nice (list "git" "pull")))
      (uiop:run-program (nice (list "git" "submodule" "update" "--init" "--recursive")))
      (uiop:run-program (list "git" "rev-parse" "HEAD") :output '(:string :stripped t)))))

(defun do-package (dir platform)
  (with-status-as (format nil "Building ~a" dir)
    (uiop:call-with-current-directory
      dir
      (lambda ()
        ; todo: old packaging method python Tools/package_server_build.py --hybrid-acz
        (uiop:run-program (nice (list "dotnet" "run" "--project" "Content.Packaging" "server" "--hybrid-acz" "--platform" platform)))))))

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

(defun do-extract (build rev)
  (with-status-as (format nil "Extracting ~a" build)
    (let ((dest (ensure-directories-exist (build-path build rev))))
      (uiop:run-program (nice (list "unzip" "-d" (namestring dest) (namestring (release-path build *platform*))))))))

(defun data-path (name)
  (pathname (format nil "data/~a/" name)))

(defun config-path (name)
  (pathname (format nil "config/~a.toml" name)))

(defun log-path (name)
  (pathname (format nil "log/~a.log" name)))

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

(defun find-running (name)
  (assoc name *running-instances* :test #'string-equal))

(defun running (name)
  (let ((inst (find-running name)))
    (if (and inst (uiop:process-alive-p (cdr inst)))
        T
        (progn (setf *running-instances* (delete inst *running-instances*))
               nil))))

(defun cvardef (name val)
  (format nil "~a=~a" name val))

(defun start-instance (name)
  (let* ((inst (cdr (find-instance name)))
         (exe (concatenate 'string (namestring (build-path (getf inst :build))) "latest/Robust.Server")))
    (print exe)
    (unless (running name)
      (ensure-directories-exist (log-path name))
      (let ((procinfo (uiop:launch-program (list exe
                                                 "--config-file" (namestring (config-path name))
                                                 "--data-dir" (namestring (data-path (getf inst :data)))
                                                 "--cvar" (cvardef "watchdog.key" name)
                                                 "--cvar" (cvardef "watchdog.token" name))
                                           :output (log-path name)
                                           :if-output-exists :append
                                           :error-output (log-path name)
                                           :if-error-output-exists :append)))
        (push (cons name procinfo) *running-instances*)))))

;(read-config "config.lisp")

;(update-build "ds14")

;(prestart-all)

;(start-instance "ds14-prod")

;(kill-instance "ds14-prod")

(defun kill-instance (name)
  (let ((inst (find-running name)))
    (uiop:terminate-process (cdr inst))
    (setf *running-instances* (delete inst *running-instances*))))
