(defun dotnet-rid ()
  "Guess the running dotnet RID, for passing as the target for the packaging script."
  (let ((os (ecase (uiop:operating-system)
              (:LINUX "linux")
              (:BSD "freebsd")))
        (arch (ecase (uiop:architecture)
                (:X64 "x64"))))
    (format nil "~a-~a" os arch)))

(defparameter *platform* (dotnet-rid))
(defvar *builds* nil)
(defvar *instances* nil)
(defvar *running-instances* nil)

(defun clear-builds ()
  (setf *builds* nil))

(defun clear-instances ()
  (setf *instances* nil) )

(defun defbuild (name &rest body)
  (when (assoc name *builds* :test #'string-equal)
    (warn "Repeated definition of build ~a" name))
  (push (cons name body) *builds*))

(defun definstance (name &key build data port)
  (when (assoc name *instances* :test #'string-equal)
    (warn "Repeated definition of instance ~a" name))
  (find-build build)
  (unless port
    (error "Instance ~a needs to specify a :PORT" name))
  (push (cons name (list :build build :data (if data data name) :port port)) *instances*))

(defun read-config (path)
  (clear-builds)
  (clear-instances)
  (load path))

(defun find-build (build)
  (let ((ret (assoc build *builds* :test #'string-equal)))
    (if ret
        ret
        (error "No build with the name ~a was found." build))))

(defun build-repo-path (build)
  (merge-pathnames (pathname (concatenate 'string build "/")) #P"git/"))

(defun release-path (build platform)
  (merge-pathnames (pathname (concatenate 'string build (format nil "/release/SS14.Server_~a.zip" platform))) #P"git/") )

(defun build-path (build &optional rev)
  (merge-pathnames (pathname (if rev
                                 (concatenate 'string build "/" rev "/")
                                 (concatenate 'string build "/"))) #P"build/") )

(defun update-build (build)
  "Update the given build. Returns T if a new build was made, NIL if it failed or no build needed."
  (destructuring-bind (name m url hash) (find-build build)
    (unless (equal :GIT m)
      (error 'program-error "Update type not supported"))
    (multiple-value-bind (repo-path created)
        (ensure-directories-exist (build-repo-path name))
      (when created
        (clone url hash repo-path))
      (let ((has-new-build nil)
            (latest (update hash repo-path)))
        (if (has-build build latest)
            (format t "===> Revision ~a already built~%" latest)
            (progn
              (do-package repo-path *platform*)
              (do-extract build latest)
              (setf has-new-build T)))
        ; symlink to latest build
        (with-cwd (build-path build)
          (symlink (concatenate 'string latest "/") "latest"))
        has-new-build))))

(defun has-build (build rev)
  (probe-file (build-path build rev)))

(defun nice (command)
  (cons "nice" command))

(defmacro with-status-as (message &body body)
  `(progn
     (format t "===> ~a... " ,message)
     (force-output)
     (let ((ret (progn ,@body)))
       (format t "~a~%" (if ret ret "done"))
       (force-output)
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

(defun do-extract (build rev)
  (with-status-as (format nil "Extracting ~a" build)
    (let ((dest (ensure-directories-exist (build-path build rev))))
      (uiop:run-program (nice (list "unzip" "-d" (namestring dest) (namestring (release-path build *platform*)))))
      (with-cwd dest
        (uiop:run-program (list "chmod" "+x" "Robust.Server"))))))

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
  (format t "===> Starting instance ~a...~%" name)
  (let* ((inst (cdr (find-instance name)))
         (exe (concatenate 'string (namestring (build-path (getf inst :build))) "latest/Robust.Server")))
    (print exe)
    (unless (running name)
      (ensure-file-exist (log-path name))
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

(defun instances-needing-build (build)
  "Return a list of instance ID's that depend on the given build."
  (mapcar #'first (remove-if-not (lambda (i) (string-equal build (getf (cdr i) :build))) *instances*)))

(defun notify-update (build)
  (when (update-build build)
    (dolist (i (remove-if-not #'running (instances-needing-build build)))
      (signal-update-instance i))))

(defun signal-update-instance (name)
  (with-status-as (format nil "Notifying ~a of an update" name)
    (let* ((inst (find-instance name))
           (port (getf (cdr inst) :port)))
      (when port
        (dex:post (format nil "http://localhost:~a/update" port)
                  :headers (list (cons "WatchdogToken" name))
                  :connect-timeout 1)))))

(defun kill-instance (name)
  (let ((inst (find-running name)))
    (setf *running-instances* (delete inst *running-instances*))  
    (uiop:terminate-process (cdr inst))))

(defun check-children ()
  "Return a list of servers that are supposed to be running but are not."
  (let ((reaplist nil))
    (dolist (el *running-instances*)
      (unless (uiop:process-alive-p (cdr el))
        (format t "===>>> Instance ~a died~%" (car el))
        (push (car el) reaplist)))
    reaplist))

(defun list-builds ()
  (mapcar #'first *builds*))

(defun list-instances ()
  (mapcar #'first *instances*))

(defun start-instances (is)
  (dolist (i is)
    (start-instance i)))

(defun reload ()
  (read-config "config.lisp")
  (prestart-all)
  (format t "===>>> Loaded configuration ~a~%" (list-instances)))

(defun cli ()
  (format t "===> Welcome to the ss14-watchdog command line!~%")
  (break))

(defun select-build (prompt)
  (checklist prompt (list-builds)))

(defun run-update ()
  (dolist (l (select-build "Which build(s) should be updated?"))
    (notify-update l)))

(defun dialog-menu ()
  (menu-dispatch
    "What do you want to do?"
    (list (list "update" "Update a build" #'run-update)
          (list "reload" "Reload configuration file" #'reload)
          (list "break" "Drop into a REPL (advanced)" #'cli)
          (list "shutdown" "Shut down everything and exit" #'shutdown))))

(defun watchdog ()
  (loop do
        (start-instances (check-children))
        (handler-case (sleep 10)
          (#+sbcl sb-sys:interactive-interrupt
           #+ecl ext:interactive-interrupt
            ()
            (dialog-menu)
            (dialog-clear)
            (format t "===> Watchdog running. Press CTRL-C to enter menu.~%")))))

(defun start ()
  (format t "===>>> Starting builds for ~a...~%" (list-builds))
  (dolist (b (list-builds))
    (update-build b))
  (format t "===>>> Starting instances ~a...~%" (list-instances))
  (start-instances (list-instances)))

(defun stop ()
  (dolist (i *running-instances*)
    (kill-instance (car i))))

(defun shutdown ()
  (stop)
  (#+sbcl sb-ext:exit
   #+ecl sys:exit))

(defun main ()
  (reload)
  (start)
  (watchdog))
