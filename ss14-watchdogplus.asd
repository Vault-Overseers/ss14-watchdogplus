(defsystem "ss14-watchdogplus"
  :depends-on ("bt-semaphore" "dexador" "str" "uiop" "woo")
  :build-operation program-op
  :build-pathname "ss14-watchdogplus"
  :entry-point "cl-user::main"
  :components ((:file "fsutil")
               (:file "dialog")
               (:file "builder")))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))
