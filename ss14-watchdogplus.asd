(defsystem "ss14-watchdogplus"
  :depends-on ("bt-semaphore" "dexador" "str" "uiop" "woo")
  :build-operation program-op
  :build-pathname "ss14-watchdogplus"
  :entry-point "cl-user::main"
  :components ((:file "fsutil")
               (:file "dialog")
               (:file "builder")))
