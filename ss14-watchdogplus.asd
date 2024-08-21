(defsystem "ss14-watchdogplus"
  :depends-on ("dexador" "uiop")
  :build-operation program-op
  :build-pathname "ss14-watchdogplus"
  :entry-point "cl-user::main"
  :components ((:file "builder")))
