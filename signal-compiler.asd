(asdf:defsystem #:signal-compiler
  :description "Compiler for a subset of Signal language"
  :author "Yurii Hryhorenko <yuragrig@ukr.net>"
  :depends-on (#:cl-yatlp)
  :serial t
  :components ((:file "signal")))
