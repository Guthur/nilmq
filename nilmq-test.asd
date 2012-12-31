(asdf:defsystem #:nilmq-test
  :serial t
  :description "NILMQ regression tests"
  :author "Michael Compton"
  :license "MIT"
  :depends-on (#:nilmq #:fiveam #:flexi-streams)
  :components ((:module tests
                :serial t
                :components ((:file "package")
                             (:file "wire-protocol")
                             (:file "socket-type-id")))))