;;;; nilmq.asd

(asdf:defsystem #:nilmq
  :serial t
  :description "Native Implementation of ZeroMQ transport protocol and socket semantics"
  :author "Michael Compton"
  :license "MIT"
  :depends-on (#:usocket)
  :components ((:module src
                :serial t
                :components ((:file "nilmq")))))
