;;;; nilmq.lisp

(defpackage #:nilmq
  (:use #:cl))

(in-package #:nilmq)

(defparameter *protocol-revision* 1)
(defparameter *more-frames-flag* 1)
(defparameter *long-message-flag* 2)

(defmacro define-socket-types ((&rest socket-types))
  (let ((max-socket-id 0)
        (socket-values nil))
    `(progn
       ,@(loop
          :for socket-type :in socket-types
          :for id :from 0
          :collect `(defparameter ,(car socket-type) ,id)
          :do (push (cons (car socket-type) id) socket-values)
          :finally (setf max-socket-id id))

       (defun valid-socket-type-id-p (socket-id)
         (and (>= socket-id 0) (<= socket-id ,max-socket-id)))

       (defparameter *socket-assoc-list*
         ',(loop
            :for socket-type :in socket-types
            :collect (cons (cdr (assoc (car socket-type) socket-values))
                           (list (mapcar (lambda (mapping)
                                           (cdr (assoc mapping socket-values)))
                                         (cadr socket-type))))))

       (defun valid-socket-type-pattern-p (socket-1 socket-2)
         (member socket-2 (cadr (assoc socket-1 *socket-assoc-list*)))))))

(define-socket-types ((*pair* (*pair*))
                      (*pub* (*sub*))
                      (*sub* (*pub*))
                      (*req* (*rep* *router*))
                      (*rep* (*req* *dealer*))
                      (*dealer* (*rep* *router* *dealer*))
                      (*router* (*req* *router* *dealer*))
                      (*pull* (*push*))
                      (*push* (*pull*))))

(define-condition invalid-identity-length-error (error) nil)
(define-condition invalid-socket-type-id-error (error) nil)
(define-condition invalid-socket-type-pattern-error (error) nil)
(define-condition invalid-signature-error (error) nil)
(define-condition invalid-identity-error (error) nil)
(define-condition invalid-frame-error (error) nil)
(define-condition message-body-too-long-error (error) nil)

(let ((signature #(#xff 0 0 0 0 0 0 0 0 #x7f)))
  (defun write-no-interop-signature (stream)
    (write-sequence signature stream)))

(defun write-signature (stream &optional (identity-length 0))
  (assert (and (>= identity-length 0)
               (<= identity-length 255))
          nil 'invalid-identity-length-error)
  (write-sequence (make-array 10
                              :element-type '(unsigned-byte 8)
                              :initial-contents `(#xff 0 0 0 0 0 0 ,(ash (1+ identity-length) -8)
                                                       ,(boole boole-and #xff (1+ identity-length))
                                                       #x7f))
                  stream))

(defun write-revision-and-socket-type (stream revision socket-type)
  (assert (valid-socket-type-id-p socket-type) nil 'invalid-socket-type-id-error)
  (write-byte revision stream)
  (write-byte socket-type stream))

(defun write-identity (stream identity)
  (assert (< (length identity) 255) nil 'invalid-identity-length-error)
  (write-sequence `#(0 ,(length identity)) stream)
  (when identity
    (write-sequence identity stream)))

(defun write-large-body-length (length stream)
  (assert (< length (expt 2 64)) nil 'message-body-too-long-error)
  (loop
   :for bit-shift :from -56 :upto 0 :by 8
   :do (write-byte (boole boole-and
                          (ash length bit-shift)
                          #xff)
                   stream)))

(defun write-frame (stream body &optional more)
  (let ((body-length (length body)))
    (write-byte (+ (if more *more-frames-flag* 0)
                   (if (> body-length 255) *long-message-flag* 0))
                stream)
    (if (> body-length 255)
        (write-large-body-length body-length stream)
        (write-byte body-length stream))
    (when body (write-sequence body stream))))

(defun read-length-from-octets (sequence start count)
  (loop
   :for index :from start :below (+ start count)
   :for shift :from (* (1- count) 8) :downto 0 :by 8
   :summing (ash (aref sequence index) shift)))

(defun read-signature (stream)
  (let ((sig (make-array 10 :element-type '(unsigned-byte 8))))
    (assert (= (read-sequence sig stream) 10) nil 'invalid-signature-error)
    (assert (= (aref sig 0) #xff) nil 'invalid-signature-error)
    (assert (= (aref sig 9) #x7f) nil 'invalid-signature-error)
    (let ((identity-length (read-length-from-octets sig 1 8)))
      (assert (and (>= identity-length 0)
                   (<= identity-length 256))
              nil 'invalid-signature-error))
    sig))

(defun read-revision (stream)
  (read-byte stream))

(defun read-socket-type (stream)
  (read-byte stream))

(defun read-identity (stream)
  (let ((final-short (make-array 2 :element-type '(unsigned-byte 8))))
    (assert (= (read-sequence final-short stream) 2)
            nil 'invalid-identity-error)
    (assert (zerop (aref final-short 0)) nil 'invalid-identity-error)
    (let* ((identity-length (aref final-short 1))
           (identity (make-array identity-length
                                 :element-type '(unsigned-byte 8))))
      (assert (= (read-sequence identity stream) identity-length)
              nil 'invalid-identity-error)
      (values identity identity-length))))

(defun read-frame (stream)
  (let ((flags (read-byte stream)))
    (assert (zerop (boole boole-and flags #xfc)) nil 'invalid-frame-error)
    (let* ((more? (= (boole boole-and flags *more-frames-flag*)
                     *more-frames-flag*))
           (long? (= (boole boole-and flags *long-message-flag*)
                     *long-message-flag*))
           (length (if long?
                       (let ((seq (make-array 8 :element-type '(unsigned-byte 8))))
                         (assert (= (read-sequence seq stream) 8) nil 'invalid-frame-error)
                         (read-length-from-octets seq 0 8))
                       (read-byte stream)))
           (body (make-array length :element-type '(unsigned-byte 8))))
      (assert (= (read-sequence body stream) length) nil 'invalid-frame-error)
      (values body length more?))))

(defclass socket ()
  ((protocol-revision :initform *protocol-revision*
                      :initarg :protocol-revision)
   (type-id :initform nil
            :initarg :type-id)
   (id :initform nil
       :initarg :id
       :reader id)
   (connection-engines :initform (list))))

(defclass connection-engine ()
  ((in-bound :initform (mp:make-mailbox))
   (out-bound :initform (mp:make-mailbox))
   (raw-socket :initarg :raw-socket)
   (out-worker-thunk :initform nil)
   (in-worker-thunk :initform nil)))

(defun connection-engine-out-thunk (connection-engine)
  (lambda ()
    (with-slots ((out-bound out-bound)
                 (raw-socket raw-socket))
        connection-engine
      (loop :for msg = (mp:mailbox-read out-bound) :do
       (write-frame (usocket:socket-stream raw-socket) nil)
       (write-frame (usocket:socket-stream raw-socket) nil t)
       (write-frame (usocket:socket-stream raw-socket)
                    (car msg)
                    (cdr msg))
       (when (mp:mailbox-empty-p out-bound)
         (force-output (usocket:socket-stream raw-socket)))))))

(defun connection-engine-in-thunk (connection-engine)
  (lambda ()
    (with-slots ((in-bound in-bound)
                 (raw-socket raw-socket))
        connection-engine
      (loop

       (multiple-value-bind (body length more?) (read-frame (usocket:socket-stream raw-socket))
         (declare (ignore length))
         (mp:mailbox-send in-bound (cons body more?)))))))

(defun make-connection-engine (socket host port)
  (let ((raw-socket (usocket:socket-connect host port :element-type '(unsigned-byte 8))))
    (send-greeting socket (usocket:socket-stream raw-socket))
    (receive-greeting socket (usocket:socket-stream raw-socket))
    (let ((conn-eng (make-instance 'connection-engine :raw-socket raw-socket)))
      (setf (slot-value conn-eng 'out-worker-thunk)
            (mp:process-run-function "out" nil (connection-engine-out-thunk conn-eng))
            (slot-value conn-eng 'in-worker-thunk)
            (mp:process-run-function "in" nil (connection-engine-in-thunk conn-eng)))
      conn-eng)))

(defun add-connection-engine (socket socket-engine)
  (push socket-engine (slot-value socket 'connection-engines)))

(defun make-socket (type &optional id (protocol *protocol-revision*))
  (make-instance 'socket :type-id type
                 :id id
                 :protocol-revision protocol))

(defun send-greeting (socket stream)
  (write-no-interop-signature stream)
  (write-revision-and-socket-type stream
                                  (slot-value socket 'protocol-revision)
                                  (slot-value socket 'type-id))
  (write-identity stream (id socket))
  (force-output stream))

(defun receive-greeting (socket stream)
  (read-signature stream)
  (read-revision stream)
  (let ((type-id (read-socket-type stream)))
    (assert (valid-socket-type-id-p type-id)
            nil 'invalid-socket-type-id)
    (assert (valid-socket-type-pattern-p type-id (slot-value socket 'type-id))
            nil 'invalid-socket-type-pattern-error))
  (read-identity stream))

(defun connect (socket host port)
  (add-connection-engine socket (make-connection-engine socket host port)))

(defun disconnect (socket))

(defun send (socket message &optional more?)
  (mp:mailbox-send (slot-value (car (slot-value socket 'connection-engines)) 'out-bound)
                   (cons message more?)))

(defun receive (socket)
  (mp:mailbox-read (slot-value (car (slot-value socket 'connection-engines)) 'in-bound)))
