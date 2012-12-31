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

       (defun valid-socket-type-p (socket-id)
         (and (>= socket-id 0) (<= socket-id ,max-socket-id)))

       (defparameter *socket-assoc-list*
         ',(loop
            :for socket-type :in socket-types
            :collect (cons (cdr (assoc (car socket-type) socket-values))
                           (list (mapcar (lambda (mapping)
                                           (cdr (assoc mapping socket-values)))
                                         (cadr socket-type))))))

       (defun valid-socket-pattern-p (socket-1 socket-2)
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
(define-condition invalid-socket-id-error (error) nil)
(define-condition message-body-too-long-error (error) nil)

(defun write-signature (stream &optional (identity-length 0))
  (assert (and (>= identity-length 0)
               (<= identity-length 255))
          nil 'invalid-identity-length-error)
  (write-sequence (make-array 10
                              :element-type '(unsigned-byte 8)
                              :initial-contents `(#xff 0 0 0 0 0 0 ,(ash (1+ identity-length) -8)
                                                       ,(boole boole-and #xff (1+ identity-length))
                                                       #x7f))
                  stream)
  (force-output stream))

(defun write-revision-and-socket-type (stream revision socket-type)
  (assert (valid-socket-p socket-type) nil 'invalid-socket-id-error)
  (write-byte revision stream)
  (write-byte socket-type stream)
  (force-output stream))

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
    (when body (write-sequence body stream))
    (unless more (force-output stream))))

(defun read-length-from-octets (sequence start count)
  (loop
   :for index :from start :below (+ start count)
   :for shift :from (* (1- count) 8) :downto 0 :by 8
   :summing (ash (aref sequence index) shift)))

(defun read-signature (stream)
  (let ((sig (make-array 10 :element-type '(unsigned-byte 8))))
    (assert (= (read-sequence sig stream) 10))
    (assert (= (aref sig 0) #xff))
    (assert (= (aref sig 9) #x7f))
    (let ((identity-length (read-length-from-octets sig 1 8)))
      (assert (and (>= identity-length 1)
                   (<= identity-length 256))))
    sig))

(defun read-revision (stream)
  (read-byte stream))

(defun read-socket-type (stream)
  (read-byte stream))

(defun read-identity (stream)
  (assert (zerop (read-byte stream)))
  (let* ((identity-length (read-byte stream))
         (identity (make-array identity-length :element-type '(unsigned-byte 8))))
    (assert (= (read-sequence identity stream) identity-length))
    (values identity identity-length)))

(defun read-frame (stream)
  (let* ((flags (read-byte stream))
         (more? (= (boole boole-and flags *more-frames-flag*) 1))
         (long? (= (boole boole-and flags *long-message-flag*) 1))
         (length (if long?
                     nil
                     (read-byte stream)))
         (body (make-array length :element-type '(unsigned-byte 8))))
    (assert (= (read-sequence body stream) length))
    (values body length more?)))

(defun test-run (str)
  (format t ">~x~%" (read-signature str))
  (write-signature str)
  (format t ">~x~%" (read-revision str))
  (format t ">~x~%" (read-socket-type str))
  (write-revision-and-socket-type str 1 *req*)
  (format t ">~x~%" (read-identity str))
  (write-frame str #())
  (force-output str)
  (write-frame str #() t)
  (write-frame str #(102 111 111))
  (force-output str)
  (format t ">~x~%" (read-frame str))
  (format t ">~x~%" (read-frame str)))
