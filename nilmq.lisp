;;;; nilmq.lisp

(defpackage #:nilmq
  (:use #:cl))

(in-package #:nilmq)

(defparameter *more-frames-flag* 1)
(defparameter *long-message-flag* 2)

(defparameter *pair* #x0)
(defparameter *pub* #x1)
(defparameter *sub* #x2)
(defparameter *req* #x3)
(defparameter *rep* #x4)
(defparameter *dealer* #x5)
(defparameter *router* #x6)
(defparameter *pull* #x7)
(defparameter *push* #x8)

(defparameter *protocol-revision* #x1)

(defun write-signature (stream &optional (identity-length 1))
  (write-sequence (make-array 10
                              :element-type '(unsigned-byte 8)
                              :initial-contents `(#xff 0 0 0 0 0 0 0
                                                       ,identity-length
                                                       #x7f))
                  stream)
  (force-output stream))

(defun write-revision-and-socket-type (stream revision socket-type)
  (write-byte revision stream)
  (write-byte socket-type stream)
  (force-output stream))

(defun write-frame (stream body &optional more)
  (write-byte (+ (if more 1 0)
                 (if (> (length body) 255) 2 0))
              stream)
  (write-byte (length body) stream)
  (write-sequence body stream)
  #++(unless more (force-output stream)))

(defun read-signature (stream)
  (let ((sig (make-array 10 :element-type '(unsigned-byte 8))))
    (assert (= (read-sequence sig stream) 10))
    (assert (= (aref sig 0) #xff))
    (assert (= (aref sig 9) #x7f))
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
