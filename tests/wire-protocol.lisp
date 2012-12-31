
(in-package #:nilmq-tests)
(def-suite :wire-protocol-tests)
(in-suite :wire-protocol-tests)

(def-fixture dummy-out-stream ()
  (flexi-streams:with-output-to-sequence (stream)
    (&body)))

(def-fixture dummy-in-stream (sequence)
  (flexi-streams:with-input-from-sequence (stream sequence)
    (&body)))

(test :write-signature-with-no-identity
  "Writes signature for socket where no identity has been set"
  (is (equalp (with-fixture dummy-out-stream ()
                (nilmq::write-signature stream))
              #(#xff 0 0 0 0 0 0 0 1 #x7f))))

(test :write-signature-with-minimum-identity-length
  "Writes signature for socket with minimum identity length"
  (is (equalp (with-fixture dummy-out-stream ()
                (nilmq::write-signature stream 1))
              #(#xff 0 0 0 0 0 0 0 2 #x7f))))

(test :write-signature-with-maximum-identity-length
  "Writes signature for socket with maximum identity length"
  (is (equalp (with-fixture dummy-out-stream ()
                (nilmq::write-signature stream 255))
              #(#xff 0 0 0 0 0 0 1 0 #x7f))))


(test :write-signature-with-identity-length-less-than-zero
  "Writes signature for socket with identity length of less than zero"
  (signals nilmq::invalid-identity-length-error
    (with-fixture dummy-out-stream ()
      (nilmq::write-signature stream -1))))

(test :write-signature-with-identity-greater-than-maximum-length
  "Writes signature for socket with identity length of greater than the allowed maximum"
  (signals nilmq::invalid-identity-length-error
    (with-fixture dummy-out-stream ()
      (nilmq::write-signature stream 256))))

(test :write-revision-and-socket-type-with-valid-socket-id
  "Writes revision and socket type using a valid socket id"
  (is (equalp (with-fixture dummy-out-stream ()
                (nilmq::write-revision-and-socket-type stream
                                                       nilmq::*protocol-revision*
                                                       nilmq::*req*))
              `#(,nilmq::*protocol-revision* ,nilmq::*req*))))

(test :write-revision-and-socket-type-with-invalid-socket-id
  "Writes revision and socket type using a invalid socket id"
  (signals nilmq::invalid-socket-id-error
    (with-fixture dummy-out-stream ()
      (nilmq::write-revision-and-socket-type stream nilmq::*protocol-revision* -1))))

(test :write-frame-with-nil-body
  "Write frame with nil body"
  (is (equalp (with-fixture dummy-out-stream ()
                (nilmq::write-frame stream nil))
              #(0 0))))

(test :write-frame-with-zero-length-body
  "Write frame with nil body"
  (is (equalp (with-fixture dummy-out-stream ()
                (nilmq::write-frame stream #()))
              #(0 0))))

(test :write-frame-with-short-body
  "Write frame with short body"
  (is (equalp (with-fixture dummy-out-stream ()
                (nilmq::write-frame stream #(1 2 3)))
              #(0 3 1 2 3))))

(test :write-frame-with-max-length-short-body
  "Write frame with maximum length for short body message"
  (let ((expected (make-array 257
                              :initial-element 1
                              :element-type '(unsigned-byte 8))))
    (setf (aref expected 0) 0
          (aref expected 1) 255)
    (is (equalp (with-fixture dummy-out-stream ()
                  (nilmq::write-frame stream (make-array 255
                                                         :initial-element 1
                                                         :element-type '(unsigned-byte 8))))
                expected))))

(test :write-large-body-length-test
  "Test the WRITE-LARGE-BODY-LENGTH function with minimum large message length value"
  (is (equalp (with-fixture dummy-out-stream ()
                (nilmq::write-large-body-length 256 stream))
              #(0 0 0 0 0 0 1 0))))

(test :write-large-body-length-maximum-length-test
  "Test the WRITE-LARGE-BODY-LENGTH function with maximum large message length value"
  (is (equalp (with-fixture dummy-out-stream ()
                (nilmq::write-large-body-length (1- (expt 2 64)) stream))
              #(255 255 255 255 255 255 255 255))))

(test :write-large-body-length-too-long
  "Test the WRITE-LARGE-BODY-LENGTH function with too large of a large message length value"
  (signals nilmq::message-body-too-long-error
    (with-fixture dummy-out-stream ()
      (nilmq::write-large-body-length (expt 2 64) stream))))

(test :write-frame-with-min-length-long-body
  "Write frame with minimum length for a long body message"
  (let ((expected (make-array 265
                              :initial-element 1
                              :element-type '(unsigned-byte 8))))
    (loop
     :for val :in `(,nilmq::*long-message-flag* 0 0 0 0 0 0 1 0)
     :for index :from 0
     :do (setf (aref expected index) val))
    (is (equalp (with-fixture dummy-out-stream ()
                  (nilmq::write-frame stream (make-array 256
                                                         :initial-element 1
                                                         :element-type '(unsigned-byte 8))))
                expected))))

(test :read-length-from-octets-test-reading-one-octets
  "Test READ-LENGTH-FROM-OCTETS reading length from one octets"
  (is (= (nilmq::read-length-from-octets #(1) 0 1)
         1)))

(test :read-length-from-octets-test-reading-more-than-one-octets
  "Test READ-LENGTH-FROM-OCTETS reading length from more than one octets"
  (is (= (nilmq::read-length-from-octets #(1 0) 0 2)
         256)))

(test :read-length-from-octets-test-reading-no-octets
  "Test READ-LENGTH-FROM-OCTETS reading length from no octets"
  (is (zerop (nilmq::read-length-from-octets #(1 0) 0 0))))

(test :read-length-from-octets-test-reading-eigth-octets
  "Test READ-LENGTH-FROM-OCTETS reading eight octets"
  (is (= (nilmq::read-length-from-octets #(255 255 255 255 255 255 255 255) 0 8)
         (1- (expt 2 64)))))

(test :read-length-from-octets-test-reading-octets-from-non-zero-start
  "Test READ-LENGTH-FROM-OCTETS reading octets from non zero start in sequence"
  (is (= (nilmq::read-length-from-octets #(255 255 255 255 255 255 255 255) 2 3)
         (1- (expt 2 24)))))


(test :read-signature-with-zero-length-identity
  "Read signature with zero length identity"
  (is (equalp (with-fixture dummy-in-stream (#(#xff 0 0 0 0 0 0 0 1 #x7f))
                (nilmq::read-signature stream))
              #(#xff 0 0 0 0 0 0 0 1 #x7f))))

(test :read-signature-with-max-length-identity
  "Read signature with maximum length identity"
  (is (equalp (with-fixture dummy-in-stream (#(#xff 0 0 0 0 0 0 1 0 #x7f))
                (nilmq::read-signature stream))
              #(#xff 0 0 0 0 0 0 1 0 #x7f))))

(test :read-signature-with-invalid-identity-length
  "Read signature with invalid identity length"
  (signals nilmq::invalid-signature-error
    (with-fixture dummy-in-stream (#(#xff 0 0 0 0 0 0 2 0 #x7f))
      (nilmq::read-signature stream))))

(test :read-signature-with-invalid-identity-length-of-zero
  "Read signature with invalid identity length of zero"
  (signals nilmq::invalid-signature-error
    (with-fixture dummy-in-stream (#(#xff 0 0 0 0 0 0 0 0 #x7f))
      (nilmq::read-signature stream))))

(test :read-signature-with-invalid-lead-octet
  "Read signature with invalid lead octet (#xFF)"
  (signals nilmq::invalid-signature-error
    (with-fixture dummy-in-stream (#(#xf0 0 0 0 0 0 0 1 0 #x7f))
      (nilmq::read-signature stream))))

(test :read-signature-with-invalid-trailing-octet
  "Read signature with invalid lead octet (#xFF)"
  (signals nilmq::invalid-signature-error
    (with-fixture dummy-in-stream (#(#xff 0 0 0 0 0 0 1 0 #x0f))
      (nilmq::read-signature stream))))

(test :read-identity-of-zero-length-check-identity
  "Read zero length identity checking identity"
  (is (multiple-value-bind (identity length)
          (with-fixture dummy-in-stream (#(0 0))
            (nilmq::read-identity stream))
        (declare (ignore length))
        (equalp identity #()))))

(test :read-identity-of-zero-length-check-length
  "Read zero length identity checking length"
  (is (multiple-value-bind (identity length)
          (with-fixture dummy-in-stream (#(0 0))
            (nilmq::read-identity stream))
        (declare (ignore identity))
        (zerop length))))

(test :read-identity-with-identity-check-identity
  "Read identity with non-zero length identity"
  (is (multiple-value-bind (identity length)
          (with-fixture dummy-in-stream (#(0 3 65 65 65))
            (nilmq::read-identity stream))
        (declare (ignore length))
        (equalp identity #(65 65 65)))))

(test :read-identity-with-identity-check-length
  "Read non-zero length identity checking length"
  (is (multiple-value-bind (identity length)
          (with-fixture dummy-in-stream (#(0 3 65 65 65))
            (nilmq::read-identity stream))
        (declare (ignore identity))
        (= length 3))))

(test :read-identity-with-invalid-length
  "Read identity with invalid length"
  (signals nilmq::invalid-identity-error
    (with-fixture dummy-in-stream (#(0 4 65 65 65))
      (nilmq::read-identity stream))))

(test :read-identity-with-invalid-final-short
  "Read identity with invalid final short"
  (signals nilmq::invalid-identity-error
    (with-fixture dummy-in-stream (#(1 3 65 65 65))
      (nilmq::read-identity stream))))

(test :read-identity-with-invalid-final-short-too-short
  "Read identity with too short final short"
  (signals nilmq::invalid-identity-error
    (with-fixture dummy-in-stream (#(0))
      (nilmq::read-identity stream))))
