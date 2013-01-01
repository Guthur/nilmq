(in-package #:nilmq-tests)

(def-suite :socket-type-id-tests)
(in-suite :socket-type-id-tests)

(test :test-valid-socket-type-id-p-with-valid-socket-types
  "Tests the VALID-SOCKET-TYPE-ID-P function with valid socket-types"
  (is (nilmq::valid-socket-type-id-p nilmq::*pair*))
  (is (nilmq::valid-socket-type-id-p nilmq::*pub*))
  (is (nilmq::valid-socket-type-id-p nilmq::*sub*))
  (is (nilmq::valid-socket-type-id-p nilmq::*req*))
  (is (nilmq::valid-socket-type-id-p nilmq::*rep*))
  (is (nilmq::valid-socket-type-id-p nilmq::*dealer*))
  (is (nilmq::valid-socket-type-id-p nilmq::*router*))
  (is (nilmq::valid-socket-type-id-p nilmq::*pull*))
  (is (nilmq::valid-socket-type-id-p nilmq::*push*)))

(test :test-valid-socket-type-id-p-with-invalid-socket-types
  "Tests the VALID-SOCKET-TYPE-ID-P function with invalid socket-types"
  (is-false (nilmq::valid-socket-type-id-p -1))
  (is-false (nilmq::valid-socket-type-id-p 9)))

(test :test-valid-socket-type-pattern-p-with-valid-socket-combinations
  "Tests the VALID-SOCKET-TYPE-PATTERN-P function with valid socket-type combinations"
  (is (nilmq::valid-socket-type-pattern-p nilmq::*pair* nilmq::*pair*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*pub* nilmq::*sub*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*sub* nilmq::*pub*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*req* nilmq::*rep*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*req* nilmq::*router*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*rep* nilmq::*req*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*rep* nilmq::*dealer*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*dealer* nilmq::*rep*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*dealer* nilmq::*dealer*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*dealer* nilmq::*router*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*router* nilmq::*req*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*router* nilmq::*dealer*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*router* nilmq::*router*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*pull* nilmq::*push*))
  (is (nilmq::valid-socket-type-pattern-p nilmq::*push* nilmq::*pull*)))

(test :test-valid-socket-type-pattern-p-with-invalid-socket-combinations
  "Tests the VALID-SOCKET-TYPE-PATTERN-P function with invalid socket-type combinations"
  (is-false (nilmq::valid-socket-type-pattern-p nilmq::*req* nilmq::*pull*))
  (is-false (nilmq::valid-socket-type-pattern-p nilmq::*dealer* nilmq::*pull*))
  (is-false (nilmq::valid-socket-type-pattern-p nilmq::*pair* nilmq::*pub*))
  (is-false (nilmq::valid-socket-type-pattern-p nilmq::*sub* nilmq::*router*)))
