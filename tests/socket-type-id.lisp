(in-package #:nilmq-tests)

(def-suite :socket-type-id-tests)
(in-suite :socket-type-id-tests)

(test :test-valid-socket-p-with-valid-socket-types
  "Tests the VALID-SOCKET-P function with valid socket-types"
  (is (nilmq::valid-socket-p nilmq::*pair*))
  (is (nilmq::valid-socket-p nilmq::*pub*))
  (is (nilmq::valid-socket-p nilmq::*sub*))
  (is (nilmq::valid-socket-p nilmq::*req*))
  (is (nilmq::valid-socket-p nilmq::*rep*))
  (is (nilmq::valid-socket-p nilmq::*dealer*))
  (is (nilmq::valid-socket-p nilmq::*router*))
  (is (nilmq::valid-socket-p nilmq::*pull*))
  (is (nilmq::valid-socket-p nilmq::*push*)))

(test :test-valid-socket-p-with-invalid-socket-types
  "Tests the VALID-SOCKET-P function with invalid socket-types"
  (is-false (nilmq::valid-socket-p -1))
  (is-false (nilmq::valid-socket-p 9)))

(test :test-valid-socket-pattern-p-with-valid-socket-combinations
  "Tests the VALID-SOCKET-PATTERN-P function with valid socket-type combinations"
  (is (nilmq::valid-socket-pattern-p nilmq::*pair* nilmq::*pair*))
  (is (nilmq::valid-socket-pattern-p nilmq::*pub* nilmq::*sub*))
  (is (nilmq::valid-socket-pattern-p nilmq::*sub* nilmq::*pub*))
  (is (nilmq::valid-socket-pattern-p nilmq::*req* nilmq::*rep*))
  (is (nilmq::valid-socket-pattern-p nilmq::*req* nilmq::*router*))
  (is (nilmq::valid-socket-pattern-p nilmq::*rep* nilmq::*req*))
  (is (nilmq::valid-socket-pattern-p nilmq::*rep* nilmq::*dealer*))
  (is (nilmq::valid-socket-pattern-p nilmq::*dealer* nilmq::*rep*))
  (is (nilmq::valid-socket-pattern-p nilmq::*dealer* nilmq::*dealer*))
  (is (nilmq::valid-socket-pattern-p nilmq::*dealer* nilmq::*router*))
  (is (nilmq::valid-socket-pattern-p nilmq::*router* nilmq::*req*))
  (is (nilmq::valid-socket-pattern-p nilmq::*router* nilmq::*dealer*))
  (is (nilmq::valid-socket-pattern-p nilmq::*router* nilmq::*router*))
  (is (nilmq::valid-socket-pattern-p nilmq::*pull* nilmq::*push*))
  (is (nilmq::valid-socket-pattern-p nilmq::*push* nilmq::*pull*)))

(test :test-valid-socket-pattern-p-with-invalid-socket-combinations
  "Tests the VALID-SOCKET-PATTERN-P function with invalid socket-type combinations"
  (is-false (nilmq::valid-socket-pattern-p nilmq::*req* nilmq::*pull*))
  (is-false (nilmq::valid-socket-pattern-p nilmq::*dealer* nilmq::*pull*))
  (is-false (nilmq::valid-socket-pattern-p nilmq::*pair* nilmq::*pub*))
  (is-false (nilmq::valid-socket-pattern-p nilmq::*sub* nilmq::*router*)))