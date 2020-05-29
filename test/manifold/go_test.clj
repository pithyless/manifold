(ns manifold.go-test
  (:require [clojure.test :refer :all]
            [manifold.go :refer [go <!?]]
            [manifold.deferred :as d]))

(deftest async-test
  (testing "values are returned correctly"
    (is (= 10
           @(go (<!? (d/success-deferred 10))))))

  (testing "case with go"
    (is (= :1
           @(go (case (name :1)
                       "0" :0
                       "1" :1
                       :3)))))

  (testing "nil result of go"
    (is (= nil
           @(go nil))))

  (testing "take inside binding of loop"
    (is (= 42
           @(go (loop [x (<!? (d/success-deferred 42))]
                       x)))))

  (testing "can get from a catch"
    (let [c (d/success-deferred 42)]
      (is (= 42
             @(go (try
                         (assert false)
                         (catch Throwable ex (<!? c)))))))))

(deftest enqueued-chan-ops
  (testing "enqueued channel takes re-enter async properly"
    (is (= :foo
           (let [d          (d/deferred)
                 async-chan (go (<!? d))]
             (d/success! d :foo)
             @async-chan)))))

(deftest go-nests
  (is (= [23 42] @@(go (let [let* 1 a 23] (go (let* [b 42] [a b])))))))

;; TODO: Tests for correct error handling

(deftest error-propagation
  (is (= "chained catch"
         @(d/catch (go (/ 5 0))
                  (constantly "chained catch"))))

  (is (= "try/catch in block"
         @(go (try (/ 5 0)
                   (catch Throwable _ "try/catch in block")))))

  (testing "Try/catch around parking will continue block"
    (is (= "try/catch parking"
           @(go (try (<!? (d/future (/ 5 0)))
                     (catch Throwable _ "try/catch parking")))))
    (is (= 5
           @(go (try (<!? (d/future (/ 5 0)))
                     (catch Throwable _))
                5))))

  (testing "Normal deferred handling still works"
    (is (= 5
           @(go (<!? (d/catch (d/future (/ 5 0)) (constantly 5))))))))
