(ns manifold.go-test
  (:require [clojure.test :refer :all]
            [manifold.go :refer [go <!?]]
            [manifold.deferred :as d])
  (:import (java.util.concurrent TimeoutException)))

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
             @async-chan)))

    (is (= 3
           (let [d1 (d/deferred)
                 d2 (d/deferred)
                 d3 (d/deferred)
                 async-chan (go (+ (<!? d1) (<!? d2) (<!? d3)))]
             (d/success! d3 1)
             (d/success! d2 1)
             (d/success! d1 1)
             @async-chan)))))

(deftest go-nests
  (testing "return deferred will always result in a a realizable value, not another deferred"
    (is (= [23 42] @(go (let [let* 1 a 23] (go (let* [b 42] [a b]))))))
    (is (= 5 @(go (go (go (go (go (go (go 5))))))))))
  (testing "Parking unwraps nested deferreds"
    (is (= 5 @(go (<!? (go (go (go 5)))))))))

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

(deftest non-deferred-takes
  (testing "Can take from non-deffereds"
    (is (= 5 @(go (<!? 5))))
    (is (= "test" @(go (<!? "test"))))))

(deftest already-realized-values
  (testing "When taking from already realized values, the threads should not change."
    (let [original-thread (atom nil)]
      (is (= @(go (reset! original-thread (Thread/currentThread))
                  (<!? "cat")
                  (Thread/currentThread))
             @original-thread)))

    (let [original-thread (atom nil)]
      (is (= @(go (reset! original-thread (Thread/currentThread))
                  (<!? (d/success-deferred "cat"))
                  (Thread/currentThread))
             @original-thread)))))

(deftest deferred-interactions
  (testing "timeouts"
    (is (= ::timeout @(go (<!? (d/timeout! (d/deferred) 10 ::timeout)))))
    (is (= ::timeout @(d/timeout! (go (<!? (d/deferred))) 10 ::timeout)))
    (is (thrown? TimeoutException @(go (<!? (d/timeout! (d/deferred) 10)))))
    (is (thrown? TimeoutException @(d/timeout! (go (<!? (d/deferred))) 10))))

  (testing "alt"
    (is (= ::timeout @(go (<!? (d/alt (d/deferred) (d/timeout! (d/deferred) 10 ::timeout))))))
    (is (= ::timeout @(d/alt (go (<!? (d/deferred))) (d/timeout! (d/deferred) 10 ::timeout))))
    (is (= 1 @(go (<!? (d/alt (d/deferred) (d/success-deferred 1))))))
    (is (= 1 @(d/alt (go (<!? (d/deferred))) (d/success-deferred 1))))))
