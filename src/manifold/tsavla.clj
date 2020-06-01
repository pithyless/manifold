(ns ^{:author "Ryan Smith"
      :doc    "Provide a variant of `core.async/go` that works with manifold's deferreds and executors. Utilizes core.async's state-machine generator, so core.async must be available as a dependency."}
  manifold.tsasvla
  (:require [manifold
             [executor :as ex]
             [deferred :as d]]
            [clojure.core.async.impl
             [ioc-macros :as ioc]]))

(defn return-deferred [state value]
  (let [d (ioc/aget-object state ioc/USER-START-IDX)]
    (d/success! d value)
    d))

(defn <!-no-throw
  "takes value from deferred. Must be called inside a (go ...) block. Will
  return nil if closed. Will park if nothing is available. If an error
  is thrown inside the body, that error will be placed as the return value.

  N.B. To make `tsasvla` usage idiomatic with the rest of manifold, use `<!?`
  instead of this directly."
  [port]
  (assert nil "<! used not in (tsasvla ...) block"))

(defmacro <!?
  "takes a val from port. Must be called inside a (tsasvla ...) block.
  Will park if nothing is available. If value that is returned is
  a Throwable, will re-throw."
  [port]
  `(let [r# (<!-no-throw ~port)]
     (if (instance? Throwable r#)
       ;; this is a re-throw of the original throwable. the expectation is that
       ;; it still will maintain the original stack trace
       (throw r#)
       r#)))

(defn run-state-machine-wrapped [state]
  (try (ioc/run-state-machine state)
       (catch Throwable ex
         (d/error! (ioc/aget-object state ioc/USER-START-IDX) ex)
         (throw ex))))

(defn take! [state blk d]
  (let [handler (fn [x]
                  (ioc/aset-all! state ioc/VALUE-IDX x ioc/STATE-IDX blk)
                  (run-state-machine-wrapped state))
        d'      (-> d
                    (d/chain handler)
                    (d/catch handler))]
    (when (d/realized? d')
      :recur)))

(def async-custom-terminators
  {'manifold.tsasvla/<!-no-throw `manifold.tsasvla/take!
   :Return                      `return-deferred})

(defmacro tsasvla
  "Asynchronously executes the body, returning immediately to the
  calling thread. Additionally, any visible calls to <!? and <!-no-throw
  deferred operations within the body will block (if necessary) by
  'parking' the calling thread rather than tying up an OS thread.
  Upon completion of the operation, the body will be resumed.

  Returns a deferred which will receive the result of the body when
  completed. If the body returns a deferred, the result will be unwrapped
  until a non-deferable value is available to be placed onto the return deferred.

  This method is very similar to `core.async/go`, and even uses underlying functions
  of core.async to implement the state machine, but has some slightly different
  semantics to make it's usage more \"as expected\" with the rest of manifold. The name
  წასვლა is Georgian for \"to go\"."
  [& body]
  (let [crossing-env (zipmap (keys &env) (repeatedly gensym))]
    `(let [d#                 (d/deferred)
           captured-bindings# (clojure.lang.Var/getThreadBindingFrame)]
       (.execute (ex/execute-pool) ^Runnable
                 (^:once fn* []
                   (let [~@(mapcat (fn [[l sym]] [sym `(^:once fn* [] ~(vary-meta l dissoc :tag))]) crossing-env)
                         f# ~(ioc/state-machine `(do ~@body) 1 [crossing-env &env] async-custom-terminators)
                         state# (-> (f#)
                                    (ioc/aset-all! ioc/USER-START-IDX d#
                                                   ioc/BINDINGS-IDX captured-bindings#))]
                     (run-state-machine-wrapped state#))))
       ;; chain is8 being used to apply unwrap chain
       (d/chain d#))))
