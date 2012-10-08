(ns clojurejs.test
  (:use [clojurejs.js]
        [evaljs.core]
        [evaljs.rhino]))

(def boot-js (tojs "src/clojurejs/boot.cljs"))

(defmacro js-eval [& body]
  `(with-context (rhino-context)
             (evaljs (js ~@body))))

(defmacro js-eval-with-boot-js [& body]
  `(with-context (rhino-context)
             (evaljs (str boot-js (js ~@body)))))
