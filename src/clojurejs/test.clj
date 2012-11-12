(ns clojurejs.test
  (:use [clojurejs.js]
        [evaljs.core]
        [evaljs.rhino]
        [clojurejs.util]))

(def boot-js
  (str (tojs [:private "boot.cljs"])))

(defmacro load-boot []
  `(tojs [:private "boot.cljs"]))

(defmacro js-eval [& body]
  `(with-context (rhino-context)
             (evaljs (js ~@body))))

(defmacro js-eval-context [m & body]
  `(with-context (rhino-context m)
             (evaljs (js ~@body))))

(defmacro js-eval-with-boot-js [& body]
  `(with-context (rhino-context)
             (evaljs (str boot-js (js ~@body)))))