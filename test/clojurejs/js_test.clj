(ns clojurejs.js-test
  (:use [clojurejs.js]
        [clojure.test]))

(deftest unzip-test
  (is (= (unzip [:foo 1 :bar 2 :baz 3])
         [[:foo :bar :baz] [1 2 3]])))