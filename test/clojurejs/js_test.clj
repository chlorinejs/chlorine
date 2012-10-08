(ns clojurejs.js-test
  (:use [clojurejs.js]
        [clojure.test]))

(deftest unzip-test
  (is (= (unzip [:foo 1 :bar 2 :baz 3])
         [[:foo :bar :baz] [1 2 3]])))

(tojs "src/clojurejs/boot.cljs")

(deftest literals
  (is (= (js *print-pretty*) "__print_pretty__"))
  (is (= (js number?) "numberp"))
  (is (= (js foo-bar-baz) "foo_bar_baz"))
  (is (= (js inc!) "incf"))
  (is (= (js {:foo 1 :bar 2 :baz 3}) "{'foo' : 1,'bar' : 2,'baz' : 3}"))
  (is (= (js #{:foo :bar :baz}) "{'foo' : true,'bar' : true,'baz' : true}"))
  (is (= (js [:foo :bar :baz]) "['foo','bar','baz']"))
  (is (= (js #"^([a-z]*)([0-9]*)") "/^([a-z]*)([0-9]*)/"))
  (is (= (js \newline) "'\n'"))
  (is (= (js \a) "'a'")))

(deftest functions
  (is (= (js (+ 1 2 3)) "(1 + 2 + 3)"))
  (is (= (js (+ "foo" "bar" "baz")) "(\"foo\" + \"bar\" + \"baz\")"))
  (is (= (js (:test {:test 1 :foo 2 :bar 3}))
         "{'test' : 1,'foo' : 2,'bar' : 3}['test']"))
  (is (= (js (let [m {:test 1 :foo 2 :bar 3}] (:baz m 4)))
         (str "var m = {'test' : 1,'foo' : 2,'bar' : 3};"
              " ('baz' in m ? m['baz'] : 4);")))
  (is (= (js (append '(:foo bar baz) '(quux)))
         "append(['foo','bar','baz'], ['quux'])"))

  (is (= (js (fn [a b] (+ a b)))
         "function (a, b) { return (a + b); }"))

  (is (= (with-pretty-print (js (fn "Some func does stuff" [x] (+ x 1))))
         (str "function (x) {\n"
              "    /* Some func does stuff */\n"
              "    return (x + 1);\n}")))

  (is (= (with-pretty-print (js (fn "Some func\ndoes stuff" [x] (+ x 1))))
         (str "function (x) {\n"
              "    /* Some func\n"
              "       does stuff */\n"
              "    return (x + 1);\n}")))

  (is (= (js (defn foo [a b] (+ a b)))
         "foo = function (a, b) { return (a + b); }"))

  (is (= (js (defn foo [c] (.methodOf c)))
         "foo = function (c) { return c.methodOf(); }"))

  (is (= (js
          (defn test []
            (let [a 1] (log (* a a)))
            (let [a 2] (log (* a a)))))
         (str "test = function () { var a = 1;"
              " log((a * a));;"
              " var a = 2;"
              " return log((a * a));; }")))
  (is (= (js
          (defn test []
            (let [a 1] (log (* a a)))
            (do (log "test") (+ 1 1))))
         (str "test = function () {"
              " var a = 1;"
              " log((a * a));;"
              "  log(\"test\");"
              " return (1 + 1);; }"))))

(deftest property-access
  (is (= (js (get map :key))
         "map['key']"))
  (is (= (js (:key map))
         "map['key']"))
  (is (= (js (get map .key))
         "map.key")))

(deftest property-access-default
  (is (= (js (get map :key default))
         "('key' in map ? map['key'] : default)"))

  (is (= (js (get map .key default))
         "('key' in map ? map.key : default)")))