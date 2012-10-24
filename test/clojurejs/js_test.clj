(ns clojurejs.js-test
  (:use [clojurejs.js]
        [clojure.test]
        [clojurejs.util]))

(dosync (ref-set *macros* {}))

(tojs (resource-file "private/boot.cljs"))

(deftest literals
  (is (= (js *print-pretty*) "__print_pretty__"))
  (is (= (js number?) "number___p"))
  (is (= (js foo-bar-baz) "foo_bar_baz"))
  (is (= (js inc!) "inc___f"))
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

  (is (= (js (fn foo [a b] (+ a b)))
         "function foo (a, b) { return (a + b); }"))

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

(deftest destructuring
  (is (= (js
          (defn test []
            (let [a 1
                  b (+ a 1)
                  c (+ b 1)]
              (+ a b c))))
         (str "test = function () {"
              " var a = 1, b = (a + 1), c = (b + 1);"
              " return (a + b + c);; }")))

  ;; & rest
  (is (= (js
          (defn test []
            (let [[a b & r] [1 2 3 4]]
              [(+ a b) r])))
         (str "test = function () {"
              " var _temp_1000 = [1,2,3,4],"
              " a = _temp_1000[0],"
              " b = _temp_1000[1],"
              " r = _temp_1000.slice(2);"
              " return [(a + b),r];; }")))

  (is (= (js
          (defn test [[a b & r]]
            [(+ a b) r]))
         (str "test = function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " _temp_1001 = _temp_1000[0],"
              " a = _temp_1001[0],"
              " b = _temp_1001[1],"
              " r = _temp_1001.slice(2);"
              " return [(a + b),r]; }")))

  (is (= (js
          (defn test [a b & r]
            [(+ a b) r]))
         (str "test = function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " a = _temp_1000[0],"
              " b = _temp_1000[1],"
              " r = _temp_1000.slice(2);"
              " return [(a + b),r]; }")))

  ;; :as
  (is (= (js
          (fn [a [b] [c d & e :as f] :as g] nil))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " a = _temp_1000[0],"
              " _temp_1001 = _temp_1000[1],"
              " b = _temp_1001[0],"
              " _temp_1002 = _temp_1000[2],"
              " c = _temp_1002[0],"
              " d = _temp_1002[1],"
              " e = _temp_1002.slice(2),"
              " f = _temp_1002,"
              " g = _temp_1000;"
              " return null; }")))

  ;; map destructuring
  (is (= (js
          (fn [x {y :y, fred :fred}] fred))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " x = _temp_1000[0],"
              " _temp_1001 = _temp_1000[1],"
              " y = _temp_1001['y'],"
              " fred = _temp_1001['fred'];"
              " return fred; }")))

  (is (= (js
          (fn [[{x :x, {z :z} :y}]] z))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " _temp_1001 = _temp_1000[0],"
              " _temp_1002 = _temp_1001[0],"
              " x = _temp_1002['x'],"
              " _temp_1003 = _temp_1002['y'],"
              " z = _temp_1003['z'];"
              " return z; }")))

  ;; numbers as keys (this actually works)
  (is (= (js
          (fn [{a 1, b 2, :or {a 3}}]))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " _temp_1001 = _temp_1000[0],"
              " a = (1 in _temp_1001 ? _temp_1001[1] : 3),"
              " b = _temp_1001[2];"
              " return null; }")))

  ;; :keys, :strs
  (is (= (js
          (fn [x {y :y, z :z :keys [a b]}] z))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " x = _temp_1000[0],"
              " _temp_1001 = _temp_1000[1],"
              " a = _temp_1001['a'],"
              " b = _temp_1001['b'],"
              " y = _temp_1001['y'],"
              " z = _temp_1001['z'];"
              " return z; }")))

  (is (= (js
          (fn [x {y :y, z :z :strs [a b]}] z))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " x = _temp_1000[0], _temp_1001 = _temp_1000[1],"
              " a = _temp_1001['a'],"
              " b = _temp_1001['b'],"
              " y = _temp_1001['y'],"
              " z = _temp_1001['z'];"
              " return z; }")))
                                        ; defaults
  (is (= (js
          (fn [x {y :y, z :z :or {y 1, z "foo"}}] z))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " x = _temp_1000[0],"
              " _temp_1001 = _temp_1000[1],"
              " y = ('y' in _temp_1001 ? _temp_1001['y'] : 1),"
              " z = ('z' in _temp_1001 ? _temp_1001['z'] : \"foo\");"
              " return z; }")))

  (is (= (js
          (fn [x {y :y, z :z :keys [a b] :or {a 1, y :bleh}}] z))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " x = _temp_1000[0],"
              " _temp_1001 = _temp_1000[1],"
              " a = ('a' in _temp_1001 ? _temp_1001['a'] : 1),"
              " b = _temp_1001['b'],"
              " y = ('y' in _temp_1001 ? _temp_1001['y'] : 'bleh'),"
              " z = _temp_1001['z']; return z; }")))

  ;; unsupported for now
  (is (thrown-with-msg? Exception #"& must be followed by"
        (js
         (fn [x y & {z :z}] z)))))

(deftest loops
  (is (= (js
          (defn join [arr delim]
            (loop [str (get arr 0)
                   i 1]
              (if (< i (get arr .length))
                (recur (+ str delim (get arr i))
                       (+ i 1))
                str))))
         (str "join = function (arr, delim) {"
              " for (var str = arr[0], i = 1; true;) {"
              " if ((i < arr.length)) {"
              " var _temp_1000 = [(str + delim + arr[i]),(i + 1)];\n"
              " str = _temp_1000[0];"
              " i = _temp_1000[1];"
              " continue; }"
              " else {"
              " return str; };"
              " break; }; }"))))

(deftest inline-if
  (is (= (js
          (defn test [a]
            ((if (> a 0) minus plus) a 1)))
         "test = function (a) { return (((a > 0) ? minus : plus))(a,1); }"))

  ;; implicit `null` alternate
  (is (= (js (defn test [a] (console.log (if (> a 0) a))))
         "test = function (a) { return console.log(((a > 0) ? a : null)); }")))

(deftest inline-primitives
  (is (= (js (defn isac? [i c] (inline "i instanceof c")))
         "isac___p = function (i, c) { return i instanceof c; }")))

(deftest case-tests
  (is (= (js (case answer 42 (bingo)))
         "switch (answer) { case 42: bingo(); }"))
  (is (= (js (case answer (+ 10 20) (bingo)))
         "switch (answer) { case (10 + 20): bingo(); }"))
  (is (= (js (case answer 1 :one 2 :two :anything-else))
         (str  "switch (answer) {"
               " case 1: 'one';"
               " case 2: 'two';"
               " default: 'anything-else'; }"))))

(deftest try-catch-finally
  (is (= (js
          (defn test []
            (try
              (/ 5 0)
              (catch ex
                  (console.log ex))
              (finally
               0))))
         (str "test = function () {"
              " try { return (5 / 0); } catch (ex) {"
              " return console.log(ex); }"
              " finally {"
              " return 0; }; }")))

  (is (= (js
          (defn test [a]
            (if (< a 0) (throw (new Error "Negative numbers not accepted")))))
         (str "test = function (a) {"
              " if ((a < 0)) {"
              " throw new Error(\"Negative numbers not accepted\"); }; }"))))

(deftest while-test
  (is (= (js
           (while (< x 10)
             (inc! x)))
         "while (x < 10) { x = (1 + x); }"))
  (is (= (js
           (while (and (< x 10) (> x 5))
             (inc! x)))
         "while ((x < 10) && (x > 5)) { x = (1 + x); }")))

(deftest js-let-test
  (is (= (js-let [a 2 b 3] (+ a b))
         " (function (a, b) { return (a + b); })(2,3);")))

(deftest do-while-test
  (is (= (js
           (do-while (< x 10)
             (inc! x)))
         "do { x = (1 + x); } while (x < 10)"
         ))
  (is (= (js
           (do-while (and (< x 10) (> x 5))
             (inc! x)))
         "do { x = (1 + x); } while ((x < 10) && (x > 5))")))

(deftest jfor-test
  (is (= (js
           (jfor [(lvar i 0
                        j 1)
                  (< i 5)
                  (inc! i)]
                 1))
         "for (var i = 0,j = 1;(i < 5);i = (1 + i)) { 1; }"))
  (is (= (js
           (jfor [(def i 0)
                  (< i 5)
                  (inc! i)]
                 1))
         "for (var i = 0;(i < 5);i = (1 + i)) { 1; }"))
  (is (= (js
           (jfor [[i 0 j 1]
                  (< i 5)
                  (inc! i)]
                 1)))))

(deftest let-js-test
  (is (= (let-js [foo 1]
                 `(def x ~foo))
         "var x = 1")))
