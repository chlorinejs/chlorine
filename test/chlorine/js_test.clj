(ns chlorine.js-test
  (:use [chlorine.js]
        [clojure.test]
        [chlorine.util]))

(dosync (ref-set *macros* {}))

(deftest detect-form-test
  (is (= (detect-form '(foo 1 :x))
         "foo"))
  (is (= (detect-form 123)
         123))
  (is (= (detect-form "foo")
         "foo")))

(deftest sym->property-test
  (is (= (sym->property :a)
         "'a'"))
  (is (= (sym->property 'b)
         "'b'"))
  (is (= (sym->property '.c)
         "'c'")))

(deftest emit-delimted-test
  (is (= (with-out-str (emit-delimited "," [1 2 3]))
         "1,2,3"))
  (is (= (with-out-str
           (emit-delimited ";" [1 2 3]
                           (fn [number] (print (inc number)))))
         "2;3;4")))

(deftest emit-map-test
  (is (= (with-out-str (emit-map {:a 1 :b 2}))
         "{'a' : 1,'b' : 2}"))
  (is (= (with-out-str (emit-map {:a 1 "b" {'c 2}}))
         "{\"b\" : {c : 2},'a' : 1}")))

(deftest emit-set-test
  (is (= (with-out-str (emit-set #{:foo "bar" 'bazz 5}))
         "{5 : true,'foo' : true,\"bar\" : true,bazz : true}")))

(deftest literals
  (is (= (js *print-pretty*) "$STAR$print_pretty$STAR$"))
  (is (= (js number?) "number$QUEST$"))
  (is (= (js foo-bar-baz) "foo_bar_baz"))
  (is (= (js inc!) "inc$EXCL$"))
  (is (= (js {:foo 1 :bar 2 :baz 3}) "{'foo' : 1,'bar' : 2,'baz' : 3}"))
  (is (= (js #{:foo :bar :baz}) "{'foo' : true,'bar' : true,'baz' : true}"))
  (is (= (js [:foo :bar :baz]) "['foo','bar','baz']"))
  (is (= (js #"/^([a-z]*)([0-9]*)/") "/^([a-z]*)([0-9]*)/"))
  (is (= (js \newline) "'\n'"))
  (is (= (js \a) "'a'")))

(deftest functions
  (is (= (js (+* 1 2 3)) "(1 + 2 + 3)"))
  (is (= (js (+* "foo" "bar" "baz")) "(\"foo\" + \"bar\" + \"baz\")"))
  (is (= (js (:test {:test 1 :foo 2 :bar 3}))
         "get({'test' : 1,'foo' : 2,'bar' : 3}, 'test')"))
  (is (= (js (append '(:foo bar baz) '(quux)))
         "append(['foo','bar','baz'], ['quux'])"))

  (is (= (js (fn* [a b] (+* a b)))
         "function (a, b) { return (a + b); }"))

  (is (= (js (fn* foo [a b] (+* a b)))
         "function foo (a, b) { return (a + b); }"))

  (is (= (with-pretty-print (js (fn* "Some func does stuff" [x] (+* x 1))))
         (str "function (x) {\n"
              "    /* Some func does stuff */\n"
              "    return (x + 1);\n}")))

  (is (= (with-pretty-print (js (fn* "Some func\ndoes stuff" [x] (+* x 1))))
         (str "function (x) {\n"
              "    /* Some func\n"
              "       does stuff */\n"
              "    return (x + 1);\n}")))

  (is (= (js (fn* foo [a b] (+* a b)))
         "function foo (a, b) { return (a + b); }"))

  (is (= (js (fn* foo [c] (.methodOf c)))
         "function foo (c) { return c.methodOf(); }"))

  (is (= (js
          (fn* test []
            (let [a 1] (log (** a a)))
            (let [a 2] (log (** a a)))))
         (str  "function test () {"
               " (function () {"
               " var a = 1;"
               " return log((a * a));"
               "  })();"
               " return (function () {"
               " var a = 2;"
               " return log((a * a));"
               "  })(); }"
               )))
  (is (= (js
          (fn* test []
            (let [a 1] (log (** a a)))
            (do (log "test") (+* 1 1))))
         (str "function test () {"
              " (function () {"
              " var a = 1;"
              " return log((a * a));"
              "  })();"
              "  log(\"test\");"
              " return (1 + 1);; }"
              ))))

(deftest normalize-dot-form-test
  (is (= (normalize-dot-form '.)
         '.))
  (is (= (normalize-dot-form '.f)
         'f))
  (is (= (normalize-dot-form '.foo)
         'foo))
  (is (= (normalize-dot-form 'Foo.)
         'Foo))
  (is (= (normalize-dot-form 'F.)
         'F)))

(deftest member-form?-tests
  (is (= (member-form? '.foo)
         true))
  (is (= (member-form? '.-bar)
         true)))

(deftest property-access
  (is (= (js (get* map :key))
         "map['key']"))
  (is (= (js (:key map))
         "get(map, 'key')")))

(deftest dot-form-test
  (is (= (js (. foo -bar))
         "foo.bar"))
  (is (= (js (.-bar foo))
         "foo.bar"))
  (is (= (js (. foo bar))
         "foo.bar()"))
  (is (= (js (. foo bar :bazz 0))
         "foo.bar('bazz', 0)"))
  (is (= (js (.bar foo))
         "foo.bar()"))
  (is (= (js (.bar (new foo)))
         "new foo().bar()"))
  (is (= (js (. (. a (b 1)) (c 2)))
         "a.b(1).c(2)")))

(deftest destructuring
  (is (= (js
          (fn* test []
               (let [a 1
                     b (+* a 1)
                     c (+* b 1)]
                 (+* a b c))))
         (str "function test () {"
              " var a = 1, b = (a + 1), c = (b + 1);"
              " return (a + b + c);; }")))

  ;; & rest
  (is (= (js
          (fn* test []
               (let [[a b & r] [1 2 3 4]]
                 [(+* a b) r])))
         (str "function test () {"
              " var _temp_1000 = [1,2,3,4],"
              " a = _temp_1000[0],"
              " b = _temp_1000[1],"
              " r = _temp_1000.slice(2);"
              " return [(a + b),r];; }")))

  (is (= (js
          (fn* test [[a b & r]]
               [(+* a b) r]))
         (str "function test () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " _temp_1001 = _temp_1000[0],"
              " a = _temp_1001[0],"
              " b = _temp_1001[1],"
              " r = _temp_1001.slice(2);"
              " return [(a + b),r]; }")))

  (is (= (js
          (fn* test [a b & r]
               [(+* a b) r]))
         (str "function test () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " a = _temp_1000[0],"
              " b = _temp_1000[1],"
              " r = _temp_1000.slice(2);"
              " return [(a + b),r]; }")))

  ;; :as
  (is (= (js
          (fn* [a [b] [c d & e :as f] :as g] nil))
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
              " g = _temp_1000; }")
         ))

  ;; map destructuring
  (is (= (js
          (fn* [x {y :y, fred :fred}] fred))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " x = _temp_1000[0],"
              " _temp_1001 = _temp_1000[1],"
              " y = get(_temp_1001, 'y'),"
              " fred = get(_temp_1001, 'fred');"
              " return fred; }")))

  (is (= (js
          (fn* [[{x :x, {z :z} :y}]] z))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " _temp_1001 = _temp_1000[0],"
              " _temp_1002 = _temp_1001[0],"
              " x = get(_temp_1002, 'x'),"
              " _temp_1003 = get(_temp_1002, 'y'),"
              " z = get(_temp_1003, 'z');"
              " return z; }")))

  ;; numbers as keys (this actually works)
  (is (= (js
          (fn* [{a 1, b 2, :or {a 3}}]))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " _temp_1001 = _temp_1000[0],"
              " a = get(_temp_1001, 1, 3),"
              " b = get(_temp_1001, 2); }")
         ))

  ;; :keys, :strs
  (is (= (js
          (fn* [x {y :y, z :z :keys [a b]}] z))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " x = _temp_1000[0],"
              " _temp_1001 = _temp_1000[1],"
              " a = get(_temp_1001, 'a'),"
              " b = get(_temp_1001, 'b'),"
              " y = get(_temp_1001, 'y'),"
              " z = get(_temp_1001, 'z');"
              " return z; }")))

  (is (= (js
          (fn* [x {y :y, z :z :strs [a b]}] z))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " x = _temp_1000[0],"
              " _temp_1001 = _temp_1000[1],"
              " a = get(_temp_1001, 'a'),"
              " b = get(_temp_1001, 'b'),"
              " y = get(_temp_1001, 'y'),"
              " z = get(_temp_1001, 'z');"
              " return z; }")))

  (is (= (js
          (fn* [x {y :y, z :z :or {y 1, z "foo"}}] z))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " x = _temp_1000[0],"
              " _temp_1001 = _temp_1000[1],"
              " y = get(_temp_1001, 'y', 1),"
              " z = get(_temp_1001, 'z', \"foo\");"
              " return z; }")))

  (is (= (js
          (fn* [x {y :y, z :z :keys [a b] :or {a 1, y :bleh}}] z))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " x = _temp_1000[0],"
              " _temp_1001 = _temp_1000[1],"
              " a = get(_temp_1001, 'a', 1),"
              " b = get(_temp_1001, 'b'),"
              " y = get(_temp_1001, 'y', 'bleh'),"
              " z = get(_temp_1001, 'z');"
              " return z; }")))

  (is (= (js
          (fn* [{x :x y :y :as all}]
               [x y all]))
         (str "function () {"
              " var _temp_1000 = Array.prototype.slice.call(arguments),"
              " all = _temp_1000[0],"
              " x = get(all, 'x'),"
              " y = get(all, 'y');"
              " return [x,y,all]; }")))
  ;; unsupported for now
  (is (thrown-with-msg? Exception #"& must be followed by"
        (js
         (fn* [x y & {z :z}] z)))))

(deftest loops
  (is (= (js
          (fn* join [arr delim]
            (loop [str (get* arr 0)
                   i 1]
              (if (< i (count arr))
                (recur (+* str delim (get* arr i))
                       (+* i 1))
                str))))
         (str "function join (arr, delim) {"
              " for (var str = arr[0], i = 1; true;) {"
              " if ((i < count(arr))) {"
              " var _temp_1000 = [(str + delim + arr[i]),(i + 1)];\n"
              " str = _temp_1000[0];"
              " i = _temp_1000[1];"
              " continue; }"
              " else {"
              " return str; };"
              " break; }; }"))))

(deftest inline-if
  (is (= (js
          (fn* test [a]
            ((if (> a 0) minus plus) a 1)))
         "function test (a) { return (((a > 0) ? minus : plus))(a,1); }"))

  ;; implicit `null` alternate
  (is (= (js (fn* test [a] (console.log (if (> a 0) a))))
         (str "function test (a) {"
              " return console.log("
              "((a > 0) ? a : undefined)); }")
         )))

(deftest if-tests
  (is (= (js (if a b c))
         "if (a) { b; } else { c; }"))
  (is (= (js (if :a b c))
         "if ('a') { b; } else { c; }"))
  (is (= (js (def x (if x :a :b)))
         "var x = (x ? 'a' : 'b')"))
  (is (= (js (def x (if true :a :b)))
         "var x = (true ? 'a' : 'b')"))
  (is (= (js (fn* [] (if a b c)))
         "function () { if (a) { return b; } else { return c; }; }"))
  (is (= (js (fn* [] (if :true b c)))
         "function () { if ('true') { return b; } else { return c; }; }"))
  )

(deftest inline-primitives
  (is (= (js (fn* isac? [i c] (inline "i instanceof c")))
         "function isac$QUEST$ (i, c) { return i instanceof c; }")))

(deftest case-tests
  (is (= (with-pretty-print (js (case answer 42 (bingo) 24 (tiny))))
         (str "switch (answer) {\n"
              "    case 42:\n"
              "        bingo();\n"
              "        break;\n"
              "    case 24:\n"
              "        tiny();\n"
              "        break;\n"
              "}")))
  (is (= (js (case answer 42 (bingo) 24 (tiny)))
         (str "switch (answer) {"
              " case 42: bingo(); break; case 24: tiny(); break; }")))
  (is (= (js (case answer (+* 10 20) (bingo)))
         "switch (answer) { case (10 + 20): bingo(); break; }"))
  (is (= (js (case answer "text" (foo) (+* 10 20) (bingo)))
         (str "switch (answer) {"
              " case \"text\": foo(); break;"
              " case (10 + 20): bingo(); break; }")))
  (is (= (js (case answer 1 :one 2 :two :anything-else))
         (str "switch (answer) {"
              " case 1: 'one'; break;"
              " case 2: 'two'; break;"
              " default: 'anything-else'; }"))))

(deftest do-test
  (is (= (binding [*inline-if* true]
           (js (do 1 2 3)))
         "(function(){ 1; 2; return 3;})()"))
  (is (= (binding [*inline-if* false]
           (js (do 1 2 3)))
         " 1; 2; 3;")))

(deftest try-catch-finally
  (is (= (js
          (fn* test []
            (try
              (/ 5 0)
              (catch ex
                  (console.log ex))
              (finally
               0))))
         (str "function test () {"
              " try { return (5 / 0); } catch (ex) {"
              " return console.log(ex); }"
              " finally {"
              " return 0; }; }")))

  (is (= (js
          (fn* test [a]
            (if (< a 0) (throw (new Error "Negative numbers not accepted")))))
         (str "function test (a) {"
              " if ((a < 0)) {"
              " throw new Error(\"Negative numbers not accepted\"); }; }"))))

(deftest let-tests
  (is (= (js (def x (let [y 3] y)))
         "var x = (function () { var y = 3; return y;  })()"))
  (is (= (js (fn* [] (let [x 1 y 2] (+ x y))))
         "function () { var x = 1, y = 2; return $PLUS$(x, y);; }"))
  (is (= (js (fn* [] (let [x 1 y 2] (+ x y)) 3))
         (str "function () {"
              " (function () {"
              " var x = 1, y = 2; return $PLUS$(x, y);  })();"
              " return 3; }")))
  (is (= (js (let [m {:test 1 :foo 2 :bar 3}] (:baz m 4)))
         (str "(function () { var m = {'test' : 1,'foo' : 2,'bar' : 3};"
              " return get(m, 'baz', 4);  })()"))))

(deftest js-let-test
  (is (= (js-let [a 2 b 3] (+* a b))
         " (function (a, b) { return (a + b); })(2,3);")))

(deftest let-js-test
  (is (= (let-js [foo 1]
                 `(def x ~foo))
         "var x = 1")))

(deftest new-and-delete-tests
  (is (= (js (Foo. :bar))
         "new Foo('bar')"))
  (is (= (js (new bar boo buzz))
         "new bar(boo,buzz)"))
  (is (= (js (delete foo))
         "delete foo")))

(borrow-macros '..)
(deftest expand-macro-test
  (is (= (expand-macro-1 '(.. foo bar))
         '(. foo bar)))
  (is (= (expand-macro '(.. foo (bar) (buzz)))
         '(. (. foo (bar)) (buzz)))))

(deftest macroexpand-1-test
  (is (= (js (macroexpand-1 (. foo (bar))))
         "\"(. foo (bar))\""))
  (is (= (js (macroexpand-1 (.. foo (bar) (buzz))))
         "\"(.. (. foo (bar)) (buzz))\""))
  (is (= (js (macroexpand (.. foo (bar) (buzz))))
         "\"(. (. foo (bar)) (buzz))\"")))
