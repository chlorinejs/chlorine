(ns clojurejs.js
  (:require [clojure.string :as str])
  (:use [clojure.java.io :only [reader]]
        [clojurejs.util :only [unzip assert-args]]))

(defn- sexp-reader [source]
  "Wrap `source' in a reader suitable to pass to `read'."
  (new java.io.PushbackReader (reader source)))

(defn- re? [expr] (= (class expr) java.util.regex.Pattern))

(def ^:dynamic *inline-if* false)
(def ^:dynamic *quoted* false)
(def ^:dynamic *print-pretty* false)

(defmacro with-pretty-print [& body]
  `(binding [*print-pretty* true]
     ~@body))

(def ^:dynamic *indent* 0)

(defmacro with-indent [[& increment] & body]
  `(binding [*indent* (+ *indent* (or ~increment 4))]
     ~@body))

(def ^:dynamic *in-block-exp* false)

(defmacro with-block [& body]
  `(binding [*in-block-exp* true]
     ~@body))

(defn- newline-indent []
  (if *print-pretty*
    (do
      (newline)
      (print (apply str (repeat *indent* " "))))
    (print " ")))

(defmacro with-parens [[& [left right]] & body]
  `(do
     (print (or ~left "("))
     ~@body
     (print (or ~right ")"))))

(defn- jskey [x]
  (let [x (if (and (coll? x) (seq x)) (first x) x)]
    (if (symbol? x) (name x) x)))

(defn- dotsymbol? [s]
  (and (symbol? s) (.startsWith (name s) ".")))

(declare emit-str)

(defn- sym->property [s]
  "Transforms symbol or keyword into property access form."
  (binding [*quoted* true]
    (emit-str
      (if (dotsymbol? s)
        (symbol (subs (name s) 1))
        s))))

(defmulti emit "Emit a javascript expression." {:private true} jskey)

(defn- emit-delimited [delimiter args & [emitter]]
  (when-not (empty? args)
    ((or emitter emit) (first args))
    (doseq [arg (rest args)]
      (print delimiter)
      ((or emitter emit) arg))))

(defn- emit-map [expr]
  (with-parens ["{" "}"]
    (binding [*inline-if* true]
      (emit-delimited ","
                      (seq expr)
                      (fn [[key val]]
                        (emit key)
                        (print " : ")
                        (emit val))))))

(defn- emit-set [expr]
  (with-parens ["{" "}"]
    (binding [*inline-if* true]
      (emit-delimited ","
                      (seq expr)
                      (fn [key]
                        (emit key)
                        (print " : true"))))))

(defn- emit-vector [expr]
  (with-parens ["[" "]"]
    (binding [*inline-if* true]
      (emit-delimited "," (seq expr)))))

(defn- emit-re [expr]
  (print (str "/" (apply str (replace {\/ "\\/"} (str expr))) "/")))

(defn- emit-symbol [expr]
  (if *quoted* (print "'"))
  (print
   (if *quoted*
     (name expr)
     (apply str (replace {\- "_" \* "__"
                          \? "___p" \! "___f"
                          \= "_eq"} (name expr)))))
  (if *quoted* (print "'")))

(defn- emit-keyword [expr]
  (binding [*quoted* true]
    (emit-symbol expr)))

(defn- unary-operator? [op]
  (and (symbol? op) (contains? #{"++" "--" "!"} (name op))))

(defn- emit-unary-operator [op arg]
  (print (name op))
  (emit arg))

(defn- infix-operator? [op]
  (and (symbol? op)
       (contains? #{"and" "or"
                    "bit-and" "bit-or" "bit-xor" "bit-not"
                    "bit-shift-left" "bit-shift-right"
                    "bit-shift-right-zero-fill"
                    "rem"
                    "+" "-" "/" "*"
                    ">" ">=" "<" "<="
                    "="
                    ;;"=="
                    "==="
                    "not="
                    ;;"!="
                    "!=="
                    "instanceof"}
                  (name op))))

(defn- emit-infix-operator [op & args]
  (let [clj->js {"and"             "&&"
                 "or"              "||"
                 "="               "==="
                 "not="            "!=="
                 "rem"             "%"
                 "bit-and"         "&"
                 "bit-or"          "|"
                 "bit-xor"         "^"
                 "bit-not"         "~"
                 "bit-shift-left"  "<<"
                 "bit-shift-right" ">>"
                 "bit-shift-right-zero-fill" ">>>"}
        js-op (get clj->js (name op) (name op))]
    (with-parens []
      (emit-delimited (str " " js-op " ") args))))

(defn- emit-function-call [fun & args]
  (emit fun)
  (with-parens []
    (with-indent [] (emit-delimited ", " args))))

(defn- emit-method-call [recvr selector & args]
  (emit recvr)
  (emit selector)
  (with-parens []
    (with-indent [] (emit-delimited ", " args))))

(def ^:dynamic *return-expr* false)

(defmacro with-return-expr [[& [new-val]] & body]
  `(binding [*return-expr* (if *return-expr*
                             (do
                               (print "return ")
                               false)
                             (or ~new-val false))]
     ~@body))

(def ^:dynamic *in-fn-toplevel* true)

(defn- emit-function-form [form]
  (binding [*inline-if* true
            *in-fn-toplevel* false]
    (let [[fun & args] form
          method? (fn [f] (and (symbol? f) (= \. (first (name f)))))
          invoke-method (fn [[sel recvr & args]]
                          (apply emit-method-call recvr sel args))
          new-object? (fn [f] (and (symbol? f) (= \. (last (name f)))))
          invoke-fun (fn [fun & args]
                       (with-parens [] (emit fun))
                       (with-parens [] (emit-delimited "," args)))]
      (cond
       (unary-operator? fun) (apply emit-unary-operator form)
       (infix-operator? fun) (apply emit-infix-operator form)
       (keyword? fun) (let [[map & default] args] (emit `(get ~map ~fun ~@default)))
       (method? fun) (invoke-method form)
       (new-object? fun) (emit `(new ~(symbol (apply str (drop-last (str fun)))) ~@args))
       (coll? fun) (apply invoke-fun form)
       true (apply emit-function-call form)))))

(defn emit-statement [expr]
  (binding [*inline-if* false]
    (if (and (coll? expr) (= 'defmacro (first expr))) ; cracks are showing
      (emit expr)
      (do
        (newline-indent)
        (emit expr)
        (print ";")))))

(defn emit-statements [exprs]
  (doseq [expr exprs]
    (emit-statement expr)))

(defn emit-statements-with-return [exprs]
  (binding [*return-expr* false]
    (doseq [expr (butlast exprs)]
      (emit-statement expr)))
  (emit-statement (last exprs)))

(defmethod emit "def" [[_ name value]]
  (print "var ")
  (emit-symbol name)
  (print " = ")
  (binding [*inline-if* true]
    (emit value)))

(def ^:dynamic *macros* (ref {}))

(defn- macro? [n] (and (symbol? n) (contains? @*macros* (name n))))

(defn- get-macro [n] (and (symbol? n) (get @*macros* (name n))))

(defn- undef-macro [n]
  (when (macro? n)
    (when *print-pretty* (println "// undefining macro" n))
    (dosync (alter *macros* dissoc (name n)))))

(defmethod emit "defmacro" [[_ mname args & body]]
  (dosync
   (alter *macros*
          conj
          {(name mname) (eval `(clojure.core/fn ~args ~@body))}))
  nil)

(defn- emit-macro-expansion [form]
  (let [[mac-name & args] form
        mac (get-macro mac-name)
        macex (apply mac args)]
    (emit macex)))

(defn- emit-docstring [docstring]
  (when *print-pretty*
    (let [lines (str/split-lines docstring)]
      (newline-indent)
      (print (str "/* " (first lines)))
      (doseq [line (rest lines)]
        (newline-indent)
        (print (str "   " line)))
      (print " */"))))

(defn- ignorable-arg? [n]
  (and (symbol? n) (.startsWith (name n) "_")))

(def  ^:dynamic *temp-sym-count* nil)

(defn tempsym []
  (dosync
   (ref-set *temp-sym-count* (+ 1 @*temp-sym-count*))
   (symbol (str "_temp_" @*temp-sym-count*))))

(defn- emit-simple-binding [vname val]
  (emit (if (ignorable-arg? vname) (tempsym) vname))
  (print " = ")
  (binding [*inline-if* true]
    (emit val)))

(declare emit-var-bindings
         emit-destructured-seq-binding
         emit-destructured-map-binding)

(defn- destructuring-form? [form]
  (or (map? form) (vector? form)))

(defn- binding-form? [form]
  (or (symbol? form) (destructuring-form? form)))

(defn- binding-special? [form]
  (contains? #{'& :as} form))

(defn- emit-binding [vname val]
  (binding [*inline-if* true]
    (let [emitter (cond
                   (vector? vname) emit-destructured-seq-binding
                   (map? vname)    emit-destructured-map-binding
                   :else           emit-simple-binding)]
      (emitter vname val))))

(defn- emit-destructured-seq-binding [vvec val]
  (let [temp (tempsym)]
    (print (str temp " = "))
    (emit val)
    (loop [vseq vvec, i 0, seen-rest? false]
      (when (seq vseq)
        (let [vname (first vseq)
              vval  (second vseq)]
          (print ", ")
          (condp = vname
            '&  (cond
                  seen-rest?
                    (throw (Exception. "Unsupported binding form, only :as can follow &"))
                  (not (symbol? vval))
                    (throw (Exception. "Unsupported binding form, & must be followed by exactly one symbol"))
                  :else
                    (do (emit-binding vval `(.slice ~temp ~i))
                        (recur (nnext vseq) (inc i) true)))
            :as (cond
                  (not= (count (nnext vseq)) 0)
                    (throw (Exception. "Unsupported binding form, nothing must follow after :as <binding>"))
                  (not (symbol? vval))
                    (throw (Exception. "Unsupported binding form, :as must be followed by a symbol"))
                  :else
                    (emit-binding vval temp))
            (do (emit-binding vname `(get ~temp ~i))
                (recur (next vseq) (inc i) seen-rest?))))))))

(defn- emit-destructured-map-binding [vmap val]
  (let [temp     (tempsym)
        defaults (get vmap :or)
        keysmap  (reduce #(assoc %1 %2 (keyword %2))
                  {}
                  (mapcat vmap [:keys :strs :syms]))
        vmap     (merge (dissoc vmap :or :keys :strs :syms) keysmap)]
    (print (str temp " = "))
    (emit val)
    (doseq [[vname vkey] vmap]
      (print ", ")
      (cond
        (not (and (binding-form? vname)
                  (or (some #(% vkey) #{keyword? number? binding-form?}))))
          (throw (Exception. "Unsupported binding form, binding symbols must be followed by keywords or numbers"))

        :else
          (if-let [[_ default] (find defaults vname)]
            (emit-binding vname `(get ~temp ~vkey ~default))
            (emit-binding vname `(get ~temp ~vkey)))))))

(defn- emit-var-bindings [bindings]
  (binding [*return-expr* false]
    (emit-delimited
      ", "
      (partition 2 bindings)
      (fn [[vname val]]
        (emit-binding vname val)))))

(declare emit-function-1)

(defn- emit-function [fdecl]
  (if (symbol? (first fdecl))
    (emit-function-1 (first fdecl) (rest fdecl))
    (emit-function-1 nil fdecl)))

(defn- emit-function-1 [fname fdecl]
  (let [docstring (if (string? (first fdecl))
                    (first fdecl)
                    nil)
        fdecl     (if (string? (first fdecl))
                    (rest fdecl)
                    fdecl)
        args      (first fdecl)
        dargs?    (or (some destructuring-form? args)
                      (some binding-special? args)
                      (some ignorable-arg? args))
        body      (rest fdecl)]
    (assert-args fn
      (vector? args) "a vector for its bindings")
    (if dargs?
      (do
        (print "function () {")
        (with-indent []
          (newline-indent)
          (print "var ")
          (emit-binding args '(Array.prototype.slice.call arguments))
          (print ";")))
      (do
        (print "function ")
        (if fname (print fname ""))
        (print "(")
        (binding [*return-expr* false] (emit-delimited ", " args))
        (print ") {")))
    (with-indent []
      (when docstring
        (emit-docstring docstring))
      (binding [*return-expr* true]
        (emit-statements-with-return body)))
    (newline-indent)
    (print "}")))

(defmethod emit "fn" [[_ & fdecl]]
  (with-return-expr []
    (with-block (emit-function fdecl))))

(defmethod emit "defn" [[_ name & fdecl]]
  (assert-args defn (symbol? name) "a symbol as its name")
  (undef-macro name)
  (emit-symbol name)
  (print " = ")
  (with-block
    (emit-function fdecl)))

(defmethod emit "if" [[_ test consequent & [alternate]]]
  (let [emit-inline-if (fn []
                         (with-return-expr []
                           (with-parens []
                             (emit test)
                             (print " ? ")
                             (emit consequent)
                             (print " : ")
                             (emit alternate))))
        emit-block-if (fn []
                        (print "if (")
                        (binding [*return-expr* false
                                  *inline-if* true]
                          (emit test))
                        (print ") {")
                        (with-block
                          (with-indent []
                            (emit-statement consequent)))
                        (newline-indent)
                        (print "}")
                        ;; alternate might be `0`, which js equates as `nil`
                        (when-not (nil? alternate)
                          (print " else {")
                          (with-block
                            (with-indent []
                              (emit-statement alternate)))
                          (newline-indent)
                          (print "}")))]
    (if (and *inline-if* consequent)
      (emit-inline-if)
      (emit-block-if))))

(defmethod emit "case" [[_ e & clauses]]
  (let [pairs (partition 2 clauses)]
    (print "switch (")
    (binding [*return-expr* false]
      (print (str e)))
    (print ") {")
    (doall
     (for [[k v] pairs]
       (do (print " case " )
           (emit k)
           (print ":")
           (with-block
             (with-indent []
               (emit-statement v))))))

    (when (odd? (count clauses))
      (do (print " default:")
          (with-block
            (with-indent []
              (emit-statement (last clauses))))))
    (newline-indent)
    (print "}")))

(defmethod emit "do" [[_ & exprs]]
  (if *inline-if*
    (do
      (print "(function(){")
      (binding [*return-expr* true]
        (emit-statements-with-return exprs))
      (print "})()"))
    (emit-statements-with-return exprs)))

(defmethod emit "let" [[_ bindings & exprs]]
  (let [emit-var-decls (fn []
                         (print "var ")
                         (binding [*return-expr* false]
                           (with-block (emit-var-bindings bindings))
                           (print ";"))
                         (emit-statements-with-return exprs))]
    (if (or (not *in-fn-toplevel*) *inline-if*)
      (with-return-expr []
        (print "(function () {")
        (with-indent []
          (newline-indent)
          (binding [*return-expr* true]
            (emit-var-decls)))
        (newline-indent)
        (print " }).call(this)"))
      (binding [*in-fn-toplevel* false]
        (emit-var-decls)))))

(defmethod emit "new" [[_ class & args]]
  (with-return-expr []
    (binding [*inline-if* true]
      (print "new ")
      (emit class)
      (with-parens [] (emit-delimited "," args)))))

(defmethod emit "return" [[_ value]]
  (print "return ")
  (emit value))

(defmethod emit 'nil [_]
  (with-return-expr []
    (print "null")))

(defmethod emit "get" [args]
  (let [[_ map key default]  args
        default? (> (count args) 3)
        emit-get
          (fn []
            (emit map)
            (if (dotsymbol? key)
              (emit key)
              (do
                (print "[")
                (emit key)
                (print "]"))))]
    (with-return-expr []
      (if default?
        ;; FIXME Should be able to re-use code for
        ;; inline if and contains? macro here.
        ;; FIXME Also, `map` will be evaluated twice (once in
        ;; the `in` test, and once in output of `emit-get`
        (with-parens []
          (print (sym->property key))
          (print " in ")
          (emit map)
          (print " ? ")
          (emit-get)
          (print " : ")
          (emit default))
        (emit-get)))))

(defmethod emit "set!" [[_ & apairs]]
  (binding [*return-expr* false
            *in-fn-toplevel* false
            *inline-if* true]
    (let [apairs (partition 2 apairs)]
      (emit-delimited " = " (first apairs))
      (doseq [apair (rest apairs)]
        (print ";")
        (newline-indent)
        (emit-delimited " = " apair)))))

(defmethod emit "try" [[_ expr & clauses]]
  (print "try {")
  (with-indent []
    (with-block
      (emit-statement expr)))
  (newline-indent)
  (print "}")
  (doseq [[clause & body] clauses]
    (case clause
      catch (let [[evar expr] body]
              (with-block
                (print " catch (")
                (emit-symbol evar)
                (print ") {")
                (with-indent [] (emit-statement expr))
                (newline-indent)
                (print "}")))
      finally (with-block
                (print " finally {")
                (with-indent [] (doseq [expr body] (emit-statement expr)))
                (newline-indent)
                (print "}")))))

(def ^:dynamic *loop-vars* nil)

(defmethod emit "loop" [[_ bindings & body]]
  (let [emit-for-block (fn []
                         (print "for (var ")
                         (binding [*return-expr* false
                                   *in-block-exp* false]
                           (emit-var-bindings bindings))
                         (print "; true;) {")
                         (with-indent []
                           (binding [*loop-vars* (first (unzip bindings))]
                             (emit-statements-with-return body))
                           (newline-indent)
                           (print "break;"))
                         (newline-indent)
                         (print "}"))]
    (if (or (not *in-fn-toplevel*) *inline-if*)
      (with-return-expr []
        (print "(function () {")
        (binding [*return-expr* true]
          (with-indent []
            (newline-indent)
            (emit-for-block))
          (newline-indent))
        (print "}).call(this)"))
      (binding [*in-fn-toplevel* false]
        (emit-for-block)))))

(defmethod emit "recur" [[_ & args]]
  (binding [*return-expr* false]
    (let [tmp (tempsym)]
      (print "var" (emit-str tmp) "= ")
      (emit-vector args)
      (println ";")
      (emit-statements (map (fn [lvar i] `(set! ~lvar (get ~tmp ~i)))
                            *loop-vars*
                            (range (count *loop-vars*))))))
  (newline-indent)
  (print "continue"))

(defmethod emit "dokeys" [[_ [lvar hash] & body]]
  (binding [*return-expr* false]
    (print "for (var ")
    (emit lvar)
    (print " in ")
    (emit hash)
    (print ") {")
    (with-indent []
      (emit-statements body))
    (newline-indent)
    (print "}")))

(defmethod emit "while" [[_ test & body]]
  (binding [*return-expr* false]
    (print "while (")
    (emit test)
    (print ") {")
    (with-indent []
      (emit-statements body))
    (newline-indent)
    (print "}")))

(defmethod emit "do-while" [[_ test & body]]
  (binding [*return-expr* false]
    (print "do {")
    (with-indent []
      (emit-statements body))
    (newline-indent)
    (print "}")
    (print " while (")
    (emit test)
    (print ")")))

(defmethod emit "jfor" [[_ [init-bindings test update] & body]]
  (let [init (if (vector? init-bindings)
               (concat ['lvar] init-bindings)
               init-bindings)]
    (binding [*return-expr* false]
      (print "for (")
      (emit init)
      (print ";")
      (emit test)
      (print ";")
      (emit update)
      (print ") {")
      (with-indent []
        (emit-statements body))
      (newline-indent)
      (print "}"))))

(defmethod emit "inline" [[_ js]]
  (with-return-expr []
    (print js)))

(defmethod emit "quote" [[_ expr]]
  (binding [*quoted* true]
    (emit expr)))

(defmethod emit "throw" [[_ expr]]
  (binding [*return-expr* false]
    (print "throw ")
    (emit expr)))

(defmethod emit :default [expr]
  (if (and (coll? expr) (not *quoted*) (macro? (first expr)))
    (emit-macro-expansion expr)
    (with-return-expr []
      (cond
       (map? expr) (emit-map expr)
       (set? expr) (emit-set expr)
       (vector? expr) (emit-vector expr)
       (re? expr) (emit-re expr)
       (keyword? expr) (emit-keyword expr)
       (string? expr) (pr expr)
       (symbol? expr) (emit-symbol expr)
       (char? expr) (print (format "'%c'" expr))
       (and *quoted* (coll? expr)) (emit-vector expr)
       (coll? expr) (emit-function-form expr)
       true (print expr)))))

(defn emit-str [expr]
  (binding [*return-expr* false
            *inline-if* true]
    (with-out-str (emit expr))))

(defn js-emit [expr] (emit expr))

(defmacro js [& exprs]
  "Translate the Clojure subset `exprs' to a string of javascript
code."
  (let [exprs# `(quote ~exprs)]
    `(binding [*temp-sym-count* (ref 999)]
       (with-out-str
         (if (< 1 (count ~exprs#))
           (emit-statements ~exprs#)
           (js-emit (first ~exprs#)))))))

(defmacro js-let [bindings & exprs]
  "Bind Clojure environment values to named vars of a cljs block, and
translate the Clojure subset `exprs' to a string of javascript code."
  (let [form# 'fn
        [formals# actuals#] (unzip bindings)]
    `(with-out-str
       (emit-statement (list '(~form# ~(vec formals#) ~@exprs) ~@actuals#)))))

(defmacro let-js [bindings quoted-expr]
  "Bind Clojure environment values to named vars of a quoted cljs block, and
translate the Clojure subset `exprs' to a string of javascript code."
  (let [body# `(let ~bindings ~quoted-expr)]
    `(with-out-str
       (js-emit ~body#))))

(defmacro script [& forms]
  "Similar to the (js ...) form, but wraps the javascript in a
 [:script ] element, which can be passed to hiccup.core/html."
  `[:script {:type "text/javascript"}
    (js ~@forms)])

(defmacro script-let [bindings & forms]
  "Similar to the (js-let ...) form, but wraps the javascript in a
 [:script ] element, which can be passed to hiccup.core/html."
  `[:script {:type "text/javascript"}
    (js-let ~bindings ~@forms)])

(defmacro jq [& forms]
  "Similar to the (js ...) form, but wraps the javascript in a
 [:script ] element which is invoked on a jQuery document.ready
 event."
  (let [fnform# 'fn]
    `[:script {:type "text/javascript"}
      (js (.ready ($ document) (~fnform# [] ~@forms)))]))

(defmacro jq-let [bindings & forms]
  "Similar to the (js-let ...) form, but wraps the javascript in a
 [:script ] element which is invoked on a jQuery document.ready
 event."
  (let [fnform# 'fn
        [formals# actuals#] (unzip bindings)]
    `[:script {:type "text/javascript"}
      "$(document).ready(function () {"
      (js-let ~bindings ~@forms)
      "});"]))

(def ^:dynamic *last-sexpr* nil)

(defn tojs [& scripts]
  "Load and translate the list of cljs scripts into javascript, and
return as a string. Useful for translating an entire cljs script file."
  (binding [*temp-sym-count* (ref 999)
            *last-sexpr* (ref nil)]
    (with-out-str
      (doseq [f scripts]
        (try
          (with-open [in (sexp-reader f)]
            (loop [expr (read in false :eof)]
              (when (not= expr :eof)
                (if-let [s (emit-statement expr)]
                  (print s)
                  (dosync
                   (ref-set *last-sexpr* expr)))
                (recur (read in false :eof)))))
          (catch Throwable e
            (throw (new Throwable
                        (str
                         "Error translating script " f
                         " last s-expr " @*last-sexpr*)
                        e))))))))
