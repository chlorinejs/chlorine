(ns chlorine.js
  (:require [clojure.string :as str]
            [hiccup.core]
            [clojure.walk])
  (:use [chlorine.reader]
        [slingshot.slingshot]
        [pathetic.core :only [normalize url-normalize]]
        [chlorine.util
         :only [url? resource-path? to-resource unzip assert-args
                *cwd* *paths* get-dir find-in-paths
                re? replace-map]]))

(def ^:dynamic *print-pretty* false)

(def ^:dynamic *object-member* false)

(defmacro with-pretty-print [& body]
  `(binding [*print-pretty* true]
     ~@body))

(def ^:dynamic *indent* 0)

(defmacro with-indent [[& increment] & body]
  `(binding [*indent* (+ *indent* (or ~increment 4))]
     ~@body))

(def ^:dynamic *in-block-exp?* false)

(defmacro with-block [& body]
  `(binding [*in-block-exp?* true]
     ~@body))

(defn newline-indent []
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

(def ^:dynamic *inline-if* false)
(def ^:dynamic *quoted* false)

(def ^:dynamic *in-fn-toplevel* true)
(def ^:dynamic *unique-return-expr* false)

;; Chlorinejs transforms Clojure code/data to Javascript equivalents.
;;
;; Normal data such as strings, numbers, keywords, symbols, vectors, (quoted)
;; lists are transformed by associated emitters of their type.
;;
;; Functions, macros and special forms (including javascript native ones)
;; share the same looks: they are unquoted lists whose first element is the
;; form name and require one more step: looking up by the names to detect
;; their types.

(defn detect-form
  "Detects macro/function/special form names from expressions
for further processing. Used as dispatch function for chlorine.js/emit, the
most hardworking multi-method in chlorine library."
  [expr]
  (let [expr (if (and (coll? expr) (seq expr)) (first expr) expr)]
    (if (symbol? expr) (name expr) expr)))

(defn member-form?
  "Checks if a form is a property access or method call
(a symbol starting with '.')"
  [form-name]
  (and (symbol? form-name) (= \. (first (name form-name)))))

(defn new-object?
  "Checks if a symbol is a new object call (a symbol ending with '.')"
  [f]
  (and (symbol? f) (= \. (last (name f)))))

(defn normalize-dot-form
  "Normalizes dot forms or new-object forms by removing \".\" from their
 beginnings or endings."
  [form]
  (cond (and (.startsWith (name form) ".")
             (< 1 (count (name form))))
        (symbol (subs (name form) 1))

        (and (.endsWith (name form) ".")
           (< 1 (count (name form))))
        (symbol (apply str (drop-last (str form))))
        :default
        form))

(declare emit-str)
(declare tojs')

(defn sym->property
  "Transforms symbol or keyword into object's property access form."
  [s]
  (binding [*quoted* true]
    (emit-str
      (if (member-form? s)
        (symbol (subs (name s) 1))
        s))))

(defmulti emit
  "Receives forms, emits javascript expressions."
  detect-form)

(defn emit-delimited
  "Emit sequences with delimiters. Useful to emit javascript arrays,
function arguments etc."
  [delimiter args & [emitter]]
  (when-not (empty? args)
    ((or emitter emit) (first args))
    (doseq [arg (rest args)]
      (print delimiter)
      ((or emitter emit) arg))))

;; several functions to emit Clojure data of
;;  map, set, vector, regexp, symbol and keyword types
(defn valid-map-key?
  "Checks a map's key before emitting. Valid keys are elements which can be
  converted to strings."
  [key]
  (or (string?  key)
      (keyword? key)
      (number?  key)
      (symbol?  key)
      (and (seq? key)
           (= 2 (count key))
           (= 'quote  (first key))
           (symbol? (second key)))))

(defn emit-map
  "Clojure maps are emitted to javascript key/value objects.
Keys can only be strings. Keywords and quoted symbols don't really make
 sense in Chlorinejs and that's why they are emitted to plain strings."
  [expr]
  (with-parens ["{" "}"]
    (binding [*inline-if* true]
      (emit-delimited
       ","
       (seq expr)
       (fn [[key val]]
         (if (valid-map-key? key)
           (emit key)
           (throw+ {:known-error true
                    :msg
                    (str "Error emitting this map `"
                         expr "`:\n"
                         "Invalid map key: `" key "`.\n"
                         "Valid keys are elements which can be"
                         " converted to strings.")
                    :causes [expr key]}))
         (print " : ")
         (emit val))))))

(defn emit-set
  "Clojure sets are emitted to javascript key/value objects whose
values are all `true`. These 'sets' objects as javascript nature will have
distinct elements (the keys) which can be checked by `contains?` (javascript's
`in`). Please remember, all set elements are coerced to strings by javascript.
That means, both `(contains? 5 {:a 1 \"5\" 2})` and
 `(contains? \"5\" {:b 3 5 4} will return true."
  [expr]
  (emit `(hash-set ~@(seq expr))))

(defn emit-vector
  "Clojure vectors and quoted lists are emitted as javascript arrays."
  [expr]
  (with-parens ["[" "]"]
    (binding [*inline-if* true]
      (emit-delimited "," (seq expr)))))

(defn emit-re [expr]
  (let [[_ flags pattern] (re-find #"^(?:\(\?([idmsux]*)\))?(.*)" (str expr))]
    (print (str \/ (.replaceAll (re-matcher #"/" pattern) "\\\\/") \/ flags))))

;; Symbols are Chlorine's amazing pieces. We have a wide range of valid
;; characters for Chlorine just like Clojure. You can use Lisp-style naming
;; conventions such as "?" endings for predicate functions.

;; You can tell ChlorineJS to emit a symbol as if it's an other one by
;; using aliases
(def ^:dynamic *aliases*
  (ref '{;; `int` and `boolean` are reserved symbols in js.
         ;; They're also function names in Clojure and Chlorine core
         ;; library.
         int int*
         boolean boolean*

         ;; Chlorine uses a Clojure-like syntax of `(require ...)`
         ;; to load nodejs/browserify. It's implemented as macro which
         ;; expands to the lower level `require*`. `require*` in turn
         ;; emitted as javascript `require()`
         require* require
         }))

;; Because javascript doesn't allow such characters, the function
;; `chlorine.util/replace-map` will be used to replace all Clojure-only
;; characters to javascript-friendly ones.

;; The mapping used to do the replacements
(def ^:dynamic *symbol-map*
  (array-map
   "$"  "$USD$"
   "->" "$ARROW$"
   "=>" "$BARROW$"
   "<-" "$LARROW$"
   ">"  "$GT$"
   "<"  "$LT$"
   ">=" "$GE$"
   "=<" "$LE$"
   "-"  "_"
   "'"   "$QUOT$"
   "!"  "$EXCL$"
   "?"  "$QUEST$"
   "#"  "$HASH$"
   "%"  "$P100$"
   "&"  "$AND$"
   "*"  "$STAR$"
   "+"  "$PLUS$"
   "="  "$EQ$"
   "|"  "$PIPE$"
   ))

;; You can also specify "reserved symbols", which are NOT affected by
;; `replace-map`.
(def ^:dynamic *reserved-symbols* [#"^\$.*" #"^\.\$.*"])

(defn emit-symbol
  "Emits Clojure symbols to javascript ones. If the symbol is quoted, emits its
name as a string. Does some replacements with characters not supported by
javascript if the symbol isn't marked as reserved ones."
  [expr]
  (let [sym-name (name expr)]
    (print
     (if *quoted*
       (str "'" (name expr) "'")
       (if (or (reserved-symbol? *reserved-symbols* sym-name)
               *object-member*)
         sym-name
         (-> (or (get @*aliases* (symbol sym-name))
                 sym-name)
             (replace-map *symbol-map*)))))))

(defn emit-keyword
  "Emits Clojure keywords. Uses emit-symbol as backend."
  [expr]
  (binding [*quoted* true]
    (emit-symbol expr)))

;; Some Chlorine forms are converted directly to javascript native
;; operators: unary and infix ones.

;; Unary operators in Chlorine: "!"
;; Infix operators are consist of
;;   - `instance?` special form
;;   - and/or macros
;;   - some low-level math operators such as plus (+*), minus (-*), multiple (**)
;; and remainder (rem)
;;   - low-level comparator === which can't work on vectors and maps
;;   - binary operators
;;
;; Please use high-level functions from Chlorine's core library instead of
;; low-level ones"

(defn emit-unary-operator
  [op arg]
  (print (name op))
  (emit arg))

(defn emit-infix-operator
  [op & args]
  (let [clj->js {"instance?"       "instanceof"
                 "and"             "&&"
                 "or"              "||"
                 "=*"              "==="
                 "+*"              "+"
                 "-*"              "-"
                 "**"              "*"
                 "rem"             "%"
                 "bit-and"         "&"
                 "bit-or"          "|"
                 "bit-xor"         "^"
                 "bit-not"         "~"
                 "bit-shift-left"  "<<"
                 "bit-shift-right" ">>"
                 "bit-shift-right-zero-fill" ">>>"}
        js-op (get clj->js (name op) (name op))]
    (binding [*unique-return-expr* false
              *in-fn-toplevel* false]
      (with-parens []
        (emit-delimited (str " " js-op " ") args)))))

(defn property->member
  "Removes `-` prefix in a property name to bring it a member look."
  [property]
  (symbol (subs (name property) 1)))

(defn emit-function-call
  "Emits a function call by simply emitting the function name and its arguments
in parentheses."
  [fun & args]
  (emit fun)
  (with-parens []
    (with-indent [] (emit-delimited ", " args))))

(defn emit-invoke-function
  "Like emit-function-call, but wraps the function in parentheses. Used to
emit function calls where function is not a symbol but an other form instead."
  [fun & args]
  (with-parens [] (emit fun))
  (with-parens [] (emit-delimited "," args)))

;; All Clojure forms return something (even nil). Javascript is imperative
;; and its forms may or may not return values. Javascript function bodies
;; require a manual `return` keyword.
;;
;; That's why we create this dynamic var with initial value `false`,
;; change its value to `true` where necessary and "consume" `true` values
;; (print "return" and set the var back to `false`)
(def ^:dynamic *return-expr* false)

(defmacro with-return-expr
  "Consumes *return-expr* `true` states or sets it to a new value."
  [[& [new-val]] & body]
  `(binding [*return-expr* (if *return-expr*
                             (do
                               (print "return ")
                               false)
                             (or ~new-val false))]
     ~@body))

(defn emit-function-form
  "Emits function forms such as: unary and infix operator calls,
applying keyword on a map, method calls, creating new object calls,
and normal function calls."
  [form]
  (binding [*inline-if* true
            *unique-return-expr* false
            *in-fn-toplevel* false]
    (let [[fun & args]  form]
      (cond
       ;; those are not normal function calls
       (unary-operator? fun) (apply emit-unary-operator form)

       (infix-operator? fun) (apply emit-infix-operator form)

       (keyword? fun)
       (let [[map & default] args]
         (emit `(get ~map ~fun ~@default)))

       (member-form? fun)
       (let [[object & margs] args]
         (emit `(. ~object ~(normalize-dot-form fun) ~@margs)))

       (new-object? fun)
       (emit
        `(new ~(normalize-dot-form fun)
              ~@args))

       ;; Normal function calls:
       ;;  - Ensures caller are in parentheses by using `emit-invoke-function`
       ;;  instead of `emit-function-call` in case the caller is not simply
       ;;  a symbol.
       (coll? fun)           (apply emit-invoke-function form)

       true                  (apply emit-function-call form)))))

(defn emit-statement
  "Emits an expression with trailing `;` and `newline-indent` if necessary."
  [expr]
  (try+
   (binding [*inline-if* false]
     (if (and (coll? expr) (#{'defmacro 'load-js 'load-file 'load-file-macros}
                            (first expr)))
       (emit expr)
       (do
         (newline-indent)
         (emit expr)
         (when-not (and (coll? expr) (#{'do 'let 'let*} (first expr)))
           (print ";")))))
   (catch map? e
     (throw+ (merge e
                    {:causes (conj (or (:causes e) [])
                                   expr)})))
   (catch Throwable e
     (throw+ {:known-error false
              :msg (.getMessage e)
              :causes [expr]
              :trace e}))))

(defn emit-statements [exprs]
  (doseq [expr exprs]
    (emit-statement expr)))

(defn emit-statements-with-return
  "Emits statements with the manual `return` added in front of the
 last expression. If the last expression in `nil`, ignores it."
  [exprs]
  (binding [*return-expr* false]
    (doseq [expr (butlast exprs)]
      (emit-statement expr)))
  (when (not= 'nil (last exprs))
    (emit-statement (last exprs))))

;; Lispers love macros. In fact, making a macro means writing a function
;; that returns some code. Chlorine macros are nearly Clojure ones:
;; you can write the function (by `defmacro`) using all Clojure expressions,
;; even ones from external Clojure libraries (if you've already loaded them).
;; The only difference is that the generated code is treated as Chlorine one.

;; When defined, new macros are added to a ref holding a map. The map keys
;; are macro names while the values are the macro functions (the one that
;; generates code).
(def ^:dynamic *macros* (ref {}))

(defn macro?
  "Checks if a macro with that name is defined."
  [n] (and (symbol? n) (contains? @*macros* (name n))))

(defn get-macro
  "Gets the macro function by its name in order to generate code."
  [n] (and (symbol? n) (get @*macros* (name n))))

(defn undef-macro
  "Removes a macro from known macro list."
  [n]
  (when (macro? n)
    (when *print-pretty* (println "// undefining macro" n))
    (dosync (alter *macros* dissoc (name n)))))

(defmethod emit "defmacro" [[_ mname & mdeclrs]]
  (try+
   (let [mdeclrs (if (string? (first mdeclrs))
                   (rest mdeclrs)
                   mdeclrs)]
     (when *print-pretty* (println "// defining macro" mname))
     (dosync
      (alter *macros*
             assoc
             (name mname)
             (eval `(clojure.core/fn ~@mdeclrs)))))
   (catch Throwable e
     (throw+ {:known-error true
              :msg (str "Error defining macro `" mname "`:\n"
                        (.getMessage e))
              :causes [`(~_ ~mname ~@mdeclrs)]
              :trace e})))
  ;; returns `nil` because output are consumed by `with-out-str`
  nil)

(defn borrow-macros
  "Many Clojure macros work the same in Chlorine. Use this function to reuse
them instead of rewriting."
  [& syms]
  (doseq [sym syms]
    (dosync
     (alter *macros* conj
            {(name sym)
             (try+
              (fn [& args#]
                (apply (resolve sym) (concat [nil nil] args#)))
              (catch Throwable e
                (throw+ {:known-error true
                         :msg (str "Error borrowing macro `" sym "`:\n"
                                   (.getMessage e))
                         :causes [`(borrow-macros ~sym)]
                         :trace e})))}))))

(defn expand-macro-1
  "Gets and executes macro function, returns the Chlorine code."
  [form]
  (if (seq? form)
    (let [[mac-name & args] form]
      (if-let [mac (get-macro mac-name)]
        (try+
         (apply mac args)
         (catch Throwable e
           (throw+ {:known-error true
                    :msg   (str "Error expanding macro `" form "`:\n"
                                (.getMessage e))
                    :causes [form]
                    :trace e})))
        form))
    form))

(defn expand-macro
  "Repeatedly calls expand-macro-1 on form until it no longer
  represents a macro form, then returns it.  Note neither
  expand-macro-1 nor expand-macro expand macros in subforms."
  [form]
  (let [ex (expand-macro-1 form)]
    (if (identical? ex form)
      form
      (expand-macro ex))))

(defn emit-macro-expansion
  "Gets and executes macro function, emits the result as Chlorine code."
  [form]
  (emit (expand-macro-1 form)))

(defn emit-docstring
  "Prints docstrings as javascript comments."
  [docstring]
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

;; ChlorineJS produces a lot of temporary javascript symbols. To ensure
;; all these symbols are unique, we use this counter
(def  ^:dynamic *temp-sym-count* nil)

(defn tempsym
  "Generates an unique temporary symbol."
  []
  (dosync
   (ref-set *temp-sym-count* (+ 1 @*temp-sym-count*))
   (symbol (str "_temp_" @*temp-sym-count*))))

;; Chlorine supports the following Clojure binding forms:
;;  - Basic binding with just a single symbol
;;  - Destructuring binding with sequences or maps

(defn- emit-simple-binding [vname val]
  (emit (if (ignorable-arg? vname) (tempsym) vname))
  (print " = ")
  (binding [*inline-if* true]
    (emit val)))

(declare emit-var-bindings
         emit-destructured-seq-binding
         emit-destructured-map-binding)

(defn- emit-binding [vname val]
  (binding [*inline-if* true]
    (let [emitter (cond
                   (vector? vname) emit-destructured-seq-binding
                   (map? vname)    emit-destructured-map-binding
                   :else           emit-simple-binding)]
      (emitter vname val))))

;; Note on choice of get/get* in destructuring:
;;  - destructuring seq use `get*` for faster array access
;;  - destructuring map use `get` function which works correctly
;; on maps and supports default value when not found.
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
                    (throw+
                     {:known-error true
                      :msg (str "Unsupported binding form `" vvec "`:\n"
                                "only `:as` can follow `&`")
                      :causes [vvec]})
                  (not (symbol? vval))
                    (throw+
                     {:known-error true
                      :msg
                      (str  "Unsupported binding form `" vvec "`:\n"
                            "`&` must be followed by exactly one symbol")
                      :causes [vvec]})
                  :else
                    (do (emit-binding vval `(.slice ~temp ~i))
                        (recur (nnext vseq) (inc i) true)))
            :as (cond
                  (not= (count (nnext vseq)) 0)
                    (throw+
                     {:known-error true
                      :msg (str "Unsupported binding form `" vvec "`:\n"
                                "nothing may follow after `:as <binding>`")
                      :causes [vvec]})
                  (not (symbol? vval))
                    (throw+
                     {:known-error true
                      :msg (str "Unsupported binding form, `" vvec "`:\n"
                                "`:as` must be followed by a symbol")
                      :causes [vvec]})
                  :else
                    (emit-binding vval temp))
            (do (emit-binding vname `(get* ~temp ~i))
                (recur (next vseq) (inc i) seen-rest?))))))))

(defn- emit-destructured-map-binding [vmap val]
  (let [temp     (or (:as vmap) (tempsym))
        defaults (get vmap :or)
        keysmap  (reduce #(assoc %1 %2 (keyword %2))
                         {}
                         (mapcat vmap [:keys :strs :syms]))
        vmap     (merge (dissoc vmap :as :or :keys :strs :syms) keysmap)]
    (print (str temp " = "))
    (emit val)
    (doseq [[vname vkey] vmap]
      (print ", ")
      (cond
       (not (and (binding-form? vname)
                 (or (some #(% vkey) #{keyword? number? binding-form?}))))
       (throw+
        {:known-error true
         :msg (str "Unsupported binding form `" vmap "`:\n"
                   "binding symbols must be followed by keywords or numbers")
         :causes [vmap]})

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

(defn- emit-function [fdecl]
  (let [[fname fdecl] (if (symbol? (first fdecl))
                        [(first fdecl) (rest fdecl)]
                        [nil fdecl])
        docstring     (if (string? (first fdecl))
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
        (print "function ")
        (if fname (do (emit-symbol fname) (print " ")))
        (print "() {")
        (with-indent []
          (newline-indent)
          (print "var ")
          (emit-binding args '(Array.prototype.slice.call arguments))
          (print ";")))
      (do
        (print "function ")
        (if fname (do (emit-symbol fname) (print " ")))
        (print "(")
        (binding [*return-expr* false] (emit-delimited ", " args))
        (print ") {")))
    (with-indent []
      (when docstring
        (emit-docstring docstring))
      (binding [*return-expr* true
                *unique-return-expr* (when (= 1 (count body)) true)
                *in-fn-toplevel* false]
        (emit-statements-with-return body)
        ))
    (newline-indent)
    (print "}")))

;; We define local vars with `def`
(defmethod emit "def" [[_ name value]]
  (print "var ")
  (emit-symbol name)
  (print " = ")
  (binding [*inline-if* true]
    (emit value)))

(defmethod emit "alias" [[_ sym other]]
  (when *print-pretty* (println "// alias" sym "as" other))
  (dosync
   (alter *aliases* assoc sym other)))

;; Macro expansions are useful in REPL.
;; macroexpand-1 and macroexpand work the like in Clojure except:
;; - they're special forms, not functions and receive unquoted Chlorine forms
;; instead of quoted ones like in Clojure.
;; - they print out code as strings because javascript is not a Lisp.
;; - namespaces don't make sense in ChlorineJS so they're automatically removed.

(defn remove-namespaces
  "Removes all namespaces in forms using clojure.walk/postwalk."
  [forms]
  (clojure.walk/postwalk
   (fn [x] (if (symbol? x) (symbol (name x)) x))
   forms))

(defmethod emit "macroexpand-1" [[_ form]]
  (emit  (pr-str (remove-namespaces (expand-macro-1 form)))))

(defmethod emit "macroexpand" [[_ form]]
  (emit (pr-str (remove-namespaces (expand-macro form)))))

;; Low-level function form. Please use `fn` and `defn` macros instead
(defmethod emit "fn*" [[_ & fdecl]]
  (with-return-expr []
    (with-block (emit-function fdecl))))

;; Javascript's `if` expressions don't return values directly [1]. That's
;; opposite to Clojure/Chlorine where returning something is a must
;; (functional programming means efficiency!)
;; Just keep writing `(if exprs)` as usual, and ChlorineJS will determine
;; whether an `if` expression should return a value *directly* or not.
;; If 'yes', outputs the "inline" syntax as following:
;; `{{test}} ? {{consequent}}: {{alternate}}`
;;
;; [1]: Javascript's `if` with `return` in it is for the upper
;; function but itself

(defn emit-inline-if
  [test consequent alternate]
  (with-return-expr []
    (with-parens []
      (emit test)
      (print " ? ")
      (emit consequent)
      (print " : ")
      (emit alternate))))
;; If 'no', traditional javascript `if` will be used instead.
(defn emit-block-if [test consequent alternate]
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
  (when-not (or (nil? alternate)
                (= '(clojure.core/cond)
                   alternate))
    (print " else {")
    (with-block
      (with-indent []
        (emit-statement alternate)))
    (newline-indent)
    (print "}")))

(defmethod emit "if" [[_ test consequent & [alternate]]]
  ;; emit consequent directly without printing checks
  ;; used to optimize `cond` macro output
  (if (and *inline-if* consequent)
    (emit-inline-if test consequent alternate)
    (emit-block-if test consequent alternate)))

;; Clojure/ChlorineJS `(case ...)`syntax will output
;; javascript `switch ... case` equivalent.

(defn emit-case [e clauses]
  (binding [*unique-return-expr* false
            *in-fn-toplevel* false]
    (let [pairs (partition 2 clauses)]
      (print "switch (")
      (binding [*return-expr* false]
        (emit e))
      (print ") {")
      (doseq [[k v] pairs]
        (with-indent []
          (newline-indent)
          (print "case " )
          (binding [*return-expr* false]
            (emit k))
          (print ":")
          (with-block
            (with-indent []
              (emit-statement v)
              (newline-indent)
              (when-not *return-expr*
                (print "break;")))))))

    (when (odd? (count clauses))
      (with-indent []
        (newline-indent)
        (print "default:")
        (with-block
          (with-indent []
            (emit-statement (last clauses)))))))
  (newline-indent)
  (print "}"))

(defmethod emit "case" [[_ e & clauses]]
  (if *inline-if*
    (do
      (print "(function(){")
      (binding [*return-expr* true]
        (with-indent []
          (newline-indent)
          (emit-case e clauses)))
      (print "})()"))
    (emit-case e clauses)))

(defmethod emit "do" [[_ & exprs]]
  (if *inline-if*
    (do
      (print "(function(){")
      (binding [*return-expr* true]
        (with-indent []
          (newline-indent)
          (emit-statements-with-return exprs)))
      (print "})()"))
    (emit-statements-with-return exprs)))

;; `let` is a Clojure fundamental form that provides lexical bindings
;; of data structures to symbols.
;; The binding is available only within the lexical context of the let.
;;
;; Chlorine implements the same behavior of `let` by wrapping the body
;; inside a function in most cases.

(defmethod emit "let" [[_ bindings & exprs]]
  (let [emit-var-decls (fn []
                         (print "var ")
                         (binding [*return-expr* false]
                           (with-block (emit-var-bindings bindings))
                           (print ";"))
                         (emit-statements-with-return exprs))
        emit-let-fun (fn []
                       (print "(function () {")
                       (with-indent []
                         (newline-indent)
                         (binding [*return-expr* true]
                           (emit-var-decls)))
                       (newline-indent)
                       (print " })()"))]
    (cond
     *inline-if*
     (with-return-expr []
       (emit-let-fun))
     *unique-return-expr* ;; *in-fn-toplevel*
     (binding [*unique-return-expr* false]
       (emit-var-decls))

     *return-expr*
     (with-return-expr []
       (emit-let-fun))

     :default
     (do (emit-let-fun)
         (print ";")))))

;; "Leaky" versions of `let` that don't wrap anything inside a function.
(defmethod emit "let!" [[_ & bindings]]
  (binding [*return-expr* false]
    (with-block (emit-var-bindings bindings))
    (print ";")))

(defmethod emit "let*" [[_ & bindings]]
  (print "var ")
  (binding [*return-expr* false]
    (with-block (emit-var-bindings bindings))
    (print ";")))

(defmethod emit "new" [[_ class & args]]
  (with-return-expr []
    (binding [*inline-if* true]
      (print "new ")
      (emit class)
      (with-parens [] (emit-delimited "," args)))))

(defmethod emit "delete" [[_ item]]
  (with-return-expr []
    (binding [*inline-if* true]
      (print "delete ")
      (emit item))))

(defmethod emit "return" [[_ value]]
  (print "return ")
  (emit value))

;; Low-level form to directly access object properties/array indexes.
;; Use `get` (in core library) which support default value when not found
;; instead
(defmethod emit "get*" [[_ map key]]
  (with-return-expr []
    (emit map)
    (print "[")
    (emit key)
    (print "]")))

(defmethod emit "." [[_ object key & args]]
  (with-return-expr []
    (emit object)
    (print ".")
    (cond
     (symbol? key)
     (if (.startsWith (name key) "-")
       (binding [*object-member* true]
         (emit (property->member key)))
       (do (binding [*object-member* true]
             (emit key))
           (with-parens []
             (with-indent [] (emit-delimited ", " args)))))
     (coll? key)
     (do (binding [*object-member* true]
           (emit (first key)))
         (with-parens []
           (with-indent [] (emit-delimited ", " (rest key))))))))

(defmethod emit "set!" [[_ & apairs]]
  (binding [*return-expr* false
            *in-fn-toplevel* false
            *unique-return-expr* false
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
                                   *in-block-exp?* false]
                           (emit-var-bindings bindings))
                         (print "; true;) {")
                         (with-indent []
                           (binding [*loop-vars* (first (unzip bindings))]
                             (emit-statements-with-return body))
                           (newline-indent)
                           (print "break;"))
                         (newline-indent)
                         (print "}"))]
    (if (or *in-fn-toplevel* *unique-return-expr*)
      (binding [*unique-return-expr* false
                *in-fn-toplevel* false]
        (emit-for-block))
      (with-return-expr []
        (print "(function () {")
        (binding [*return-expr* true]
          (with-indent []
            (newline-indent)
            (emit-for-block))
          (newline-indent))
        (print "}).call(this)")))))

(defmethod emit "recur" [[_ & args]]
  (binding [*return-expr* false]
    (let [tmp (tempsym)]
      (print "var" (emit-str tmp) "= ")
      (emit-vector args)
      (println ";")
      (emit-statements (map (fn [lvar i] `(set! ~lvar (get* ~tmp ~i)))
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

(defmethod emit "dofor" [[_ [init-bindings test update] & body]]
  (let [init (if (vector? init-bindings)
               `(let* ~@init-bindings)
               init-bindings)]
    (binding [*return-expr* false]
      (print "for (")
      (emit-statements [init test update])
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
       (nil? expr) (print "void(0)")
       true (print expr)))))

(defn emit-str [expr]
  (binding [*return-expr* false
            *inline-if* true]
    (with-out-str (emit expr))))

(defn js-emit [expr] (emit expr))

(defmacro js
  "Translate the Clojure subset `exprs' to a string of javascript
code."
  [& exprs]
  (let [exprs# `(quote ~exprs)]
    `(binding [*temp-sym-count* (ref 999)]
       (with-out-str
         (if (< 1 (count ~exprs#))
           (emit-statements ~exprs#)
           (js-emit (first ~exprs#)))))))

(defmacro js-let
  "Bind Clojure environment values to named vars of a cljs block, and
translate the Clojure subset `exprs' to a string of javascript code."
  [bindings & exprs]
  (let [form# 'fn*
        [formals# actuals#] (unzip bindings)]
    `(with-out-str
       (emit-statement (list '(~form# ~(vec formals#) ~@exprs) ~@actuals#)))))

(defmacro let-js
  "Bind Clojure environment values to named vars of a quoted cljs block, and
translate the Clojure subset `exprs' to a string of javascript code."
  [bindings quoted-expr]
  (let [body# `(let ~bindings ~quoted-expr)]
    `(with-out-str
       (js-emit ~body#))))

(declare raw-script)

;; Chlorine doesn't support an official way to modularize code like Clojure
;; with namespaces. Instead, Chlorine provides a basic syntax to load code
;; from other files into the current file as if they are one. This can be
;; done with `load-file`

(defmethod emit "load-file" [[_ & files]]
  ;(print (str (apply tojs' files)))
  (doseq [file files]
    (when *print-pretty* (println "// <-- Starts loading file: " file))
    (if-let [content (tojs' file)]
      (print (str content)))
    (when *print-pretty* (println "// Ends loading file: " file " -->"))))

;; Sometimes you only want to load macros from an outside file and print out
;; nothing. Use `load-file-macros` then
(defmethod emit "load-file-macros" [[_ & files]]
  (doseq [file files]
    (when *print-pretty* (println "// Loads macros from file: " file))
    (tojs' file)))

;; Inlines raw javascript from files instead of Chlorine ones.
(defmethod emit "load-js" [[_ & files]]
  (doseq [file files]
    (when *print-pretty* (println "// <-- Starts Javascipt file: " file))
    (if-let [content (raw-script file)]
      (print (str content)))
    (when *print-pretty* (println "// Ends Javascript file: " file " -->"))))

(defn raw-script [& scripts]
  (with-out-str
    (doseq [script scripts
            :let [file (find-in-paths script)
                  dir  (get-dir file)]]
      (binding [*cwd* dir]
        (if (nil? file) (throw+ {:known-error true
                                 :msg
                                 "File not found `" script "`"
                                 :causes [script]}))
        (let [f (if (resource-path? file)
                  (to-resource file)
                  file)]
          (print (slurp f)))))))

(defn tojs'
  "The low-level, stateful way to compile Chlorine source files. This function
varies depending on states such as macros, temporary symbol count etc."
  [& scripts]
  (with-out-str
    (doseq [script scripts
            :let [file (find-in-paths script)
                  dir  (get-dir file)]]
      (binding [*cwd* dir]
        (try+
         (if (nil? file) (throw+ {:known-error true
                                  :msg
                                  "File not found `" script "`"
                                  :causes [script]}))
         (let [f (if (resource-path? file)
                   (to-resource file)
                   file)]
           (with-open [in (sexp-reader f)]
             (loop [expr (read in false :eof)]
               (when (not= expr :eof)
                 (when-let [s (emit-statement expr)]
                   (print s))
                 (recur (read in false :eof))))))
         (catch map? e
           (throw+ (merge e
                          {:causes (conj (or (:causes e) [])
                                         file)})))
         (catch RuntimeException e
           (if (= (.getMessage e) "EOF while reading")
             (throw+ {:known-error true
                      :msg (str "EOF while reading file "
                                file "\n"
                                "Maybe you've got mismatched parentheses,"
                                " brackets or braces.")
                      :causes [file]
                      :trace e})
             (throw+ {:known-error false
                      :msg (.getMessage e)
                      :causes [file]
                      :trace e})))
         )))))

(defn tojs
  "The top-level, stateless way to compile Chlorine source files.
Loads and compiles a list of cl2 scripts into javascript, and
returns them in a string. This function starts its own temporary symbol count
 and macro memory."
  [& scripts]
  (binding [*temp-sym-count* (ref 999)
            *macros*         (ref {})]
    (apply tojs' scripts)))
