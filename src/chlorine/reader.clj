(ns chlorine.reader
  (:use [clojure.java.io :only [reader]]
        [chlorine.util :only [re?]]))

(defn sexp-reader
  "Wrap `source' in a reader suitable to pass to `read'."
  [source]
  (new java.io.PushbackReader (reader source)))

(defn unary-operator? [op]
  (and (symbol? op) (contains? #{"++" "--" "!"} (name op))))

(defn infix-operator? [op]
  (and (symbol? op)
       (contains? #{"and" "or"
                    "bit-and" "bit-or" "bit-xor" "bit-not"
                    "bit-shift-left" "bit-shift-right"
                    "bit-shift-right-zero-fill"
                    "rem"
                    "+*" "-*" "**" "/"
                    ">" ">=" "<" "<="
                    ;;"="
                    "=="
                    "==="
                    "not="
                    ;;"!="
                    "!=="
                    "in"
                    "instance?" "instanceof"}
                  (name op))))

(defn destructuring-form? [form]
  (or (map? form) (vector? form)))

(defn binding-form? [form]
  (or (symbol? form) (destructuring-form? form)))

(defn binding-special? [form]
  (contains? #{'& :as} form))

(defn reserved-symbol?
  "Checks if a string is specified in reserved symbol list."
  [v s]
  (let [strings (filter string? v)
        regexps (filter re? v)]
    (or (contains? (set strings) s)
        (some #(re-matches % s) regexps))))
