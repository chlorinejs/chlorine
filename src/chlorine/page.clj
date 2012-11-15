(ns chlorine.page
  (:use [chlorine.js :only [js js-let]]
        [chlorine.util :only [unzip]]))

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