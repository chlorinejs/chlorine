(ns chlorine.page
  (:use [chlorine.js :only [js js-let]]
        [chlorine.util :only [unzip]]))

(defmacro script
  "Similar to the (js ...) form, but wraps the javascript in a
 [:script ] element, which can be passed to hiccup.core/html."
  [& forms]
  `[:script {:type "text/javascript"}
    (js ~@forms)])

(defmacro script-let
  "Similar to the (js-let ...) form, but wraps the javascript in a
 [:script ] element, which can be passed to hiccup.core/html."
  [bindings & forms]
  `[:script {:type "text/javascript"}
    (js-let ~bindings ~@forms)])

(defmacro jq
  "Similar to the (js ...) form, but wraps the javascript in a
 [:script ] element which is invoked on a jQuery document.ready
 event."
  [& forms]
  (let [fnform# 'fn]
    `[:script {:type "text/javascript"}
      (js (.ready ($ document) (~fnform# [] ~@forms)))]))

(defmacro jq-let
  "Similar to the (js-let ...) form, but wraps the javascript in a
 [:script ] element which is invoked on a jQuery document.ready
 event."
  [bindings & forms]
  (let [fnform# 'fn
        [formals# actuals#] (unzip bindings)]
    `[:script {:type "text/javascript"}
      "$(document).ready(function () {"
      (js-let ~bindings ~@forms)
      "});"]))