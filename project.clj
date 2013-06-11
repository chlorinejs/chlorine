(defproject chlorine "1.5.4"
  :description "A naive Clojure to Javascript translator"
  :url "http://github.com/chlorinejs/chlorine"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [pathetic "0.4.0" :exclusions [org.clojure/clojure]]
                 [chlorine-utils "1.1.0"]
                 [hiccup "1.0.3"]
                 [slingshot "0.10.3"]
                 [org.clojure/tools.reader "0.7.4"]])
