(defproject chlorine "1.5.0"
  :description "A naive Clojure to Javascript translator"
  :url "http://github.com/myguidingstar/chlorine"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [pathetic "0.4.0"]]
  :profiles {:provided
             {:dependencies
              [[evaljs "0.1.2"]]}})
