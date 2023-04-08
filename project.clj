(defproject ircbot "0.1.0-SNAPSHOT"
  :main ircbot.core
  :description "Clojure Bot"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/tools.logging "0.4.0"]
                 [org.clojure/core.memoize "0.5.2"]
                 [log4j/log4j "1.2.16"]
                 [clj-time "0.14.0"]
                 [clj-http "2.3.0"]
                 [org.clojure/data.json "0.2.3"]]
  :dev-dependencies [[lein-swank "1.4.3"]])


