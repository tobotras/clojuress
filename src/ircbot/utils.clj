(ns ircbot.utils
  (:use [clojure.contrib.logging :as log :only [error debug]])
  (:import (java.net URL URLEncoder InetAddress)
           (java.lang StringBuilder)
           (java.io BufferedReader InputStreamReader StringReader PushbackReader ByteArrayInputStream))
  (:require [clojure.contrib.str-utils2 :as str2])
  (:gen-class))

(defn whisper [ & args ]
  (debug (apply str args)))

(defn begins-with? [string substr]
  {:pre [(not (or (nil? string) (nil? substr)))]}
  (let [len (count substr)]
    (and (>= (count string) len)
         (= (subs string 0 len) substr))))

(defn triml-regex [re-string string]
  {:pre [(not (or (nil? re-string) (nil? string)))]}
  (let [[_ match rest-of-string] (re-matches (re-pattern (str "(" re-string ")(.*)")) string)]
    (if (nil? match)
      string
      rest-of-string)))

(defn triml-whitespace [string]
  {:pre [(not (nil? string))]}
  "clojure.string/trim unfortunately strips control symbols too"
  (triml-regex "[ \t]*" string))

(defn eat-spaces [str]
  (str2/replace str #" +" " "))

(defn call-hook [sym]
  (if-let [v (resolve sym)]
    (apply v nil)
    nil))

(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

(defn numeric? [char]
  (in? "0123456789" char))

(defn stringify [the-list]
  (apply str (interpose " " the-list)))

(defmethod print-dup java.util.Date [o w]
  (print-ctor o (fn [o w] (print-dup (.getTime  o) w)) w))

(defn serialize
  "Print a data structure to a file so that we may read it in later."
  [data-structure #^String filename]
  (binding [*print-dup* true] 
    (spit filename (pr-str data-structure))))
 
;; This allows us to then read in the structure at a later time, like so:
(defn deserialize [filename]
  (read-string (slurp filename)))

(defn now []
  (java.util.Date.))
