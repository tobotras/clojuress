(ns ircbot.test.core
  (:use ircbot.core ircbot.utils 
        clojure.test clojure.xml)
  (:import (java.io ByteArrayInputStream)))

(deftest begins-with-check
  (is (begins-with? "long string" "long") "begins-with? failed check")
  (is (not (begins-with? "long string" "string")) "begins-with? false positive"))

(deftest numeric-check
  (is (numeric? \7) "numeric? failed check")
  (is (not (numeric? \A)) "numeric? false positive"))

(deftest triml-checks
  (is (= (triml-regex "[0-9]+" "123qwe") "qwe") "triml-regex failure")
  (is (= (triml-whitespace "        string") "string") "triml-whitespace failure"))

(defn the-hook []
  "qwe")

(deftest call-hook-check
  (is (= (call-hook 'the-hook) "qwe") "call-hook failed"))

(deftest in-check
  (is (= (in? '(1 2 3) 1)) "in? failed")
  (is (not (in? '(2 3 4) 1)) "in? false positive"))

(deftest stringify-check
  (is (= (stringify '(1 2 3)) "1 2 3") "stringify failed"))

(deftest get-xml-element-check
  (let [xml-data "<?xml version='1.0'?><body><data>content1</data><data>content2</data></body>"
        xml (xml-seq (parse (ByteArrayInputStream. (.getBytes (.trim xml-data)))))
        content1 (get-xml-element xml :data)
        content2 (get-xml-element xml :data 1)]
    (is (= content1 "content1") "get-xml-element failed")
    (is (= content2 "content2") "get-xml-element failed to find second occurence")))

(deftest resolve-IP-check
  (is (= (resolve-IP "localhost") "127.0.0.1") "resolve-IP failed to resolve localhost"))

(deftest expired-check
  (let [now #(java.util.Date. 112 10 20 14 30 0)
        recent-stamp (java.util.Date. 112 10 20 14 29 0)
        old-stamp (java.util.Date. 112 10 20 13 59 0)]
    (is (= (expired? old-stamp (* 30 60))) "expired? failed on old stamp")
    (is (not (= expired? recent-stamp (* 30 60))) "expired? failed on recent stamp")))

(deftest find-real-user-by-AKA-and-lookup-user-entry-check
  (binding [*userdb* (hash-map "vasya" (hash-map :password "the-password" :admin false :city "Nowhere" :AKA "pupkin" :logged-at (now))
                               "petya" (hash-map :password "the-password" :admin false :city "Nowhere" :AKA "petrov" :logged-at (now)))]
    (let [entry (find-real-user-by-AKA "pupkin")]
      (is (not (nil? entry)) "find-real-user-by-AKA failed to find user")
      (is (and (= (first entry) "vasya") (= (:AKA (second entry)) "pupkin")) "find-real-user-by-AKA found wrong user"))
    (let [name-entry (lookup-user-entry "VasyA")
          aka-entry (lookup-user-entry "PupKin")]
      (is (not (nil? name-entry)) "lookup-user-entry failed to find entry by name")
      (is (not (nil? aka-entry)) "lookup-user-entry failed to find entry by AKA")
      (is (= name-entry aka-entry) "lookup-user-entry found different entries for name and AKA"))))

(deftest serialize-check
  (let [o #{:a :b :c #{:d :e :f}}
        name "/tmp/foobarbaz"]
    (serialize o name)
    (let [o2 (deserialize name)]
      (is (= o o2) (str "serialize+deserialize returned different objects: " o " vs " o2)))))

(deftest register-user-check
  (register-user "vasya" "the-password" false)
  (is (not (nil? (lookup-user-entry "Vasya"))) "register-user failed to register user")
  (unregister-user "Vasya")
  (is (nil? (lookup-user-entry "Vasya")) "unregister-user failed to unregister user"))

(deftest authenticate-user-check
  (binding [*userdb* (hash-map "vasya" (hash-map :password "v-password" :admin false :city "Nowhere" :AKA "pupkin" :logged-at (now)))]
    (is (= true (authenticate-user "vasya" "v-password")) "user is not authenticated by correct pass")
    (is (= false (authenticate-user "vasya" "v-pass")) "user is authenticated by incorrect pass")))

(deftest real-name-check
  (binding [*userdb* (hash-map "vasya" (hash-map :password "v-password" :admin false :city "Nowhere" :AKA "pupkin" :logged-at (now)))]
    (is (= "vasya" (real-name "vasya")) "real-name didn't find user by name")
    (is (= "vasya" (real-name "pupkin")) "real-name didn't find user by AKA")))

(deftest known-user-check
  (binding [*userdb* (hash-map "vasya" (hash-map :password "v-password" :admin false :city "Nowhere" :AKA "pupkin" :logged-at (now))
                               "petya" (hash-map :password "v-password" :admin false :city "Nowhere" :AKA "petroff"
                                         :logged-at (java.util.Date. 111 10 20 17 21 0)))]
    (is (= true (known-user? "vasya")) "known-user? failed to know user by name")
    (is (= true (known-user? "pupkin")) "known-user? failed to know user by AKA")
    (is (= false (known-user? "petya")) "known-user? failed to expire session")))

(deftest admin-check
  (binding [*userdb* (hash-map "vasya" (hash-map :password "v-password" :admin true :city "Nowhere" :AKA "pupkin" :logged-at (now))
                               "petya" (hash-map :password "v-password" :admin true :city "Nowhere" :AKA "pupkin" 
                                                 :logged-at (java.util.Date. 111 10 20 17 21 0))
                               "vova" (hash-map :password "v-password" :admin false :city "Nowhere" :AKA "pupkin" :logged-at (now)))]
    (is (= true (admin? "vasya")) "admin? failed to recognize vasya")
    (is (= false (admin? "petya")) "admin? failed to expire petya")
    (is (= false (admin? "vova")) "admin? failed to refuse vova")))

(deftest trim-is-test
  (is (= (trim-is "one") "one") "trim-is trimmed some extra")
  (is (= (trim-is "  is   one") "one") "trim-is doesn't"))

(deftest make-action-test
  (is (= (make-action "something") (str (char 1) "ACTION something" (char 1))) "make-action failed"))

(deftest server-user-test
  (is (= (server-user "~Tobotras@xtalk.msk.su") "Tobotras") "server-user failed"))

(deftest eat-spaces-test
  (is (= (eat-spaces "   some    sparse    text     ") " some sparse text ") "eat-spaces ate wrong"))

