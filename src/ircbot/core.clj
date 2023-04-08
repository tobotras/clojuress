(ns ircbot.core
  (:use [clojure.string :only [lower-case trim trimr split]]
        clojure.java.io
        clojure.xml
        clojure.contrib.math
        clojure.core.memoize
        [clojure.contrib.logging :as log :only [error debug]]
        ircbot.utils)
  (:require [clojure.contrib.str-utils2 :as str2]
            [clojure.data.json :as json]
            [clj-http.client :as http-client]
            [clj-time.core :as ctime]
            [clj-time.format :as ftime]
            clojure.java.shell)
  (:import (java.net URL URLEncoder URLDecoder InetAddress)
           (java.io BufferedReader InputStreamReader OutputStreamWriter StringReader PrintWriter IOException FileNotFoundException))
  (:gen-class))

;;; Globals

(def ^:dynamic *current-nick* nil)

(def bot-config
  { :server "irc.atw-inter.net"
   :port 6667
   :nick "clojuress"
   :to-join "#fidorus"
   :user-db-file "/var/tmp/clojuress.db"
   :saved-urls-file "/var/tmp/clojuress.urls.db"
   :kb-file "/var/tmp/clojuress.knowledge.db"
   :log-file-prefix "/var/tmp/clojuress."
   :session-expiration-time 300
   :health-check-period 60
   :currency-expiration-time (* 3600 4)
   :max-saved-urls 3000
   :max-definitions 20
   :daily 7
   :max-grep-lines 5
   :who-am-i (str "ClojureBot 0.1 " "$Rev: 123 $")
   })

(defn get-xml-element [ xml tag & { :keys [ occurence ] :or { occurence 0 } } ]
  (first (:content 
          (get (vec (filter #(= tag (:tag %)) xml)) occurence))))

(defn resolve-IP [hostname]
  (whisper "Resolving " hostname)
  (and hostname
       (second (split (.toString (java.net.InetAddress/getByName hostname)) #"/"))))

(defn- load-bot-config [filename]
  (try 
    (let [x (xml-seq (parse filename))
          xml-element (partial get-xml-element x)]
      (def ^:dynamic bot-config 
        (merge bot-config
               {:server (trim (xml-element :server))
                :port (Integer. (xml-element :port))
                :nick (trim (xml-element :nick))
                :to-join (xml-element :channel)
                :user-db-file (xml-element :users)
                :saved-urls-file (xml-element :urls)
                :kb-file (xml-element :knowledge-base)
                :log-file-prefix (xml-element :log-prefix)
                :currency-expiration-time (* 3600 (Integer. (xml-element :currency)))
                :session-expiration-time (Integer. (xml-element :session))
                :max-saved-urls (Integer. (xml-element :saved-urls))
                :max-definitions (Integer. (xml-element :definitions))
                :daily (Integer. (xml-element :public-per-day))
                :max-grep-lines (Integer. (xml-element :grep-lines))}))
      (whisper "Configured by " filename))
    (catch FileNotFoundException e
      (whisper (format "No config file found: %s, using default configuration" filename))))
  (whisper "Bot configuration: " bot-config))

;;; Persistent data
;; User DB
;(defstruct user-entry :password :admin :city :AKA :logged-at :last-seen :sents)
(def user-db (atom { "tobotras" { :password "q1", :admin true } }))

(defn userdb-changed [_ _ old new]
  (when (not= old new)
    (whisper "Updating user DB")
    (serialize new (:user-db-file bot-config))))

(add-watch user-db :key userdb-changed)

;; Saved URLs
(def saved-urls (atom ()))

(defn saved-urls-changed [_ _ old new]
  (let [new-urls (distinct new)]
    (when-not (= old new-urls)
      (whisper "New URLs found, updating urls file")
      (with-open [output (clojure.java.io/writer (:saved-urls-file bot-config))]
        (binding [*out* output]
          (pr (take (:max-saved-urls bot-config) new-urls)))))))

(add-watch saved-urls :key saved-urls-changed)

(defn load-saved-urls [file]
  (when (.exists (as-file file))
    (reset! saved-urls (read-string (slurp file)))))

;; Knowledge base
;; (assoc *knowledge-base* term (list def1 def2...))
(def ^:dynamic *knowledge-base* {})

(def ^:dynamic *channels* {})
(def ^:dynamic *recent-channel* nil)

;; Per-user statistics of public commands
;;(defstruct user-command-entry :last :count)
(def ^:dynamic *user-commands* {})

;;(defstruct request-entry :request :requestor :command)
;; index by target
(def ^:dynamic *request-list* {})

(defn socket [host port]
  (let [socket (java.net.Socket. host port)
        in (BufferedReader. 
            (InputStreamReader. (.getInputStream socket) "KOI8_R"))
        out (PrintWriter.
             (OutputStreamWriter. (.getOutputStream socket) "KOI8_R"))]
    {:in in :out out}))

(defn login []
  "Login to IRC server"
  (println "PASS qq")
  (loop [nick (:nick bot-config)]
    (println "USER" nick "sw _ ClojureBot")
    (println "NICK" nick)
    (let [line (read-line)]
      (whisper "< " line)
      (when (or (nil? line)
                (= (first (split line #"\s")) "ERROR"))
        (throw (Exception. (str "Cannot log in to server: " line ))))
      (if-let [[_ code _] (re-matches (re-pattern (str ":[^ ]+ ([0-9][0-9][0-9]) (\\* )?" nick " :.*")) line)]
        (when (= code "433")
          (let [new-nick (str nick "_")]
            (def ^:dynamic *current-nick* new-nick)
            (whisper "Changed nick to " new-nick)
            (println "NICK" new-nick)
            (recur new-nick)))
        (recur nick)))))

(defn notice [ whom & text ]
  (println "NOTICE" whom (apply str ":" text)))

(defn shout [whom & text]
  (loop [rest-of-line (apply str text)
         rest-of-len (count rest-of-line)]
    (let [SEGMENT-SIZE 400
          segment (subs rest-of-line 0 (min SEGMENT-SIZE rest-of-len))]
      (println "PRIVMSG" whom (apply str ":" segment))
      (Thread/sleep 1000)
      (when (> rest-of-len SEGMENT-SIZE)
        (recur (subs rest-of-line SEGMENT-SIZE) (- rest-of-len SEGMENT-SIZE))))))

(defn update-knowledge-base [term defs]
  (whisper "update-knowledge-base. Term" term "defs" defs)
  (def ^:dynamic *knowledge-base*
    (if (empty? defs)
      (dissoc *knowledge-base* term)
      (assoc *knowledge-base* term defs)))
  (serialize *knowledge-base* (:kb-file bot-config))
  true)

(defn trim-is [text]
  (let [txt (str2/trim text)]
    (if (begins-with? txt "is")
      (str2/trim (subs txt 2))
      txt)))

(defn remember-term-definition [term_ text_]
  (let [term (trim term_)
        text (trim text_)
        known-term (*knowledge-base* term)
        new-def (conj known-term text)]
    (when-not (or (> (count known-term) (:max-definitions bot-config))
                  (in? known-term text))
      (update-knowledge-base term new-def))))

(defn act-encode [ txt ]
  (str (char 1) txt (char 1)))

(defn request-time [name whom_]
  (whisper "Requesting time from " whom_)
  (if (empty? whom_)
    (shout name "Whose time do you want?")
    (let [whom (lower-case (triml-regex "[ \t]*@?" whom_))]
      (shout whom (act-encode "TIME"))
      (def ^:dynamic *request-list*
        (assoc *request-list* whom 
               {:command (str ":" (char 1) "TIME")
                :request "time request"
                :requestor name })))))

(defn handle-notice [name_ text]
  (whisper "handle-notice '" name_ "', '" text "'")
  (let [name (lower-case name_)
        parsed-text (split (trim text) #"\s")
        command (trimr (first parsed-text))]
    (whisper "Looking up queued request for '" command "' by '" name "'")
    (if-let [entry (*request-list* name)]
      (let [cmd (:command entry)
            requestor (:requestor entry)
            request (:request entry)]
        (if (= cmd command)
          (do (shout requestor name " has responded to " request " with '" (stringify (rest parsed-text)) "'")
              (def ^:dynamic *request-list*
                (dissoc *request-list* name)))
          (do (doseq [ch (str2/codepoints command)] (whisper "Char: " ch) )
              (whisper "Got an unexpected '" command "' notice from '" name "' while waiting for '" cmd "'"))))
      (whisper "Got an unsolicited '" command "' notice from '" name))))

(defn ask-google [whom term]
  (whisper "Googling for " term)
  (let [uc (.openConnection 
            (URL. (format "http://www.google.com/search?q=%s&btnI=I&src=navclient" (URLEncoder/encode term "UTF-8"))))]
    (whisper "configuring URL connection")
    (doto uc
      (.setRequestProperty "Referer" "http://www.google.com")
      (.setRequestProperty "User-Agent" "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.1.5) Gecko/20091102 Firefox/3.5.5")
      (.setInstanceFollowRedirects false))
    (whisper "Streaming")
    (try
      (let [content (apply str (-> uc .getInputStream InputStreamReader. BufferedReader. line-seq))]
        (whisper "Google response:" content)
        (if-let [[_ redir] (re-matches #"(?s).*302 Moved.*<A HREF=\"(.*)\">here</A>.*" content)]
          (if (re-matches #".*google.com/sorry.*" redir)
            (shout whom "Google hates you!")
            (do
              (remember-term-definition term redir)
              (shout whom term ": see " redir)))
          (shout whom "Nothing is known about " term)))
      (catch IOException e
        (whisper "IOException: " e)))))

(defn- host-to-coords [hostname]
  (whisper "Geolocating " hostname)
  (let [ip-addr (resolve-IP hostname)
        json (json/read-str (http-client/get (str "http://ipinfo.io/" ip-addr)))
        coords (json "loc")]
    (whisper "--> " coords)
    coords))

(defn parse-wwo-xml [url]
  "Parse worldweatheronline API response"  
  (try
    (let [xml (xml-seq (parse url))
          query (get-xml-element xml :query)
          observation-time (get-xml-element xml :observation_time)
          cur-temp-C (get-xml-element xml :temp_C)
          cur-desc (get-xml-element xml :weatherDesc)
          today-temp-min (get-xml-element xml :tempMinC)
          today-temp-max (get-xml-element xml :tempMaxC)
          today-desc (get-xml-element xml :weatherDesc :occurence 1)
          forecast-temp-min (get-xml-element xml :tempMinC :occurence 1)
          forecast-temp-max (get-xml-element xml :tempMaxC :occurence 1)
          forecast-desc (get-xml-element xml :weatherDesc :occurence 2)]
      (if (and cur-temp-C cur-desc observation-time query today-temp-min today-temp-max 
               today-desc forecast-desc forecast-temp-max forecast-temp-min)
        (format "%s (%s GMT): %sC, %s. Today: %sC..%sC, %s. Forecast: %sC..%sC, %s."
                query observation-time cur-temp-C (lower-case (trim cur-desc))
                today-temp-min today-temp-max (lower-case (trim today-desc))
                forecast-temp-min forecast-temp-max (lower-case (trim forecast-desc)))
        nil))
    (catch java.io.IOException e
      (whisper "Cannot load weather:" e))))


;; {:tag :time,
;;  :attrs {:from "2017-06-11T18:00:00", :to "2017-06-11T21:00:00"},
;;  :content [
;;            {:tag :symbol, :attrs {:number "803", :name "broken clouds", :var "04n"}, :content nil}
;;            {:tag :precipitation, :attrs nil, :content nil}
;;            {:tag :windDirection, :attrs {:deg "274.505", :code "W", :name "West"}, :content nil}
;;            {:tag :windSpeed, :attrs {:mps "1.86", :name "Light breeze"}, :content nil}
;;            {:tag :temperature, :attrs {:unit "kelvin", :value "289.847", :min "289.847", :max "289.847"}, :content nil}
;;            {:tag :pressure, :attrs {:unit "hPa", :value "1003.49"}, :content nil}
;;            {:tag :humidity, :attrs {:value "58", :unit "%"}, :content nil}
;;            {:tag :clouds, :attrs {:value "broken clouds", :all "64", :unit "%"}, :content nil}]}


(defn fetch-xml-element [x tag]
  (first (filter #(= (:tag %) tag) (:content x))))

(defn K->C [deg]
  (let [val (Math/round (- (Double/parseDouble deg) 273.15))
        sign (if (> val 0) "+" (if (< val 0) "-" ""))]
  (format "%s%dC" sign val)))

(defn pronounce-forecast [item]
  (whisper "pronounce-forecast got item: " item)
  (if-let [timestamp (:from (:attrs item))]
    (let [temperature (fetch-xml-element item :temperature)
          symbol (fetch-xml-element item :symbol)]
      (format "%s, %s" (K->C (:value (:attrs temperature))) (:name (:attrs symbol))))
    "not found"))

(defn timestamp->datetime [s]
  (ftime/parse (ftime/formatters :date-hour-minute-second) s))

(defn lookup-tomorrow [items]
  (let [now (timestamp->datetime (:from (:attrs (first items))))
        t (ctime/plus now (ctime/days 1))
        tomorrow-day (ctime/date-time (ctime/year t) (ctime/month t) (ctime/day t) 14 59)]
    (loop [items items]
      (when-let [next-item (first items)]
        (if (ctime/after? (timestamp->datetime (:from (:attrs next-item))) tomorrow-day)
          next-item
          (recur (rest items)))))))

(defn- parse-location [resp]
  (->> (:content (fetch-xml-element resp :location))
       (filter #(or (= (:tag %) :name)
                    (= (:tag %) :country)))
       (map :content)
       flatten
       (interpose ", ")
       (apply str)))

(defn parse-owm-xml [location url]
  "Parse openweathermap API response"
  (try
    (whisper "Calling openweathermap")
    (let [x (parse url)
          forecast (fetch-xml-element x :forecast)
          forecasts (vec (filter #(= (:tag %) :time) (:content forecast)))]
      (format "Current weather for %s: %s, forecast for tomorrow: %s."
              (parse-location x)
              (pronounce-forecast (first forecasts))
              (pronounce-forecast (lookup-tomorrow forecasts))))
    (catch java.io.IOException e
      (whisper "Cannot load weather:" e))))
  
(defn retrieve-user-server [name]
  (println "WHO" name)
  (loop [server-list '()]
    (let [line (read-line)
          [_ _ code _ rest-of-line] (re-matches #"(:[^ ]+) ([0-9]+) ([^ ]+) (.*)" line)]
      (case code
        ("315" "311") (if (= 1 (count server-list))
                        (first server-list)
                        nil)
        ("352" "318") (let [[_ _ server] (re-matches #"\* ([^ ]+) ([^ ]+).*" rest-of-line)]
                        (if (empty? server)
                          nil
                          (recur (conj server-list server))))
        nil))))

(defn expired? [timestamp expiration-seconds]
  (whisper "expired? " timestamp " should be newer than " expiration-seconds)
  (or (nil? timestamp)
      (> (/ (- (.getTime (now)) (.getTime timestamp)) 1000) expiration-seconds)))

(defn expired-session? [{time :logged-at}]
  (whisper "expired-session? logged-at: " time)
  (expired? time (:session-expiration-time bot-config)))

(defn find-real-user-by-AKA [aka]
  (whisper "find-real-user-by-AKA " aka)
  (first (filter #(= (:AKA (second %)) aka) @user-db)))

(defn lookup-user-entry [name]
  (if (empty? name)
    nil
    (let [canonic-name (lower-case name)]
      (whisper "lookup-user-entry. Canonic: " canonic-name ", by aka: " (find-real-user-by-AKA canonic-name)
               ", user-db entry: " (@user-db canonic-name))
      (or (second (find-real-user-by-AKA canonic-name))
          (@user-db canonic-name)))))

(defn display-weather [location whom name orig-location]
  (whisper "Displaying weather of " location)
  (if (nil? location)
    (shout whom name
           (str ", "
                (if (= (first orig-location) \@)
                  (str (subs orig-location 1) " lives")
                  "you live")
                " in the middle of nowhere"))
    (if-let [weather
             ;; (parse-wwo-xml 
             ;;  (str "http://api.worldweatheronline.com/free/v1/weather.ashx?q=" 
             ;;       (URLEncoder/encode location)
             ;;                     ; This is my personal key, please obtain your own
             ;;       "&includeLocation=yes&format=xml&num_of_days=2&key=jvvccgga6fwy2fszc8d5dj8a"))
             (parse-owm-xml location
                                        ; This is my personal key, please obtain your own
              (str "http://api.openweathermap.org/data/2.5/forecast?mode=xml&appid=3badf65afb944b0dd4a201fe232ce17c&q="
                   (URLEncoder/encode location)))
             ]
      (doall (map #(shout whom name ", " %) (split weather #"\n")))
      (shout whom name
             (str ", no weather have been found for " location)))))

;;http://api.worldweatheronline.com/free/v1/weather.ashx?q=London&format=xml&num_of_days=2&key=jvvccgga6fwy2fszc8d5dj8a

; This is my personal key, please obtain your own
;http://api.ipinfodb.com/v3/ip-city/?key=deaf0d1866bb62667d5440df1a148549cde1e949c556aace73cb260e00d71141&ip=

(defn known-user? [name]
  (not (expired-session? (lookup-user-entry name))))

(defn set-user-location [name location]
  (whisper "set-user-location: " name "@" location)
  (if (known-user? name)
    (let [entry (lookup-user-entry name)
          realname (:name entry)
          current-location (:city entry)]
      (if (empty? location)               ;Just report
        (shout name "You live in " (or current-location "nowhere"))
        (do (reset! user-db (assoc-in @user-db [realname :city] location))
            (shout name "You now live in " location))))
    (shout name "I don't know you, authenticate first")))

(defn user-location [user]
  (let [host (retrieve-user-server user)
                                        ;;; This is my personal key, please obtain your own!
        loc (http-client/get (format "http://api.ipinfodb.com/v3/ip-city/?key=deaf0d1866bb62667d5440df1a148549cde1e949c556aace73cb260e00d71141&ip=%s" host))
        [status _ _ _ country region city _] (split (:body loc) #";")]
    (if (and (= status "OK") city region country)
      (format "%s,%s" city country)
      nil)))

(defn report-weather [name server whom args]
  (whisper "report-weather: " name ", " server ", " whom ", " args ".")
  (let [l (stringify args)
        location (if (empty? l) nil l)
        [_ requester-hostname] (re-matches #"[^@]*@(.*)" server)
        location-by-user (fn [user]
                           (or (:city (lookup-user-entry user))
                               (user-location user)))]                           
    (whisper "Location supplied: '" location "'")
    (display-weather 
     (if (nil? location)
       (location-by-user name)
       (if (= (first location) \@)
         (location-by-user (subs location 1))
         location))
     whom name l)))

(defn authenticate-user [name password]
  (whisper "Authenticating " name)
  (= password (get-in @user-db [(lower-case name) :password])))

(defn remember-user 
  ([name password] 
     (remember-user name (or (first (find-real-user-by-AKA name)) name) password))
  ([name realname password]
     (whisper "real user to remember: " realname)
     (shout name
            (if (authenticate-user realname password)
              (do (reset! user-db 
                          (assoc @user-db realname
                                 (assoc (lookup-user-entry realname)
                                   :name realname :AKA name :logged-at (now) :last-seen (now))))
                  (str "Authenticated as " realname ", I'll remember you!"))
              "You do not exist, go away!"))))

(defn real-name [name]
  (or (first (find-real-user-by-AKA name)) name))

(defn admin? [name]
  (let [realname (real-name name)]
    (whisper "admin?: real name: " realname ", known? " (known-user? realname) ", admin: " (:admin (@user-db realname)))
    (and (known-user? realname)
         (:admin (@user-db realname)))))

(defn say-or-act-to-channel 
  ([name text]
     (if (empty? *channels*)
       (shout name "I didn't join any channel yet")
       (if (= (count *channels*) 1)
         (say-or-act-to-channel *recent-channel* name text)
         (let [parsed-text (str2/split text #"[ \t\n]+")
               chan-name (first parsed-text)]
           (if (contains? *channels* chan-name)
             (->> parsed-text
                  rest
                  (interpose " ")
                  (apply str)
                  (say-or-act-to-channel chan-name name))
             (shout name "Multiple channels joined, please specify the channel"))))))
  ([chan-name name text]
     (if (contains? *channels* chan-name)
       (shout chan-name text)
       (shout name "I didn't join " chan-name))))
       
(defn say-to-channel [name text]
  (say-or-act-to-channel name text))

(defn ctcpify [action text]
  (act-encode (str action " " text)))

(defn make-action [text]
  (ctcpify "ACTION" text))

(defn act-to-channel [name text]
  (say-or-act-to-channel name
                         (make-action text)))

(defn load-userdb [file]
  (when (.exists (as-file file))
    (->> file (deserialize)
         (reduce #(assoc %1 (first %2) (assoc (second %2) :AKA nil :name (first %2))) nil)
         (reset! user-db))
    (whisper "Loaded " (count @user-db) " user(s) from" file)))

(defn load-knowledge-base [file]
  (when (.exists (as-file file))
    (def ^:dynamic *knowledge-base* (deserialize file))))

(defn admin-only [name command & args]
  (if (admin? name)
    (do (whisper "Calling admin-only: " (pr-str command) " " name " " args)
        (apply command name args))
    (shout name "Says who?")))

(defn too-many-actions? [server]
  (let [entry (*user-commands* server)
        count (if (expired? (:last entry) (* 24 60 60))
                1
                (inc (:count entry)))]
    (whisper count " channel commands for " server)
    (if (> count (:daily bot-config))
      (do
        (def ^:dynamic *user-commands*
          (assoc *user-commands* server {:last (now), :count count}))
        true)
      false)))
    
(defn server-user [addr]
  (let [[_ user _] (re-matches #"~?(.*)@.*" addr)]
    (if (empty? user)
      (do (error "Cannot parse user name out of " addr)
          nil)
      user)))

(defn account-and-do [name server command whom & args]
  (if (too-many-actions? server)
    (shout name
           "You're over public command daily limit of " (:daily bot-config) " commands, sorry. "
           "I'd happily continue to chat with you in private!")
    (apply command whom args)))

(defn shutdown-bot [name]
  (shout name "Shutting down, bye!")
  (println "QUIT :Operator shutdown")
  (System/exit 0))

(defn list-channels [name]
  (shout name 
         (if (empty? *channels*)
           "No channels joined"
           (str "Channel list: " (apply str (interpose ", " (keys *channels*)))))))

; currency lookup

(defn fetch-currency-rates []
  ; This is my personal key, please obtain your own!
  (let [resp (http-client/get "https://openexchangerates.org/latest.json?app_id=3bf92ea593ce47a29b9745face8c1355") ]
    (whisper "response from openexchangerates:" resp)
    ((json/read-str (:body resp)) "rates")))

(def fetch-currency-rates-cached (memo-ttl fetch-currency-rates (:currency-expiration-time bot-config)))

(defn lookup-currency [rates from to]
  (try
    (let [from-rate (rates (str2/upper-case from))
          to-rate (rates (str2/upper-case to))]
      (whisper (format "lookup-currency got rates: %s for %s, %s for %s" from-rate from to-rate to))
      (/ to-rate from-rate))
    (catch Exception e
      (whisper "Rate(s) not found")
      nil)))
  
(defn do-convert-currency [amount f t]
  (whisper "do-convert-currency: " amount " " f " -> " t)
  (let [abbrevs {"$" "USD", "rur" "RUB", "RUR" "RUB", "r" "RUB", "р" "RUB", "руб" "RUB", "€" "EUR", "£" "GBP", "¥" "JPY"}
        from (or (abbrevs f) f)
        to (or (abbrevs t) t)]
    (let [currency-rates (fetch-currency-rates-cached)]
      ;;      (whisper "currency-rates:" currency-rates)
      (if-let [rate (lookup-currency currency-rates from to)]
        (format "%s %s = %.4f %s" amount from (* amount rate) to)
        (format "I don't know how to convert single %s to %s%s"
                from to
                (if (= amount 1.0)
                  ""
                  (str ", let alone " amount " of them")))))))

(defn convert-currency [ name args ]
  (whisper "convert-currency: " name ", " args)
  (try
    (shout name
           (case (count args)
             2 (let [[_ amount-specified from] (re-matches #"([0-9]+)(.*)" (first args))
                     amount (if (nil? amount-specified) 1.0 (Double. amount-specified))]
                 (do-convert-currency amount (first args) (second args)))
             3 (do-convert-currency (Double. (first args)) (second args) (nth args 2))
             "Use: ucc [amount] from to"))
    (catch NumberFormatException e
      (shout name "Usage: ucc [amount] from to"))))

(defn show-help [name]
  (shout name (str 
               "Known commands: auth, change-pass, grep, location, seen, ucc, urls, learn, forget, fact, whats, stat, time, units, where, wr, wtf."
               (if (admin? name) " Admin-only commands: stop, join, leave, say, act, add-user, kill-user, eval, channels." ""))))

(defn join-channel [name channel]
  (when channel
    (println (str "JOIN :" channel))
    (def ^:dynamic *channels* (assoc-in *channels* [channel :name] channel))
    (def ^:dynamic *recent-channel* channel)
    (shout name "I've joined " channel)))

(defn leave-channel [name channel]
  (if (empty? *channels*)
    (shout name "I didn't even joined yet!")
    (let [chan (if (empty? channel) *recent-channel* channel)]
      (println (str "PART :" chan))
      (def ^:dynamic *channels* 
        (dissoc *channels* chan))
      (def ^:dynamic *recent-channel* nil)
      (shout name "Elvis has left " chan))))
  
(defn add-user [name args]
  (let [new-user (first args)]
    (reset! user-db (assoc-in @user-db [(lower-case new-user) :password] (second args)))
    (shout name "New user '" new-user "' has been registered")))

(defn change-password [name pass]
  (if (known-user? name)
    (let [realname (real-name name)]
      (if (empty? pass)
        (shout name "Please give me some password!")
        (do (reset! user-db (assoc-in @user-db [realname :password] pass))
            (shout name "Password for " realname " has been changed"))))
    (shout name "Who are you?")))

(defn kill-user [name to-kill]
  (if (empty? to-kill)
    (shout name "Whom?")
    (let [username (lower-case to-kill)]
      (if (contains? @user-db username)
        (do (reset! user-db (dissoc @user-db username))
            (shout name "User " to-kill " has been removed"))
        (shout name "Who's that?")))))

(defn show-top-chatters [name howmany]
  (whisper "show-top-chatters " howmany)
  (doseq [entry (->> @user-db
                     (remove #(nil? (:sents (second %))))
                     (sort-by #(:sents (second %)) #(- %2 %))
                     (take howmany))]
    (whisper "chatter entry: " entry)
    (shout name (first entry) ": " (:sents (second entry)))))
     
(defn show-stat [name whom]
  (whisper "show-stat " name " " whom)
  (if (empty? whom)
    (shout name "Whose or how many?")
    (if (re-matches #"[0-9]+" whom)
      (show-top-chatters name (min (Integer. whom) 20))
      (if-let [entry (lookup-user-entry whom)]
        (do (whisper "show-stat for " entry)
            (if (nil? (:sents entry))
              (shout name whom " kept silent since I began listening")           
              (shout name "I've heard " (:sents entry) " sentences from " whom " since I began lurking here")))
        (shout name "I don't know " whom)))))

(defn show-last-seen [name whom]
  (if (empty? whom)
    (shout name "Who?")
    (if-let [entry (lookup-user-entry whom)]
      (if-let [seen (:last-seen entry)]
        (shout name "I've seen " whom
               (if (nil? (:AKA entry)) "" (str " aka " (:AKA entry) " "))
               " at " (.toString seen))
        (shout name "I've never met " whom))
      (shout name "I don't know " whom))))

(defn show-urls [name cnt]
  (if (empty? cnt)
    (shout name "How many?")
    (let [to-show (take (min (Integer. cnt) 100) @saved-urls)]
      (if (empty? to-show)
        (shout name "Nothing yet, come again later")
        (doseq [url to-show]
          (shout name (URLDecoder/decode url)))))))

(defn eval-data [name cmd]
  (try
    (whisper "(eval " cmd ")")
    (shout name cmd " yields "
           (binding [*ns* (the-ns 'ircbot.core)]
             (eval (read-string cmd))))
    (catch Exception e
      (shout name "Exception caught: " e))))

(defn learn-definition [name term text_]
  (whisper "learn-definition: " term " is " text_)
  (if (empty? term)
    (shout name "What?")
    (if (empty? text_)
      (shout name "Please elaborate on what " term " actually is.")
      (let [text (trim-is (trim text_))]
        (if (nil? (remember-term-definition term text))
          (shout name "I'll rather wont for with much wisdom comes much sorrow.")
          (shout name "I'll remember that " term " is " text))))))

(defn show-definition [name term_]
  (if (empty? term_)
    (shout name "Out of " (count *knowledge-base*) " things I know, what do you want?")
    (let [term (trim term_)
          [_ word] (re-matches #"(.*)\?*" term)
          to-search (or word term)
          defs (*knowledge-base* to-search)]
      (if (empty? defs)
        (shout name "No idea what " term " is.")
        (doseq [def (reverse defs)]
          (shout name to-search ": " def))))))

(defn forget-definition [name term_ text_]
  (if (empty? term_)
    (shout name "What?")
    (let [term (trim term_)]
      (if (empty? text_)
        (shout name "Please elaborate on what to forget about " term ".")
        (let [text (trim text_)
              known-term (*knowledge-base* term)
              new-def (remove #(= text %) known-term)]
          (if (nil? known-term)
            (shout name "No idea what " term " is.")
            (do (update-knowledge-base term new-def)
                (shout name "Forgot that " term " was " text))))))))

(defn search-for-fact [name fact_]
  (if (empty? fact_)
    (shout name "What about?")
    (let [fact (lower-case (trim fact_))
          found (concat 
                 (remove #(empty? (second %))
                         (map
                          (fn [e] (list (first e) (filter #(str2/contains? (lower-case %) fact) (second e))))
                          *knowledge-base*))
                 (map #(list (first %) (second %)) ;; Ugly but OK
                      (filter #(str2/contains? (lower-case (first %)) fact) *knowledge-base*)))]
      (whisper "found: " found ", distinct: " (distinct found))
      (if (empty? found)
        (shout name "I don't know anything about " fact ", sorry...")
        (doseq [entry (distinct found)]
          (doseq [definition (distinct (second entry))]
            (shout name (first entry) " is " definition)))))))

(defn ctcp-version [name]         
  (whisper "Replying with VERSION")
  (notice name 
          (ctcpify "VERSION" (str (:who-am-i bot-config)
                                  (System/getProperty "os.name") "/" (System/getProperty "os.arch") 
                                  "/" "JRE " (System/getProperty "java.runtime.version")))))

(defn ctcp-time [name]         
  (notice name (ctcpify "TIME" (now))))

(def ctcp-actions
  { "VERSION" ctcp-version, 
    "TIME" ctcp-time })

(defn process-ctcp [name message]
  (whisper "process-ctcp (probably): '" message "' from " name)
  (if-let [action (first 
                   (filter 
                    #(begins-with? message (act-encode %)) 
                    (keys ctcp-actions)))]
    (do (whisper "Calling CTCP handler for " action)
        ((ctcp-actions action) name)
        true)
    false))

(defn where-is [name user_]
  (if (empty? user_)
    (shout name "Who?")
    (let [user (real-name (trim user_))]
      (if-let [city (get-in @user-db [user :city])]
        (shout name user " lives in " city)
        (if-let [location (user-location user)]
          (do (shout name "I've learned that " user " lives in " location)
              (reset! user-db (assoc-in @user-db [user :city] location)))
          (shout name user " lives in the middle of nowhere"))))))

(defn convert-units [name & args ]
  (let [from (ffirst args)
        to (second (first args))]
    (whisper "convert-units: " from to)
    (if (empty? from)
      (shout name "What from?")
      ;;{:exit 0, :out "37.777778\n", :err ""}
      (let [resp (:out 
                  (apply clojure.java.shell/sh 
                         (flatten (list "units" "-t" from (if (empty? to) () to)))))]
        (shout name (trim resp))))))

(defn channel-log-file [channel]
  (str (:log-file-prefix bot-config) channel ".log"))

(defn censor-grep-output [line]
  (or
   (re-matches (re-pattern #".*PRIVMSG .*") line)
   (re-matches #".* <.*!.*@.*> \.(.*)" line) ; leading dot: public bot command, most probably
   (re-matches #".*(pidar|пидар).*" line) ; #fidorus specific :)
   (re-matches (re-pattern (str "<.*!.*@.*> " *current-nick* ".*")) line))) ; my own actions

                                        ; 2020-01-25 22:19 <damned!~damned@213.141.138.199> qwe
(defn compactify-userid [line]
  (if-let [[_ timestamp userid rest] (re-matches #"(.*) <(.*)!.*> (.*)" line)]
    (str timestamp " <" userid "> " rest)
    line))

(defn grep-log [name text_]
  (if (empty? text_)
    (shout name "What'd you like to find?")
    (let [text (trim text_)
          parsed-text (str2/split (trim text_) #"[ \t\n]+")
          maybe-channel (first parsed-text)
          [channel to-grep] (if (*channels* maybe-channel)
                              [maybe-channel (apply str (interpose " " (rest parsed-text)))]
                              [*recent-channel* text])]
      (whisper "grep-log: channel: " channel ", to-grep: " to-grep)
      (if channel
        (let [log-file (channel-log-file channel)]
          (let [lines (->> (:out (clojure.java.shell/sh "grep" "-i" "-e" to-grep log-file))
                           (#(split % #"\n"))
                           (remove censor-grep-output)
			   (reverse)
                           (take (:max-grep-lines bot-config)))]
            (if (empty? lines)
              (shout name "No match for '" to-grep "'")
              (doseq [l lines]
                (shout name (compactify-userid l))))))
        (shout name "No such channel (did I join any channel yet?)")))))

(defn do-action [name server action]
  (let [[_ command arg-string] (re-matches #"([^ ]*) ?(.*)" action)
        args (seq (split (str2/trim (eat-spaces arg-string)) #"\s"))]
    (whisper "do-action got name " name ", action " action ", command " command ", args " args)
    (case (triml-regex "\\." command)
      "grep" (grep-log name arg-string)
      "wr" (report-weather name server name args)
      "ucc" (convert-currency name args)
      "auth" (apply remember-user name args)
      "stop" (admin-only name shutdown-bot)
      "location" (set-user-location name arg-string)
      "join" (admin-only name join-channel (first args))
      "leave" (admin-only name leave-channel (first args))
      "say" (admin-only name say-to-channel arg-string)
      "act" (admin-only name act-to-channel arg-string)
      "add-user" (admin-only name add-user args)
      "kill-user" (admin-only name kill-user (first args))
      "change-pass" (change-password name (first args))
      "urls" (show-urls name (first args))
      "seen" (show-last-seen name (first args))
      "eval" (admin-only name eval-data arg-string)
      "learn" (learn-definition name (first args) (stringify (rest args)))
      "whats" (show-definition name (first args))
      "forget" (forget-definition name (first args) (stringify (rest args)))
      "stat" (show-stat name (first args))
      "fact" (search-for-fact name arg-string)
      "where" (where-is name arg-string)
      "wtf" (ask-google name arg-string)
      "units" (convert-units name args)
      "time" (request-time name (first args))
      "channels" (admin-only name list-channels)
      (show-help name))))

(defn privmsg-reply [_ name server message]
  (whisper "privmsg-reply:" name ", " server ", " message)
  (when-not (process-ctcp name message)
    (do-action name server message)))

(defn do-chan-action [channel name server action]
  (let [[_ command arg-string] (re-matches #"([^ ]*) ?(.*)" action)
        args (seq (split (str2/trim (eat-spaces arg-string)) #"\s"))
        invoker (partial account-and-do name server)]
    (whisper "do-chan-action, command: " command ", args: " args ", name: " name)
    (case command
      "grep" (invoker grep-log channel arg-string)
      "wr" (invoker  report-weather name server channel args)
      "seen" (invoker  show-last-seen channel (first args))
      "urls" (invoker  show-urls name (first args))
      "help" (invoker  show-help name)
      "ucc" (invoker  convert-currency channel args)
      "learn" (invoker  learn-definition channel (first args) (stringify (rest args)))
      "whats" (invoker  show-definition channel (first args))
      "fact" (invoker  search-for-fact channel arg-string)
      "forget" (invoker  forget-definition channel (first args) (stringify (rest args)))
      "stat" (invoker  show-stat channel (first args))
      "where" (invoker  where-is channel arg-string)
      "units" (invoker  convert-units channel args)
      "wtf" (invoker  ask-google channel arg-string)
      "time" (invoker request-time channel (first args))
      nil)))

(defn chat-statistics [name text]
  (let [new-sentences (count (remove #(empty? %) (clojure.string/split text #"[\.:\?!]")))
        user (real-name name)
        entry (lookup-user-entry user)]
    (whisper "Adding " new-sentences " sents for " user)
    (reset! user-db (update-in @user-db [user :sents] #(+ (or % 0) new-sentences)))))

(defn channel-log [channel line]
  (with-open [w (writer (channel-log-file channel) :append true)]
    (.write w (format "%1$tY-%1$tm-%1$te %1$tH:%1$tM %2$s\n" (now) line))))

(defn channel-reply [channel name server message]
  (whisper "channel-reply:" name ", " server ", " message)
  (channel-log channel (str "<" name "!" server "> " message))
  (chat-statistics name message)
  (when-let [action
             (if (= (first message) \.)
               (subs message 1)
               (if (begins-with? message *current-nick*)
                 (triml-regex "[-,: ]*" (subs message (count *current-nick*)))
                 nil))]
    (do-chan-action channel name server action)))

(defn grab-URLs [line]
  (doseq [new-url (map #(first %) (re-seq #"(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]" line))
          :when (not (in? new-url @saved-urls))]
    (whisper "New URL found: " new-url)
    (reset! saved-urls (conj @saved-urls new-url))))

(defn update-last-seen [name]
  (reset! user-db (assoc-in @user-db [name :last-seen] (now))))

(def got-pong (atom true))
(def start-status (atom "Startup"))

(defn process-command [line]
  (whisper "< " line)
  (if (= (first line) \:)
    (let [[_ actor command addressee args] (re-matches (re-pattern (str ":([^ ]+) ([^ ]+) ([^ ]+)(.*)?")) line)
          [_ name server] (re-matches #"(.*)!(.*)" actor)]
      (whisper "Actor: " actor ", (name " name ", server " server
               ", command: " command ", addressee: " addressee ", args:" args)
      (when-not (nil? name)
        (let [name (lower-case name)]
          (update-last-seen name)
          (case command
            "PRIVMSG"  (let [message (triml-whitespace (subs (triml-whitespace args) 1))]
                         (grab-URLs message)
                         ((if (= addressee *current-nick*)
                            privmsg-reply
                            channel-reply) addressee name server message))
;;;         "JOIN" (handle-user-join name)
;;;         "PART" (handle-user-leave name)
            "NOTICE" (if (= addressee *current-nick*)
                       (handle-notice name args))
            nil)))
      (case command
        "PONG" (do
                 (reset! got-pong true)
                 (whisper "PONG <"))
        nil))
    (let [[_ command args] (re-matches #"([^ ]+) :(.*)" line)]
      (whisper "Command: " command ", args: " args)
      (case command
        "PING" (do 
                 (println "PONG :" args)
                 (whisper "Ponged " args))
        nil))))


(def health-checker (atom nil))
(def reconnect? (atom false))

(defn start-health-checker []
  (reset! health-checker
          (future
            (Thread/sleep 10000)
            (while true
              (whisper "got-pong is " @got-pong)
              (when
                  (and
                   (= @got-pong false)
                   (= @start-status "Working"))
                (whisper "Network failure detected!")
                (reset! got-pong true)
                (reset! reconnect? true))
              (println "PING" (trim (:server bot-config)))
              (whisper "PING >")
              (reset! got-pong false)
              (Thread/sleep 5000)))))

(defn maintain []
  "Main loop"
  (start-health-checker)
  (loop [line (read-line)]
    (when @reconnect?
      (whisper "Breaking from maintain: network failure")
      (reset! reconnect? false)
      (future-cancel @health-checker)
      (throw (Exception. "Ping timeout")))
    (when (not (nil? line))
      (process-command line)
      (recur (read-line)))))
 
(defn startup-hook [ message ]
  (whisper "Running startup hook")
  (join-channel "Tobotras" (:to-join bot-config))
  (when message
    (shout "Tobotras" message)))

;;----------------------------------------------------------------------

(defn initialize-bot []
  (whisper "Loading config")
  (load-bot-config "ircbot.xml")
  (whisper "Loading user DB")
  (load-userdb (:user-db-file bot-config))
  (whisper "Loading knowledge DB")
  (load-knowledge-base (:kb-file bot-config))
  (whisper "Loading saved URLs")
  (load-saved-urls (:saved-urls-file bot-config))
  (System/setProperty "javax.net.ssl.trustStore" "jssecacerts")
  (def *current-nick* (:nick bot-config))
  (reset! start-status "Startup"))
 
(defn -main []
  (initialize-bot)
  (while true
    (whisper "Connecting to " (:server bot-config))
    (try
      (if-let [conn (socket (trim (:server bot-config)) (:port bot-config))]
        (binding [*in* (:in conn)
                  *out* (:out conn)]
          (whisper "Logging to server")
          (login)
          (whisper "We're in the network!")
          (startup-hook @start-status)
          (whisper "Working")
          (reset! start-status "Working")
          (maintain))
        (error (str "Cannot connect to " (:server bot-config) ":" (:port bot-config))))
      (catch java.net.ConnectException e
        (whisper "ConnectException: " e)
        (reset! start-status e))
      (catch java.lang.Exception e
        (whisper "Base exception: " e)
        (reset! start-status e)))
    (error "Restarting connection")
    (reset! start-status "Startup")
    (Thread/sleep 30000)))
