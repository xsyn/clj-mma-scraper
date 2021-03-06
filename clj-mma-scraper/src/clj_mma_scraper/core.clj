(ns clj-mma-scraper.core
  (:require [clj-http.client :as client]
            [clj-time.coerce :as c]
            [clj-time.format :as tf]
            [clojure.set :as st]
            [clojure.string :as str]
            [datomic.api :as d]
            [net.cgrand.enlive-html :as html]
            [slingshot.slingshot :refer :all]
            [taoensso.timbre :as timbre :refer [error warn]]))

;; Some initial definitions
(def starting-url "http://www.fightmetric.com/statistics/events/completed?page=all")

(def uri "datomic:free://localhost:4334/mma-stats")

;; Create database

;; NB: Initializedb on the my dev version of Datomic is broken due to:https://groups.google.com/forum/#!topic/datomic/1WBgM84nKmc
(defn initialize-db [uri]
  "Drops the database if it exists. Creats a new DB with schema"
  (if (false? (d/create-database uri))(do (d/delete-database uri)
                                          (initialize-db uri))
      (let [schema-tx (read-string (slurp "resources/schema.edn"))]
        (try
          (d/create-database uri)
          (d/transact (d/connect uri) schema-tx)
          (catch Throwable t nil)))))

(defn conn [] (d/connect uri))

(defn db [] (d/db conn))

(def schema-tx (read-string (slurp "resources/schema.edn")))

(defn init-db [uri]
  (d/transact (d/connect uri) schema-tx))

;; Setup scraper

;; TODO: Clean up try+ catch, use try catch instead

(defn get-url [url]
  (try+
   (client/get url {:insecure true})
   (catch [:status 403] {:keys [request-time headers body]}
     (do
       (println (warn "403" request-time headers))
       nil))
   (catch [:status 404] {:keys [request-time headers body]}
     (do
       (println (warn "NOT Found 404" request-time headers body))
       nil))
   (catch Object _
     (error (:throwable &throw-context) "unexpected error")
     (throw+))))

(defn fetch-body* [url]
  (let [check-url (get-url url)]
    (if (nil? check-url) nil
        (html/html-resource (java.io.StringReader.
                             (:body check-url))))))

(def fetch-body (memoize fetch-body*))

;; Standard Utility functions

(def formatter (tf/formatter "MMMM dd, YYYY"))

(defn format-date [datestring]
  "Turns 'November 29, 2015' into #inst date format for datomic"
  (let [cleandate (if (= datestring "--") "January 01, 1900" datestring)]
    (c/to-date (tf/parse formatter cleandate))))

;; HTML Utility functions

(defn html-select-first [row tree]
  (first (html/select row tree)))

(defn get-href [element]
  (get-in element [:attrs :href]))

(defn get-data-link [element]
  (get-in element [:attrs :data-link]))

(defn get-content [element path]
  (let [content (first (:content (html-select-first element path)))]
    (when content
      (str/trim content))))

(defn clean-weight-division [weight-string]
  (str/trim (str/replace weight-string #"Bout" "")))

(defn extract-type-from-name [name]
  (cond (.contains name "on Fox") :event.type/fox
        (.contains name "Ultimate Fighter") :event.type/ultimatefighter
        (.contains name "Fight Night") :event.type/fightnight
        :else :event.type/ufc))

(def weight-str->weight-type
  {"Women's Strawweight"   :fight.weightclass/womansstrawweight
   "Flyweight"             :fight.weightclass/flyweight
   "Bantamweight"          :fight.weightclass/bantamweight
   "Women's Bantamweight"  :fight.weightclass/womensbantamweight
   "Featherweight"         :fight.weightclass/featherweight
   "Lightweight"           :fight.weightclass/lightweight
   "Welterweight"          :fight.weightclass/welterweight
   "Middleweight"          :fight.weightclass/middleweight
   "Light Heavyweight"     :fight.weightclass/lightheavyweight
   })

(defn extract-type-from-weight [weight]
  (or (get weight-str->weight-type weight)
      :fight.weightclass/heavyweight))

(defn extract-stance-from-string [stance]
  (cond (= stance "Southpaw") :fighter.stance/southpaw
        :else :fighter.stance/orthodox))

(defn get-text-first [element path]
  (str/trim (html/text (first (html/select element path)))))

(defn clean-detail-string [s]
  (str/trim (second (str/split s #":"))))

(defn page-container [url]
  (html/select (fetch-body url) [:div.l-page__container]))

(defn html-get-name [html]
  (get-text-first html [:h2 :span]))

(defn clean-box [html path]
  (map clean-detail-string
       (map html/text (html/select html path))))

(defn add-tx-id [m]
  (assoc m :db/id (d/tempid :db.part/user)))

(defn get-number [s]
  (let [number (re-find #"\d+" s)]
    (if (not-empty number)
      (double (Integer/parseInt number))
      (double 0))))

(defn inches->cm [x]
  (when x
    (let [size (if (string? x)
                 (get-number x)
                 (double x))
          inches 2.54]
      (* inches size))))

(defn feet->cm [s]
  (if (= s "--") 0.00
      (when s (let [size (map get-number (str/split s #"'"))
                    feet 30.48]
                (+ (* (first size) feet)
                   (inches->cm (second size)))))))

(defn pounds->kg [s]
  (when s (let [weight (get-number s)
                pounds 0.453592]
            (* weight pounds))))


;; Datomic query functions

(defn get-all-event-urls[]
  "Get all the urls that are in the db"
  (d/q '[:find [?url ...]
         :in $
         :where [_ :event/url ?url]] (d/db (conn))))

(defn lookup-fighter-by-url [url]
  (d/q '[:find ?e
         :in $ ?url
         :where [?e :fighter/url ?url]] (d/db (conn)) url))

;; Scraping stuff

(defn get-event-details [event-url]
  "Takes in a url that maps to a FightMetric event, and creates a map of it's data."
  (when event-url
    (let [b (page-container event-url)
          event-name (html-get-name b)
          ;; use positional destructuring to pull out values
          ;; from 0, 1, 2 indexes of vector
          [event-date
           event-location
           event-attendance] (clean-box b [:div.b-fight-details :div :ul :li])]
      {:event/name event-name
       :event/url event-url
       :event/date (format-date event-date)
       :event/location event-location
       :event/type (extract-type-from-name event-name)
       :event/attendance (get-number (str/replace event-attendance #"," ""))
       })))

(defn get-fighter-details [fighter-url]
  "Returns a map after scraping fighters page"
  (when fighter-url
    (let [b (page-container fighter-url)]
      (when b
        (let [name (html-get-name b)
              nickname (get-content b [:p.b-content__Nickname]) ;; Check to see if empty
              [height
               weight
               reach
               stance
               dob] (clean-box b [:div.b-fight-details.b-fight-details_margin-top :div.b-list__info-box.b-list__info-box_style_small-width.js-guide :ul :li])]
          {:fighter/name name
           :fighter/url fighter-url
           :fighter/nickname nickname
           :fighter/height (feet->cm height)
           :fighter/weight (pounds->kg weight)
           :fighter/reach (inches->cm reach)
           :fighter/stance (extract-stance-from-string stance)
           :fighter/dob (format-date dob)
           })))))

(defn get-fight-items [body]
  "Passing in an html body, returns a map of fight-items, including round, time, time format, referee and details"
  (let [b (html/select body [:i.b-fight-details__text-item])]
    (vec (map (fn [x] (str/trim (nth (:content x) 2))) b))))

(comment (defn get-stat-items [body]
           (let [b] (html/select body [:section.b-fight-details__section.js-fight-section :table :tbody :tr]))))

(defn map-fighters [body]
  "Gets information about the fighter in a fight"
  (map (fn [row]
         (let [outcome (str/trim (first (:content (first (html/select row [:div :i])))))
               name (first (:content (first (html/select row [:div :div :h3 :a]))))
               fighter-url (get-in (first (html/select row [:div :div :h3 :a])) [:attrs :href])]
           [outcome name fighter-url])) body))

;; TODO: Get fighters and stats
(defn get-fight [url]
  "Get's fight details from a url"
  (when url
    (let [body (html-select-first (fetch-body url) [:div.b-fight-details])
          person-body (html/select body [:div.b-fight-details__person])
          details-body (html-select-first body [:div.b-fight-details__fight])
          weightclass (clean-weight-division (get-content body [:div.b-fight-details__fight-head :i]))
          fighters (map-fighters person-body) ;; - want to return
          ;; fighter datoms
                                        ;stats (map-stats fighters)
          [rounds
           time
           time-format
           referee] (get-fight-items details-body)
          method (str/trim (first (:content (second (html/select details-body [:i.b-fight-details__text-item_first :i])))))]
      {:fight/weightclass weightclass
       :fight/rounds rounds
       :fight/method method
       })))

(defn clean-fighter-row [row]
  "Traverse the row to the correct attributes, flatten, pull out the links and remove nil"
  (remove nil?
          (map get-href
               (flatten
                (map :content (:content row))))))

(defn get-fighters-url-from-card-body [body]
  "This is a shockingly ugly piece of code that traverses trees of data to get to specifc end points "
  (when body
    (map clean-fighter-row
         (html/select body [:tbody :tr [:td (html/nth-of-type 2)]]))))

(defn scrape-fighters-from-card [url]
  (when url
    (let [b (fetch-body url)]
      (get-fighters-url-from-card-body b))))

(defn scrape-fights-from-card [url]
  (when url
    (let [b (fetch-body url)]
      (map get-data-link (html/select b [:tbody :tr])))))

(defn get-all-events [url]
  "Returns a collection of event-urls, which are of all the UFC events ever."
  (let [body (fetch-body* url)]
    (when body
      (let [b (rest (html/select body [:table.b-statistics__table-events :tbody :tr.b-statistics__table-row]))]
        (map (fn [row] (get-href
                       (html-select-first row [:tr :> [:td (html/nth-of-type 1)] :i :a])))
             b)))))


;; Transactional functions

(defn def-tx-class [tx-class]
  (cond (= tx-class "event") get-event-details
        (= tx-class "fight") get-fight
        (= tx-class "fighter") get-fighter-details))

(defn insert-tx [urls tx-class]
  (do (timbre/info (str/join "Inserting class: " tx-class))
      (->> (map (def-tx-class tx-class) urls)
           (remove nil?)
           (map add-tx-id)
           (d/transact (conn))
           deref
           :db-after)))

(defn db-current? []
  "Checks the difference between the events on system, and the events on FightMetric, inserts the difference."
  (let [diff (st/difference
              (set (get-all-events starting-url))
              (set (get-all-event-urls)))]
    (if (not-empty diff)
      (->>
       (insert-tx diff "event")
       (d/q '[:find [?url ...] :where [?e :event/url ?url]]))
      (get-all-event-urls))))

(defn update-fighters [events]
  (insert-tx
   (flatten (map scrape-fighters-from-card events))
   "fighter"))

(defn update-fights [events]
  (insert-tx (map scrape-fights-from-card events)
             "fight"))

(defn insert-data []
  (->>
   ;; Check that all events are up to date
   (db-current?)
   ;; Ensure we have update to date fight list
   (update-fighters)
   ;; Get all event urls from database
   (get-all-event-urls)
   (comment (update-fights)
            (map (fn [x] map insert-fighters-and-fights x)))))

;; 1. Initialize database
;;    - Bug in Datomic. TODO: Check transactors are up to date.
;; 2. Check for new events
;; 3. If there are new events insert them into db
;;    - Get events url - DONE
;;    - Get events details - DONE
;;    - Insert events - DONE
;; 4. Validate fights on events
;; 5. Validate fighters on fights

;; (get-all-events)
;; (is there a difference between events in the db and events in the
;; list?, if so insert new ones, and return all-event-urls from db)
;; ;; One of the things that is missing is the Fighter Record, but I believe this is
;; a derived value
