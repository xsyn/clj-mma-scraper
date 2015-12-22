(ns clj-mma-scraper.core
  (:require [net.cgrand.enlive-html :as html]
            [datomic.api :as d]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clj-time.coerce :as c]
            [clojure.string :as str]
            [clojure.set :as st]
            [clojure.java.io :as io]
            [clj-http.client :as client]
            [clj-http.conn-mgr :as conn-mgr]
            [taoensso.timbre :as timbre
             :refer (log  trace  debug  info  warn  error  fatal  report
                          logf tracef debugf infof warnf errorf fatalf reportf
                          spy get-env log-env)]
            [taoensso.timbre.profiling :as profiling
             :refer (pspy pspy* profile defnp p p*)])
  (:use [slingshot.slingshot :only [throw+ try+]])
  (:import [java.net URI URL]))


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

(defn fetch-body [url]
  (let [check-url (get-url url)]
    (if (nil? check-url) nil
        (html/html-resource (java.io.StringReader.
                             (:body check-url))))))

;; Standard Utility functions

(defn format-date [datestring]
  "Turns 'November 29, 2015' into #inst date format for datomic"
  (c/to-date (tf/parse (tf/formatter "MMMM dd, YYYY") datestring)))

;; HTML Utility functions

(defn html-select-first [row tree]
  (first (html/select row tree)))

(defn get-href [element]
  (get-in element [:attrs :href]))

(defn get-data-link [element]
  (get-in element [:attrs :data-link]))

(defn get-content [element path]
  (str/trim (first (:content (html-select-first element path)))))

(defn clean-weight-division [weight-string]
  (str/trim (str/replace weight-string #"Bout" "")))

(defn extract-type-from-name [name]
  (cond (.contains name "on Fox") :event.type/fox
        (.contains name "Ultimate Fighter") :event.type/ultimatefighter
        (.contains name "Fight Night") :event.type/fightnight
        :else :event.type/ufc))

(defn extract-type-from-weight [weight]
  (cond (= weight "Women's Strawweight") :fight.weightclass/womansstrawweight
        (= weight "Flyweight") :fight.weightclass/flyweight
        (= weight "Bantamweight") :fight.weightclass/bantamweight
        (= weight "Women's Bantamweight") :fight.weightclass/womensbantamweight
        (= weight "Featherweight") :fight.weightclass/featherweight
        (= weight "Lightweight") :fight.weightclass/lightweight
        (= weight "Welterweight") :fight.weightclass/welterweight
        (= weight "Middleweight") :fight.weightclass/middleweight
        (= weight "Light Heavyweight") :fight.weightclass/lightheavyweight
        :else :fight.weightclass/heavyweight))

(defn get-text-first [element path]
  (str/trim (html/text (first (html/select element path)))))

(defn clean-detail-string [s]
  (str/trim (second (str/split s #":"))))

(defn parse-int [s]
  (Integer. (re-find  #"\d+" s )))

(defn page-container [url]
  (html/select (fetch-body url) [:div.l-page__container]))

(defn html-get-name [html]
  (get-text-first html [:h2 :span]))

(defn clean-box [html path]
  (map clean-detail-string
       (map html/text (html/select html path))))

;; CRUD functions

(defn get-all-event-urls[]
  "Get all the urls that are in the db"
  (d/q '[:find [?url ...]
         :in $
         :where [_ :event/url ?url]] (d/db (conn))))

;; Scraping stuff

(defn make-event-tx [m]
  (assoc m :db/id (d/tempid :db.part/user)))

(defn get-number [s]
  (Integer/parseInt
   (re-find #"\d+" s)))

(defn feet->cm [s]
  (let [size (map get-number (str/split s #"'"))
        inches 2.54
        feet 30.48]
    (+ (* (first size) feet)
       (* (second size) inches))))

;; TODO: Get attendance string->int working
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
                                        ;:event/attendance (parse-int (str/replace event-attendance
                                        ;#"," ""))
       })))

;; One of the things that is missing is Record, but I believe this is
;; a derived value

(defn get-fighter-details [fighter-url]
  "Returns a map after scraping fighters page"
  (when fighter-url
    (let [b (page-container fighter-url)
          name (html-get-name b)
          nickname (get-content b [:p :content]) ;; Check to see if empty
          [height
           weight
           reach
           stance
           dob] (clean-box b [:div.b-fight-details.b-fight-details_margin-top :div.b-list__info-box.b-list__info-box_style_small-width.js-guide :ul :li])]
      {:fighter/name name
       :fighter/url fighter-url
       :fighter/nickname nickname
       :fighter/height (feet->cm )})))


(defn get-fight-items [body]
  "Passing in an html body, returns a map of fight-items, including round, time, time format, referee and details"
  (let [b (html/select body [:i.b-fight-details__text-item])]
    (vec (map (fn [x] (str/trim (nth (:content x) 2))) b))))

(defn get-stat-items [body]
  (let [b] (html/select body [:section.b-fight-details__section.js-fight-section :table :tbody :tr])))

(defn get-fighter [url]
  "Gets a fighters details from a url"
  )

(defn map-fighters [body]
  "Gets information about the fighter in a fight"
  (map (fn [row]
         (let [outcome (str/trim (first (:content (first (html/select row [:div :i])))))
               name (first (:content (first (html/select row [:div :div :h3 :a]))))
               fighter-url (get-in (first (html/select row [:div :div :h3 :a])) [:attrs :href])]
           (zipmap [:outcome :name :url] [outcome name fighter-url]))) body))

(defn map-stats [fighters fight-url]
  "Take a seq of fighters and looks for their stats, on a particular fight page."
  (map (fn [fighter] ) fighters))

;; TODO: Get fighters and stats
(defn get-fight [url]
  "Get's fight details from a url"
  (when url
    (let [body (html-select-first (fetch-body url) [:div.b-fight-details])
          person-body (html/select body [:div.b-fight-details__person])
          details-body (html-select-first body [:div.b-fight-details__fight])
          weightclass (clean-weight-division (get-content body [:div.b-fight-details__fight-head :i]))
          fighters (map-fighters person-body)
          stats (map-stats fighters)
          [rounds
           time
           time-format
           referee] (get-fight-items details-body)
          method (str/trim (first (:content (second (html/select details-body [:i.b-fight-details__text-item_first :i])))))])))

;; NOTE: Need to handle edge-cases for draw

;; Build a map of all urls that we'll need to populate

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

(defn scrape-links-from-card [url]
  "Returns a map of links, to the actual fights from a card, attached to fights. This could be two function, but I want to make a single call to the url."
  (when url
    (let [body (fetch-body url)]
      (map #(assoc % :event url)
           (map #(-> (into {} %))
                (partition 2
                           (interleave
                            (map #(hash-map :fight %)
                                 (map get-data-link (html/select body [:tbody :tr])))
                            (map #(hash-map :fighters %)
                                 (get-fighters-url-from-card-body body)))))))))

(defn get-all-events [url]
  "Returns a collection of event-urls, which are of all the UFC events ever."
  (let [body (fetch-body url)]
    (when body
      (let [b (rest (html/select body [:table.b-statistics__table-events :tbody :tr.b-statistics__table-row]))]
        (map (fn [row] (get-href
                       (html-select-first row [:tr :> [:td (html/nth-of-type 1)] :i :a])))
             b)))))


;; Note: Abstract these functions together
(defn get-and-insert-events [urls]
  (->>
   (map get-event-details urls)
   (remove nil?)
   (map make-event-tx)
   (d/transact (conn))
   deref
   :db-after))

(defn get-and-insert-fighters [urls]
  (->>
   (map get-fighter-details)
   (remove nil?)
   (map make-event-tx)
   (d/transact (conn))
   deref
   :db-after))


(defn seed-events [starting-url]
  "Inserts all events into db for the first time, retuns a list of urls and a db"
  (->>
   (get-all-events starting-url)
   (get-and-insert-events)
   (d/q '[:find ?e :where [?e :event/url ?url]])))

(defn db-current? []
  "Checks the difference between the events on system, and the events on FightMetric, inserts the difference."
  (let [diff (st/difference
              (set (get-all-events starting-url))
              (set (get-all-event-urls)))]
    (if (not-empty diff)
      (->>
       (get-and-insert-events diff)
       (d/q '[:find [?url ...] :where [?e :event/url ?url]]))
      (get-all-event-urls))))

(defn insert-fighters-and-fights [row]
  "Takes a row from :event :fight :fighters tree, create a map and inserts"
  (when row
    (insert-fighters (:fighters row))))

(comment
  (defn insert-data []
    (->>
     (db-current?)
     (map scrape-links-from-card)
     (map insert-fighters-and-fights))))

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
