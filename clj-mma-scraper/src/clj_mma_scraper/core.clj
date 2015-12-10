(ns clj-mma-scraper.core
  (:require [net.cgrand.enlive-html :as html]
            [datomic.api :as d]
            [clj-time.core :as t]
            [clj-time.format :as tf]
            [clj-time.coerce :as c]
            [clojure.string :as str]
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
(def events-url "http://www.fightmetric.com/statistics/events/completed?page=all")

(def uri "datomic:free://localhost:4334/mma-stats")

(def conn (d/connect uri))

;; Create database
(defn initialize-db [uri]
  "Drops the database if it exists. Creats a new DB with schema"
  (if (false? (d/create-database uri))(do (d/delete-database uri)
                                          (initialize-db uri))
      (let [schema-tx (read-string (slurp "resources/schema.edn"))]
        (do
          (d/create-database uri)
          (d/transact (d/connect uri) schema-tx)))))


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


;; CRUD functions

(defn insert-event [event-name event-url event-date event-location event-type]
  "Transaciton to insert events into Datomic"
  (try
    @(d/transact conn [{:db/id #db/id[:db.part/db]
                        :event/name event-name
                        :event/url event-url
                        :event/date event-date
                        :event/location event-location
                        :event/type event-type}])
    (catch Throwable t nil)))

(defn insert-fighter [m]
  "Insert a fighter map into datomic")

(defn insert-fights [fight-url fight-stats fight-weightclass fight-time fight-rounds fight-method fight-winner fight-loser]
  "Transaction to insert fights into Datomic")

;; Scraping stuff

(defn get-all-events [url]
  "Pulls the event table from FightMetric"
  (let [body (fetch-body url)]
    (cond (nil? body) nil
          :else (rest (html/select body [:table.b-statistics__table-events :tbody :tr.b-statistics__table-row])))))


;; This function may be worth splitting up
(defn insert-event-metadata [row]
  "Takes FightMetric event table, iterates through trs, scrapes data, sanitizes and then inputs into Datomic."
  (cond (nil? row) nil
        :else (let [event-url (get-href (html-select-first row [:tr :> [:td (html/nth-of-type 1)] :i :a]))
                    event-name (get-content row [:tr :> [:td (html/nth-of-type 1)] :i :a])
                    event-date (get-content row [:tr :> [:td (html/nth-of-type 1)] :i :span])
                    event-location (get-content row  [:tr :> [:td (html/nth-of-type 2)]])]
                (insert-event event-name
                              (URI. event-url)
                              (format-date event-date)
                              event-location
                              (extract-type-from-name event-name)))))

;; Event url
(def event-url "http://www.fightmetric.com/event-details/4d636c3aa1105950")

(defn get-fight-items [body]
  "Passing in an html body, returns a map of fight-items, including round, time, time format, referee and details"
  (let [b (html/select body [:i.b-fight-details__text-item])]
    (vec (map (fn [x] (str/trim (nth (:content x) 2))) b))))

(defn get-fighter [url]
  "Gets a fighters details from a url")

(defn map-fighters [body]
  "Gets information about the fighter in a fight"
  (map (fn [row] (let [outcome (str/trim (first (:content (first (html/select row [:div :i])))))
                      name (first (:content (first (html/select row [:div :div :h3 :a]))))
                      fighter-url (get-in (first (html/select row [:div :div :h3 :a])) [:attrs :href])]
                  (zipmap [:outcome :name :url] [outcome name fighter-url]))) body))

;; TODO: Get fighters and stats
(defn get-fight [url]
  "Get's fight details from a url"
  (cond (nil? url) nil
        :else
        (let [body (html-select-first (fetch-body url) [:div.b-fight-details])
              person-body (html/select body [:div.b-fight-details__person])
              details-body (html-select-first body [:div.b-fight-details__fight])
              weightclass (clean-weight-division (get-content body [:div.b-fight-details__fight-head :i]))
              fighters (get-fighters person-body)
              fi (get-fight-items details-body)
              rounds (first fi)
              time (second fi)
              time-format (nth fi 2)
              referee (nth fi 3)
              method (str/trim (first (:content (second (html/select details-body [:i.b-fight-details__text-item_first :i]

                                                                     )))))])))
(defn scrape-links-from-card [url]
  "Returns a seq of links, to the actual fights from a card"
  (cond (nil? url) nil
        :else
        (map get-data-link (html/select (fetch-body url) [:tbody :tr]))))

;; Complete crawl
;;
;;
;;
(;;-> (map insert-event-metadata)
 ;;  (get-all-events)
 ;;  events-url
 )







