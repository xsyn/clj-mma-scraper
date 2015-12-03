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

;; Create database
(d/create-database uri)

(def conn (d/connect uri))


;; Setup scraper
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
    (if (nil? check-url)
      nil
      (html/html-resource (java.io.StringReader.
                           (:body check-url))))))

;; Utility functions

(defn html-select-first [row tree]
  (first (html/select row tree)))

(defn get-href [element]
  (get-in element [:attrs :href]))

(defn extract-type-from-name [name]
  (cond (.contains name "on Fox") "Fox"
        (.contains name "Ultimate Fighter") "Ultimate Fighter"
        (.contains name "Fight Night") "Fight Night"
        :else "UFC"))

(defn format-date [datestring]
  (c/to-date (tf/parse (tf/formatter "MMMM dd, YYYY") datestring)))

(defn insert-event-into-db [event-name event-url event-date event-location event-type]
  @(d/transact conn [{:db/id #db/id[:db.part/db]
                      :event/name event-name
                      :event/url event-url
                      :event/date event-date
                      :event/location event-location
                      :event/type event-type}]))

;; Scraping stuff

(defn get-all-events [url]
  (rest
   (html/select (fetch-body url) [:table.b-statistics__table-events :tbody :tr.b-statistics__table-row])))

(defn map-event-metadata [row]
  "This function does a lot, it may be worth splitting it up. There's some messing grabbing of the actual content from the page, sanitizeing it, and then it creates a map."
  (let [event-url (get-href
                   (html-select-first row [:tr :> [:td (html/nth-of-type 1)] :i :a]))
        event-name (str/trim (first (:content (html-select-first row [:tr :> [:td (html/nth-of-type 1)] :i :a]))))
        event-date (format-date (str/trim (first (:content (html-select-first row [:tr :> [:td (html/nth-of-type 1)] :i :span] )))))
        event-location (str/trim (first (:content
                                         (html-select-first row  [:tr :> [:td (html/nth-of-type 2)]]))))]
    (insert-event-into-db event-name (URI. event-url) event-date event-location (extract-type-from-name event-name))))




