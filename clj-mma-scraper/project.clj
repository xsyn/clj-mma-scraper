(defproject clj-mma-scraper "0.1.0-SNAPSHOT"
  :description "MMA Analytics engine"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [enlive "1.1.6"]
                 [clj-http "2.0.0"]
                 [cheshire "5.5.0"]
                 [slingshot "0.12.2"]
                 [com.datomic/datomic-free "0.9.5302" :exclusions [joda-time]]
                 [clj-time "0.11.0"]
                 [com.taoensso/timbre "4.1.4"]]
  :profiles {:dev
             {:datomic {:config "resources/transactor.properties"
                        :db-uri "datomic:free://127.0.0.1/mma-stats"}}}
  :datomic {:schemas ["resources" ["schema.edn"]]})
