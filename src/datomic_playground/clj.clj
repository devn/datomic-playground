(ns datomic-playground.clj
  (:use [clojure.string :only (split)])
  (:require [datomic.api :as d]
            [clojure.string :as s]))

(def data
  (map (juxt inc #(-> %(+ 97) char str))
       (range 26)))
;;=> ([1 "a"] [2 "b"] ... [26 "z"])

(d/q '[:find ?n ?char
       :in [[?n ?char]]
       :where [(mod ?n 2) ?result] ;; let the value of (mod ?n 2) for each ?n be ?result
              [(= ?result 0)]]     ;; select only the ?result values that are equal 0
     data)

;; Now, let's make it a rule...
(def rules
  '[[(div-by-two ?n)
     [(mod ?n 2) ?result]
     [(= ?result 0)]]])

(d/q '[:find ?n ?char
       :in % [[?n ?char]] ;; % => rules
       :where (div-by-two ?n)]
     rules
     data)

(def rules-2
  '[[(div-by-two ?n)
     [(mod ?n 2) ?x]
     [(= ?x 0)]]
    [(div-by-two ?n)
     [(not= ?n 0)]]])

(d/q '[:find ?n ?char
       :in % [[?n ?char]]
       :where (div-by-two ?n)]
     rules-2
     data)

(def rules-3
  '[[(div-by-two ?n)
     [(mod ?n 2) ?x]
     [(= ?x 0)]]
    [(div-by-two ?n)
     [(not= ?n 0)]]
    [(less-than-twenty ?n)
     [(< ?n 20)]]])

(d/q '[:find ?n ?char
       :in % [[?n ?char]]
       :where (div-by-two ?n)
       (less-than-twenty ?n)]
     rules-3
     data)

(def project-files
  (rest (file-seq (java.io.File. (System/getProperty "user.dir")))))

(def rules-4
  '[[(clojure-files ?f)
     [(.getName ?f) ?fileName]
     [(clojure.string/split ?fileName #"\.") [?pathElem ...]] ;; Must be fully-qualified path to split fn
     [(= ?pathElem "clj")]]])

(d/q '[:find ?files
       :in % [?files ...]
       :where (clojure-files ?files)]
     rules-4
     project-files)

(def rules-5
  '[[(git-files ?f)
     [(str ?f) ?path]
     [(clojure.string/split ?path #"\/") [?pathElem ...]]
     ;; (println ?pathElem) ;; Be careful when using this to debug. It will short-circuit your query.
     [(= ".git" ?pathElem)]]])

(d/q '[:find ?files
       :in % [?files ...]
       :where (git-files ?files)]
     rules-5
     project-files)

(def core-meta
  (map (comp meta second)
       (ns-publics 'clojure.core)))

(defn added-in-version [^String version]
  (d/q '[:find ?name
         :in [?meta-map ...] ?version
         :where [(:added ?meta-map) ?version-added]
                [(:name ?meta-map) ?name]
                [(= ?version-added ?version)]]
       core-meta
       version))

(comment

(into {}
      (for [v ["1.0" "1.2" "1.3" "1.4"]]
        {v (apply concat (added-in-version v))}))
)