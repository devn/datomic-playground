(ns datomic-playground.core
  (:require [datomic.api :as d]))

(def uri "datomic:free://localhost:4334/unique")
(d/create-database uri)
(def conn (d/connect uri))
(def db (d/db conn))
