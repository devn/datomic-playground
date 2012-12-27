(ns datomic-playground.simple
  (:require [datomic-simple.core :as ds]))

(def model-namespace :user)
(def schema
  (ds/build-schema model-namespace
                   [[:username :string]
                    [:password :string]]))

(ds/create-model-fns model-namespace)

(ds/start {:uri "datomic:free://localhost:4334/datomicsimple" :schemas [schema] :repl true})

(create {:username "fred", :password "nothing"})
;; => {:id 17592186045420, :username "fred", :password "nothing"}
(create {:username "fred", :password "nothing"})
;; => {:id 17592186045422, :username "fred", :password "nothing"}

(find-first {:username "fred"})
;; => {:id 17592186045420, :username "fred", :password "nothing"}

(find-all {:username "fred"})
;; => ({:id 17592186045420, :username "fred", :password "nothing"} {:id 17592186045422, :username "fred", :password "nothing"})

(update (:id (find-first {:username "fred"})) {:username "notfred"})
;; => {:db-before datomic.db.Db@f7555f6a,
;;     :db-after datomic.db.Db@d4add90f,
;;     :tx-data #<ArrayList [datomic.db.Datum@da78f2c5, datomic.db.Datum@7eea0b8d, datomic.db.Datum@300259]>,
;;     :tempids {}}