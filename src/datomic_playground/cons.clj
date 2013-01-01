(ns 
  datomic-playground.cons
  "Lifted from Chas Emerick's gist: https://gist.github.com/3e615a4d42b88ccefdb4"
  (:require [datomic.api :as d]))

;; all of this is surely ill-advised. Please ignore.
;; scroll down for the fun REPL interactions 
(defonce conn (let [uri "datomic:mem://collections"]
                (d/create-database uri)
                (d/connect uri)))

(def coll-schema
  [[:db/add #db/id[:db.part/user -10] :db/ident :coll/list]
   [:db/add #db/id[:db.part/user -11] :db/ident :coll/map]
   
   {:db/id #db/id[:db.part/db]
    :db/ident :coll/type
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/isComponent true
    :db/doc "Keyword indicating type of collection represented by the attributes
rooted at this entity, either :coll/list or :coll/map."
    :db.install/_attribute :db.part/db}])

(def cons-schema
  [{:db/id #db/id[:db.part/db]
    :db/ident :list/str-val
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/isComponent true
    :db/doc "String cons cell value."
    :db.install/_attribute :db.part/db}
   
   {:db/id #db/id[:db.part/db]
    :db/ident :list/ref-val
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/isComponent true
    :db/doc "Non-scalar cons cell value."
    :db.install/_attribute :db.part/db}
   
   {:db/id #db/id[:db.part/db]
    :db/ident :list/next
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/isComponent true
    :db/doc "Ref to next cons."
    :db.install/_attribute :db.part/db}])

(def map-schema
  [{:db/id #db/id[:db.part/db]
    :db/ident :map/entry
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/many
    :db/isComponent true
    :db/doc "Refs to map entries."
    :db.install/_attribute :db.part/db}
   
   {:db/id #db/id[:db.part/db]
    :db/ident :map/key
    :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one
    :db/doc "Key(word) of a map entry"
    :db.install/_attribute :db.part/db}
   
   {:db/id #db/id[:db.part/db]
    :db/ident :map/str-val
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/doc "String val of a map entry"
    :db.install/_attribute :db.part/db}
   
   {:db/id #db/id[:db.part/db]
    :db/ident :map/ref-val
    :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one
    :db/isComponent true
    :db/doc "Non-scalar map entry value."
    :db.install/_attribute :db.part/db}])

(d/transact conn coll-schema)
(d/transact conn cons-schema)
(d/transact conn map-schema)

(defmulti datom->coll :coll/type)
(defmethod datom->coll :default
  [x] x)

(defmethod datom->coll :coll/list
  [root]
  (lazy-seq
    (if-let [str-val (:list/str-val root)]
      (cons str-val (datom->coll (:list/next root)))
      (when-let [ref-val (:list/ref-val root)]
        (cons (datom->coll ref-val) (datom->coll (:list/next root)))))))

(defmethod datom->coll :coll/map
  [root]
  (reduce
    (fn [m entry]
      (assoc m
        (:map/key entry)
        (or (:map/str-val entry)
            (datom->coll (:map/ref-val entry)))))
    {} (:map/entry root)))

; just a dumb way to get stable IDs to establish references between entities
; Note that this forces the use of a separate entity even if there is
; shared structure, e.g. [{:a 5} {:a 5}] will end up storing two separate
; maps (unless the maps are `identical?`), which would not be the case if
; e.g. simple equality checks were performed to store / look up IDs.
; It may end up being desirable to configure this behaviour, e.g. to ensure
; that equal (or even `identical?`) sub-collections yield shared entities
(def sys-id #(d/tempid :db.part/user (-> % System/identityHashCode Math/abs -)))

(defmulti coll->facts class)

(defmethod coll->facts java.util.Map
  [m]
  (concat [{:db/id (sys-id m)
            :coll/type :coll/map}]
          (for [[k v :as e] m
                :when v]
            (merge {:db/id (sys-id e)
                    :map/_entry (sys-id m)
                    :map/key k}
                   (if (string? v)
                     {:map/str-val v}
                     {:map/ref-val (sys-id v)})))
          ;; gah, this is horrible.
          (->> (vals m)
            (remove string?)
            (remove keyword?)
            (remove nil?)
            (mapcat coll->facts))))

(defmethod coll->facts java.util.List
  [s]
  (when (seq s)
    (let [[v & rst] s]
      (when-not v (throw (IllegalArgumentException. "Cannot store lists containing nil members.")))
      (concat [(merge {:db/id (sys-id s)
                       :coll/type :coll/list}
                      (when v
                        (if (string? v)
                          {:list/str-val v}
                          {:list/ref-val (sys-id v)}))
                      (when (seq rst)
                        {:list/next (sys-id rst)}))]
              (when-not (string? v)
                (coll->facts v))
              (lazy-seq (coll->facts (sequence rst)))))))




(d/transact conn [{:db/id #db/id [:db.part/user]
                   :db/ident :add-map
                   ;; #db/fn doesn't seem to work? Or, doesn't work with clojure?
                   ;; CompilerException java.lang.RuntimeException: Can't embed object in code, maybe print-dup not defined: clojure.lang.Delay@7b1ebc46,
                   :db/fn (d/function '{:lang "clojure"
                                        :params [db m]
                                        :requires ([cemerick.datomic-play :as play])
                                        :code (play/coll->facts m)})}])
#_(defn all-maps
  []
  (->> (db conn)
    ; add [?id :map/name "outer"] to verify propagation of deletion via drop-all
    (q `[:find ?id :where [?id :map/entry]])))  

#_(defn drop-all
  []
  (->> (all-maps)
    (map (fn [[id]] [:db.fn/retractEntity id]))
    (d/transact conn)))




; ok, time for the fun
(comment
(d/transact conn [[:add-map {:a "5" :b "12"}]
                  [:add-map {:c "3" :d {:e "19" :f {:z "14"}}}]])

;; what key is "14" mapped to?

(q '[:find ?key :where [?id :map/str-val "14"] [?id :map/key ?key]] (db conn))
;=> #<HashSet [[:z]]>

;; where is the map that contains "14" keyed under?
;; (this is getting complicated ... rules make this slick, see below)
(q '[:find ?key
     :where
     [?id :map/str-val "14"]
     [?parent :map/entry ?id]
     [?parent2 :map/ref-val ?parent]
     [?parent2 :map/key ?key]] (db conn))
;=> #<HashSet [[:f]]>

(d/transact conn [{:db/id #db/id[:db.part/db]
                   :db/ident :app/data
                   :db/valueType :db.type/ref
                   :db/cardinality :db.cardinality/one
                   :db.install/_attribute :db.part/db}
                  {:db/id #db/id[:db.part/db]
                   :db/ident :app/name
                   :db/valueType :db.type/string
                   :db/cardinality :db.cardinality/one
                   :db.install/_attribute :db.part/db}])

(d/transact conn (let [data {:info {:j "xx" :k ["yy" "zz"]}
                             :data [{:p (map str "hello")}]}]
                   (cons {:db/id #db/id[:db.part/user]
                          :app/name "Zippy"
                          :app/data (sys-id data)}
                         (coll->facts data))))

;; ah, querying with rules is a lot more fun :-)
; find me a map entry of [:j "xx"], and return its ancestor
; entity that possesses an :app/name attribute
(q '[:find ?e
     :in $ %
     :where
     (attr= :j "xx" ?id)
     (gp-with-av ?e :app/name _ ?id)]
   (db conn)
   '[[[gp-with-av ?e ?a ?v ?leaf]
      [?e _ ?leaf]
      [?e ?a ?v]]
     [[gp-with-av ?e ?a ?v ?leaf]
      [?p _ ?leaf]
      (gp-with-av ?e ?a ?v ?p)]
     [[attr= ?k ?v ?e]
      [?e :map/key ?k]
      [?e :map/str-val ?v]]])
;=> #<HashSet [[17592186045537]]>
(->> *1 first first (d/entity (db conn)) :app/name)
;=> {:db/id 17592186045537}
(->> *2 first first (d/entity (db conn)) :app/data datom->coll)
;=> {:info {:k ("yy" "zz"), :j "xx"}, :data ({:p ("h" "e" "l" "l" "o")})}

)
