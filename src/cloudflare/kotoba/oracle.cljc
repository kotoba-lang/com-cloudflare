;; cloudflare.kotoba.oracle — product-shell loader for precompiled pure-request KIR.
;;
;; Authority dual-source pattern (W6 product-shell cutover, murakumo#86 form):
;;   1. SSoT source:  kotoba/*_core.kotoba
;;   2. Ship artifact: resources/cloudflare/oracle/*.kir.edn  (precompiled KIR)
;;   3. Host public API delegates here instead of re-implementing pure truth
;;
;; Compiler stays test-only. Production needs only kotoba-kir + checked-in resources.

(ns cloudflare.kotoba.oracle
  "Load precompiled kotoba KIR pure-request artifacts and execute exports.
  Kotoba source is the authority; this ns is the product-shell call path."
  (:require [clojure.edn :as edn]
            [kotoba.kir :as ir]
            #?(:clj [clojure.java.io :as io])))

(def ^:private catalog
  "Logical oracle id → classpath resource path under resources/."
  {:client "cloudflare/oracle/client_core.kir.edn"
   :workers-path "cloudflare/oracle/workers_path_core.kir.edn"
   :zones-path "cloudflare/oracle/zones_path_core.kir.edn"
   :logpush-path "cloudflare/oracle/logpush_path_core.kir.edn"
   :stream "cloudflare/oracle/stream_core.kir.edn"
   :deploy "cloudflare/oracle/deploy_core.kir.edn"
   :pages-bulk "cloudflare/oracle/pages_bulk_core.kir.edn"
   :analytics "cloudflare/oracle/analytics_core.kir.edn"
   :analytics-parse "cloudflare/oracle/analytics_parse_core.kir.edn"})

(def ^:private kir-cache
  "Atom map of oracle-id → loaded KIR document."
  (atom {}))

(defn- read-resource
  "Read a classpath resource as a string. Throws if missing."
  [path]
  #?(:clj
     (if-let [url (io/resource path)]
       (slurp url)
       (throw (ex-info "kotoba oracle KIR resource missing"
                       {:path path
                        :hint "regenerate via :test oracle-gen or parity drift check"})))
     :cljs
     (throw (ex-info "kotoba oracle resource load is JVM-only in this slice"
                     {:path path}))))

(defn load-kir
  "Load (and cache) the precompiled KIR for `oracle-id` (keyword in catalog)."
  [oracle-id]
  (if-let [hit (get @kir-cache oracle-id)]
    hit
    (let [path (or (get catalog oracle-id)
                   (throw (ex-info "unknown kotoba oracle id"
                                   {:oracle-id oracle-id
                                    :known (keys catalog)})))
          kir (edn/read-string (read-resource path))]
      (swap! kir-cache assoc oracle-id kir)
      kir)))

(defn ready?
  "True when the oracle artifact is on the classpath and parseable."
  [oracle-id]
  (try
    (boolean (load-kir oracle-id))
    (catch #?(:clj Exception :cljs :default) _ false)))

(defn call
  "Execute a pure export on the precompiled oracle.

  `oracle-id`  — keyword in catalog (e.g. :client)
  `export`     — symbol matching a kotoba (:export …) name
  `args`       — vector of host values (strings / i64 longs) matching guest ABI"
  [oracle-id export args]
  (let [kir (load-kir oracle-id)
        fn-name (if (symbol? export) export (symbol (name export)))]
    (ir/execute kir fn-name (vec args))))

(defn catalog-ids
  "Known oracle ids shipped as product-shell artifacts."
  []
  (keys catalog))

(defn catalog-count
  "Number of shipped product-shell oracle artifacts."
  []
  (count catalog))
