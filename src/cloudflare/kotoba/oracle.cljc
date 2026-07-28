;; cloudflare.kotoba.oracle — product-shell loader for precompiled pure-request KIR.
;;
;; Authority dual-source pattern (W6 product-shell cutover, murakumo#86/#122 form):
;;   1. SSoT source:  kotoba/*_core.kotoba
;;   2. Ship artifact: resources/cloudflare/oracle/*.kir.edn  (precompiled KIR)
;;   3. Host public API delegates here instead of re-implementing pure truth
;;
;; CLJS load (optional, ADR-0014):
;;   - register-kir! — inject pre-parsed KIR (tests / bundlers)
;;   - set-resource-loader! — custom (fn [path] → string)
;;   - nbb/node default: read resources/<path> from process.cwd()
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

(def ^:private resource-loader
  "Optional (fn [classpath-path] → content-string | nil). CLJS inject point."
  (atom nil))

(defn set-resource-loader!
  "Install a resource loader used by cljs/nbb when classpath io is unavailable.
  `f` receives the catalog path and returns file contents as a string, or nil.
  Pass nil to clear. Returns the previous loader."
  [f]
  (let [prev @resource-loader]
    (reset! resource-loader f)
    prev))

(defn register-kir!
  "Inject a pre-parsed KIR document for `oracle-id` (tests / bundlers / nbb preloads).
  Bypasses resource read for that id. Returns the registered document."
  [oracle-id kir]
  (swap! kir-cache assoc oracle-id kir)
  kir)

(defn clear-cache!
  "Drop all cached KIR documents (does not clear resource-loader)."
  []
  (reset! kir-cache {}))

#?(:cljs
   (defn- node-resource-slurp
     "nbb/node: read resources/<path> relative to process.cwd() when available."
     [path]
     (try
       (let [fs (js/require "fs")
             path-mod (js/require "path")
             cwd (str (.cwd js/process))
             full (.resolve path-mod cwd "resources" path)]
         (when (.existsSync fs full)
           (.readFileSync fs full "utf8")))
       (catch :default _ nil))))

(defn- read-resource
  "Read a classpath (or cljs-injected) resource as a string. Throws if missing."
  [path]
  #?(:clj
     (if-let [url (io/resource path)]
       (slurp url)
       (throw (ex-info "kotoba oracle KIR resource missing"
                       {:path path
                        :hint "regenerate via :test oracle-gen or parity drift check"})))
     :cljs
     (let [from-loader (when-let [f @resource-loader] (f path))
           from-node (when (nil? from-loader) (node-resource-slurp path))
           text (or from-loader from-node)]
       (if text
         text
         (throw (ex-info "kotoba oracle resource load failed on cljs"
                         {:path path
                          :hint "set-resource-loader!, register-kir!, or run nbb from repo root with resources/ present"}))))))

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
  "True when the oracle artifact is loadable and parseable on this runtime."
  [oracle-id]
  (try
    (boolean (load-kir oracle-id))
    (catch #?(:clj Exception :cljs :default) _ false)))

(defn as-i64
  "Host integer → KIR i64 payload (JVM long / cljs BigInt)."
  [n]
  #?(:clj (long n)
     :cljs (js/BigInt n)))

(defn i64->host
  "KIR i64 result → host number (cljs BigInt → Number)."
  [v]
  #?(:clj (long v)
     :cljs (js/Number v)))

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
