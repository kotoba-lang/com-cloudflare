(ns cloudflare.zones
  "Cloudflare zone listing + DNS records (read-only). REST v4, JVM-only.

  W6 product-shell: pure REST path/query via kotoba zones_path_core
  (+ workers_path list-zones/dns base strings for parity)."
  (:require [cloudflare.client :as client]
            #?(:clj [cloudflare.kotoba.oracle :as oracle])))

(def ^:private oid :zones-path)

#?(:clj
   (defn- o [export args]
     (oracle/call oid export args)))

(defn list-zones-path
  "REST path for zone listing (no query). JVM: kotoba `list-zones-path`."
  []
  #?(:clj (o 'list-zones-path [])
     :cljs "/zones"))

(defn list-zones-request-path
  "Path+query string list-zones passes to rest! (`/zones?per_page=50`).
   JVM: kotoba `list-zones-request-path`."
  []
  #?(:clj (o 'list-zones-request-path [])
     :cljs "/zones?per_page=50"))

(defn dns-records-path
  "REST path for DNS records under a zone. JVM: kotoba `dns-records-path`."
  [zone-id]
  #?(:clj (o 'dns-records-path [(str zone-id)])
     :cljs (str "/zones/" zone-id "/dns_records")))

(defn hostname-matches?
  "Exact hostname/name equality used by zone-by-name and domain filters.
   JVM: kotoba `hostname-matches?`."
  [expected actual]
  #?(:clj (= 1 (o 'hostname-matches? [(str expected) (str actual)]))
     :cljs (= expected actual)))

#?(:clj
(defn list-zones
  ([] (list-zones {}))
  ([http-opts]
   ;; Path embeds per_page=50 (historical contract; zones_path_core SSoT).
   (client/rest! (list-zones-request-path) http-opts))))

#?(:clj
(defn zone-by-name
  "The zone map for `name` (e.g. \"itonami.cloud\"), or nil if not found /
  not accessible with the current token."
  ([name] (zone-by-name name {}))
  ([name http-opts]
   (first (filter #(hostname-matches? name (:name %)) (list-zones http-opts))))))

#?(:clj
(defn dns-records
  "DNS records for `zone-id`, optionally filtered by `name` (exact match,
  e.g. \"app.itonami.cloud\") via a `:name` key in `http-opts`. Requires a
  token with DNS read scope -- Workers/Pages custom-domain routing is a
  separate, usually-broader-scoped surface (see
  cloudflare.workers/custom-domains), so a DNS-scope-only token failing
  here doesn't mean the zone/host doesn't exist."
  ([zone-id] (dns-records zone-id {}))
  ([zone-id {:keys [name] :as http-opts}]
   (client/rest! (dns-records-path zone-id)
                (assoc http-opts :query (cond-> {} name (assoc :name name)))))))
