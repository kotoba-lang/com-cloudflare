(ns cloudflare.workers
  "Cloudflare Workers scripts, zone routes, and account-level Custom
  Domains (read-only). REST v4, JVM-only.

  `custom-domains` is the one that matters most in practice: it's what let
  this library's first consumer (gftdcojp/cloud-itonami) actually identify
  which Worker serves a given hostname when neither DNS records nor a
  Pages project's domain list had the answer -- `app.itonami.cloud` turned
  out to be bound to the `local-murakumo` Worker's Custom Domain, not a
  zone-level route or a Pages custom domain (ADR-0010, 2026-07-04). Check
  `custom-domains` before assuming a hostname with no DNS/Pages hit is
  unaccounted for.

  W6 product-shell: pure REST paths via kotoba workers_path_core."
  (:require [cloudflare.client :as client]
            [cloudflare.kotoba.oracle :as oracle]))

(def ^:private oid :workers-path)

(defn- o [export args]
  (oracle/call oid export args))

(defn- o-record
  "T5.2: structural host map → call-record."
  [export host-map field-specs]
  (oracle/call-record oid export host-map field-specs))

(defn- oracle-ready? []
  (oracle/ready? oid))

(defn- try-oracle
  [thunk mirror-thunk]
  (if (oracle-ready?)
    (try
      (thunk)
      (catch #?(:clj Exception :cljs :default) _
        (mirror-thunk)))
    (mirror-thunk)))

(defn zone-routes-path
  "REST path for zone-level Worker routes.
   JVM: kotoba `zone-routes-path`."
  [zone-id]
  (try-oracle
   #(o-record 'zone-routes-path {:zone-id zone-id} [[:zone-id :string]])
   #(str "/zones/" zone-id "/workers/routes")))

(defn custom-domains-path
  "REST path for account Workers Custom Domains.
   JVM: kotoba `custom-domains-path`."
  [account-id]
  (try-oracle
   #(o-record 'custom-domains-path {:account-id account-id} [[:account-id :string]])
   #(str "/accounts/" account-id "/workers/domains")))

(defn scripts-path
  "REST path for account Worker scripts metadata.
   JVM: kotoba `scripts-path`."
  [account-id]
  (try-oracle
   #(o-record 'scripts-path {:account-id account-id} [[:account-id :string]])
   #(str "/accounts/" account-id "/workers/scripts")))

#?(:clj
(defn zone-routes
  "Worker routes for `zone-id` (classic zone-level route patterns, e.g.
  \"example.com/api/*\" -> a script). Does NOT include Custom Domains
  (see custom-domains) -- those are a newer, separate binding surface."
  ([zone-id] (zone-routes zone-id {}))
  ([zone-id http-opts]
   (client/rest! (zone-routes-path zone-id) http-opts))))

#?(:clj
(defn custom-domains
  "Account-level Workers Custom Domains: {:hostname :service :zone_name
  :environment :enabled ...} per binding. This is the account-wide list --
  filter client-side by :hostname/:zone_name/:service as needed; there is
  no server-side filter param for this endpoint."
  ([account-id] (custom-domains account-id {}))
  ([account-id http-opts]
   (client/rest! (custom-domains-path account-id) http-opts))))

#?(:clj
(defn scripts
  "Worker scripts (metadata only, not source) for `account-id`."
  ([account-id] (scripts account-id {}))
  ([account-id http-opts]
   (client/rest! (scripts-path account-id) http-opts))))
