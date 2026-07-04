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
  unaccounted for."
  (:require [cloudflare.client :as client]))

#?(:clj
(defn zone-routes
  "Worker routes for `zone-id` (classic zone-level route patterns, e.g.
  \"example.com/api/*\" -> a script). Does NOT include Custom Domains
  (see custom-domains) -- those are a newer, separate binding surface."
  ([zone-id] (zone-routes zone-id {}))
  ([zone-id http-opts]
   (client/rest! (str "/zones/" zone-id "/workers/routes") http-opts))))

#?(:clj
(defn custom-domains
  "Account-level Workers Custom Domains: {:hostname :service :zone_name
  :environment :enabled ...} per binding. This is the account-wide list --
  filter client-side by :hostname/:zone_name/:service as needed; there is
  no server-side filter param for this endpoint."
  ([account-id] (custom-domains account-id {}))
  ([account-id http-opts]
   (client/rest! (str "/accounts/" account-id "/workers/domains") http-opts))))

#?(:clj
(defn scripts
  "Worker scripts (metadata only, not source) for `account-id`."
  ([account-id] (scripts account-id {}))
  ([account-id http-opts]
   (client/rest! (str "/accounts/" account-id "/workers/scripts") http-opts))))
