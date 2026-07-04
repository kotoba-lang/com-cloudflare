(ns cloudflare.zones
  "Cloudflare zone listing + DNS records (read-only). REST v4, JVM-only."
  (:require [cloudflare.client :as client]))

#?(:clj
(defn list-zones
  ([] (list-zones {}))
  ([http-opts] (client/rest! "/zones?per_page=50" http-opts))))

#?(:clj
(defn zone-by-name
  "The zone map for `name` (e.g. \"itonami.cloud\"), or nil if not found /
  not accessible with the current token."
  ([name] (zone-by-name name {}))
  ([name http-opts]
   (first (filter #(= name (:name %)) (list-zones http-opts))))))

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
   (client/rest! (str "/zones/" zone-id "/dns_records")
                (assoc http-opts :query (cond-> {} name (assoc :name name)))))))
