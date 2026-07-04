(ns cloudflare.analytics
  "Cloudflare zone Analytics API (GraphQL): daily traffic trend and
  per-path/device/country/status breakdown. Generalized from
  cloud-itonami.analytics (gftdcojp/cloud-itonami, ADR-0010) -- every
  function here takes an explicit `zone-tag`, no itonami-specific default.

  Two real Cloudflare datasets, with real differences worth knowing before
  using either:

  - `httpRequests1dGroups` (daily-report!): daily aggregate. Any date
    range works on every plan tier.
  - `httpRequestsAdaptiveGroups` (path-report!): per-request granularity
    (path/device/country/status). Free-plan lookback is capped at ~1 day
    -- a wider range fails with a real `quota` GraphQL error, not
    silently-truncated data. `host` is optional: filtering to one host can
    legitimately return zero rows if that host has no traffic in the
    window while the zone as a whole does (a real finding this library's
    first consumer hit, not a bug -- see gftdcojp/cloud-itonami ADR-0010's
    app.itonami.cloud discovery), so omit `host` to see the whole zone
    first before assuming a host-filtered zero means something is broken."
  (:require [clojure.string :as str]
            [cloudflare.client :as client]))

(def daily-query
  "query DailyTraffic($zoneTag: String!, $since: String!, $until: String!) { viewer { zones(filter: {zoneTag: $zoneTag}) { httpRequests1dGroups(limit: 31, filter: {date_geq: $since, date_leq: $until}) { sum { requests pageViews bytes } uniq { uniques } dimensions { date } } } } }")

(defn path-query
  "GraphQL query text for the per-path/device/country/status breakdown.
  `host?` controls whether the query declares/filters on $host at all --
  the caller (path-report-request) only passes a $host variable when a
  host filter was actually requested, so the query text and the variables
  must agree on whether $host exists."
  [host?]
  (str "query PathTraffic($zoneTag: String!, $since: Time!, $until: Time!"
       (when host? ", $host: String!")
       ") { viewer { zones(filter: {zoneTag: $zoneTag}) { httpRequestsAdaptiveGroups(limit: 100, filter: {datetime_geq: $since, datetime_leq: $until"
       (when host? ", clientRequestHTTPHost: $host")
       "}) { count dimensions { clientRequestPath clientDeviceType clientCountryName edgeResponseStatus } } } } }"))

(defn daily-report-request
  [{:keys [zone-tag since until]}]
  {:query daily-query :variables {:zoneTag zone-tag :since since :until until}})

(defn path-report-request
  [{:keys [zone-tag since until host]}]
  {:query (path-query (some? host))
   :variables (cond-> {:zoneTag zone-tag :since since :until until}
                host (assoc :host host))})

(defn- groups [response dataset]
  (get-in response [:data :viewer :zones 0 (keyword dataset)]))

(defn parse-daily-report
  "{:ok? true :days [{:date :requests :page-views :uniques :bytes} ...]
  :totals {...}} sorted by date ascending, or {:ok? false :errors [...]}
  on a GraphQL-level error (never throws -- a quota/syntax error is data,
  not an exception, so callers can decide what to do with it)."
  [response]
  (if-let [errors (seq (:errors response))]
    {:ok? false :errors errors}
    (let [rows (groups response "httpRequests1dGroups")
          days (->> rows
                    (map (fn [r] {:date (get-in r [:dimensions :date])
                                 :requests (get-in r [:sum :requests])
                                 :page-views (get-in r [:sum :pageViews])
                                 :uniques (get-in r [:uniq :uniques])
                                 :bytes (get-in r [:sum :bytes])}))
                    (sort-by :date))]
      {:ok? true
       :days days
       :totals {:requests (reduce + (map :requests days))
                :page-views (reduce + (map :page-views days))
                :uniques (reduce + (map :uniques days))
                :bytes (reduce + (map :bytes days))}})))

(defn parse-path-report
  "{:ok? true :total :by-path :by-device :by-country :by-status} (each a
  {value -> count} map, tallied by summing :count across all matching
  rows) or {:ok? false :errors [...]}."
  [response]
  (if-let [errors (seq (:errors response))]
    {:ok? false :errors errors}
    (let [rows (groups response "httpRequestsAdaptiveGroups")
          tally (fn [k] (reduce (fn [acc r] (update acc (get-in r [:dimensions k]) (fnil + 0) (:count r)))
                                {} rows))]
      {:ok? true
       :total (reduce + (map :count rows))
       :by-path (tally :clientRequestPath)
       :by-device (tally :clientDeviceType)
       :by-country (tally :clientCountryName)
       :by-status (tally :edgeResponseStatus)})))

#?(:clj
(defn daily-report!
  ([opts] (daily-report! opts {}))
  ([opts http-opts] (parse-daily-report (client/graphql! (daily-report-request opts) http-opts)))))

#?(:clj
(defn path-report!
  ([opts] (path-report! opts {}))
  ([opts http-opts] (parse-path-report (client/graphql! (path-report-request opts) http-opts)))))

#?(:clj
(defn iso-date
  "ISO date string `days-ago` days before today -- a daily-report!
  :since/:until building block."
  [days-ago]
  (str (.minusDays (java.time.LocalDate/now) days-ago))))

#?(:clj
(defn iso-instant-hours-ago
  "ISO instant string `hours-ago` hours before now -- a path-report!
  :since/:until building block. Takes whole hours directly (not fractional
  days) -- an earlier version of this in cloud-itonami.analytics divided
  hours by 24.0 and passed the resulting double to Duration/ofDays, which
  takes a long and silently truncated to a zero-width range for any
  sub-24h window (ADR-0010, 2026-07-04)."
  [hours-ago]
  (str (.minus (java.time.Instant/now) (java.time.Duration/ofHours hours-ago)))))
