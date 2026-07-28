(ns cloudflare.client
  "Portable core for talking to the Cloudflare API v4 -- REST and GraphQL
  Analytics, one auth/HTTP boundary for every cloudflare.* namespace in
  this library.

  Extracted from cloud-itonami.analytics (gftdcojp/cloud-itonami,
  ADR-0010/0012), which had grown its own ad hoc, itonami-specific copy of
  this exact query-building/HTTP-call pattern. This library generalizes it
  (no itonami-specific defaults) so any project needing Cloudflare zone/
  worker/analytics access can depend on one tested client instead of
  re-deriving it.

  Query construction and response parsing are pure .cljc. The actual HTTP
  call is JVM-only by default (java.net.http) but always takes an
  injectable `:http-fn` -- the same `{:url :method :headers :body} ->
  {:status :body}` convention used throughout kotoba-lang/gftdcojp's other
  HTTP-calling namespaces (cloud-itonami.runtime/jvm-http-fn,
  cloud-itonami.mail/jvm-http-fn) -- so every namespace here is testable
  with a stub, never only against a live account."
  (:require [clojure.string :as str]
            #?(:clj [clojure.data.json :as json])))

(def api-base "https://api.cloudflare.com/client/v4")
(def graphql-endpoint (str api-base "/graphql"))

#?(:clj
(defn jvm-http-fn
  "Real java.net.http transport. {:url :method :headers :body} ->
  {:status :body}, same convention as cloud-itonami.runtime/jvm-http-fn."
  ([] (jvm-http-fn {}))
  ([{:keys [timeout-seconds] :or {timeout-seconds 30}}]
   (fn [{:keys [url method headers body]}]
     (let [builder (-> (java.net.http.HttpRequest/newBuilder (java.net.URI/create url))
                       (.timeout (java.time.Duration/ofSeconds timeout-seconds))
                       (as-> b (reduce-kv (fn [b k v] (.header b k v)) b headers)))
           request (case method
                     :post (-> builder
                              (.POST (java.net.http.HttpRequest$BodyPublishers/ofString (or body "")))
                              .build)
                     :put (-> builder
                             (.PUT (java.net.http.HttpRequest$BodyPublishers/ofString (or body "")))
                             .build)
                     :get (-> builder .GET .build)
                     :delete (-> builder .DELETE .build)
                     (throw (ex-info "Unsupported HTTP method" {:method method})))
           resp (.send (java.net.http.HttpClient/newHttpClient) request
                      (java.net.http.HttpResponse$BodyHandlers/ofString))]
       {:status (.statusCode resp) :body (.body resp)})))))

;; Named secret identity for W6 secret-custody kit (provider.secret id 21 /
;; secret-transport ADR 0145–0146). Hosts inject :fetch with the same reply
;; shape; default path reads only CLOUDFLARE_API_TOKEN by exact name.
(def api-token-secret-name "cloudflare-api-token")
(def api-token-env-name "CLOUDFLARE_API_TOKEN")

#?(:clj
(defn env-token-fetch
  "Default host fetch transport: exact env var only (no process-env dump).

  Reply shape matches provider.secret-transport:
  `{:tag :value :value s}` | `{:tag :error :code :message}`."
  []
  (fn [{:keys [name]}]
    (if (= name api-token-secret-name)
      (if-let [v (System/getenv api-token-env-name)]
        (if (str/blank? v)
          {:tag :error :code :secret/empty :message "CLOUDFLARE_API_TOKEN empty"}
          {:tag :value :value v})
        {:tag :error :code :secret/not-found :message "CLOUDFLARE_API_TOKEN not set"})
      {:tag :error :code :secret/not-found :message "no env mapping"}))))

#?(:clj
(defn api-token
  "Resolve the Cloudflare API token under the named-secret contract.

  Preference order:
  1. explicit `:token` string
  2. `:fetch` one-shot `(fn [{:keys [name]}] reply)` — kit-compatible;
     wire `provider.secret-transport/env-fetch`, `fn-fetch` (kagi), or
     `keychain-fetch` here
  3. default env-token-fetch (exact CLOUDFLARE_API_TOKEN only)

  Never enumerates process environment keys."
  ([] (api-token {}))
  ([{:keys [token fetch] :or {fetch (env-token-fetch)}}]
   (or (when (and (string? token) (not (str/blank? token))) token)
       (let [reply (fetch {:name api-token-secret-name})]
         (when (and (map? reply) (= :value (:tag reply)))
           (let [v (str (:value reply))]
             (when-not (str/blank? v) v))))
       (throw (ex-info "CLOUDFLARE_API_TOKEN is required"
                       {:secret-name api-token-secret-name
                        :env-name api-token-env-name}))))))

#?(:clj
(defn- auth-headers
  ([token] (auth-headers token "application/json"))
  ([token content-type]
   (cond-> {"Authorization" (str "Bearer " token)}
     content-type (assoc "Content-Type" content-type)))))

#?(:clj
(defn- resolve-token
  "Pick explicit :token or resolve via named-secret :fetch."
  [{:keys [token fetch] :as opts}]
  (or (when (and (string? token) (not (str/blank? token))) token)
      (api-token (cond-> {} fetch (assoc :fetch fetch))))))

#?(:clj
(defn graphql!
  "POST `request-body` ({:query :variables}) to the Analytics GraphQL API.
  Returns the parsed JSON response ({:data ... :errors ...}), never
  throws on a GraphQL-level error (callers check :errors) -- only throws on
  a transport-level non-2xx HTTP status.

  Auth: `:token` string, or `:fetch` kit-shaped secret getter (see api-token)."
  ([request-body] (graphql! request-body {}))
  ([request-body {:keys [http-fn] :or {http-fn (jvm-http-fn)} :as opts}]
   (let [resp (http-fn {:url graphql-endpoint
                        :method :post
                        :headers (auth-headers (resolve-token opts))
                        :body (json/write-str request-body)})]
     (when-not (< (:status resp) 300)
       (throw (ex-info "Cloudflare GraphQL request failed" {:status (:status resp) :body (:body resp)})))
     (json/read-str (:body resp) :key-fn keyword)))))

#?(:clj
(defn rest!
  "Call a Cloudflare REST v4 endpoint. `path` is relative to api-base (e.g.
  \"/zones\" or (str \"/zones/\" zone-id \"/dns_records\")). Returns the
  parsed JSON response's :result on success; throws (with the full
  response) on a transport-level non-2xx status OR a Cloudflare-level
  {:success false} response -- REST errors are structural, unlike GraphQL's
  partial-success shape, so failing closed here is the safer default.

  Auth: `:token` string, or `:fetch` kit-shaped secret getter (see api-token)."
  ([path] (rest! path {}))
  ([path {:keys [method body http-fn query content-type raw-body?]
          :or {method :get http-fn (jvm-http-fn) content-type "application/json"}
          :as opts}]
   (let [query-string (when (seq query)
                        (str "?" (str/join "&" (map (fn [[k v]] (str (name k) "=" v)) query))))
         ;; Deploy uploads may send raw JS (`raw-body?` true); default JSON-encodes maps.
         body-str (cond
                    (nil? body) nil
                    raw-body? (str body)
                    (string? body) body
                    :else (json/write-str body))
         ;; When content-type is application/javascript, treat string body as raw.
         body-str (if (and (string? body)
                           (not= content-type "application/json")
                           (not raw-body?))
                    (str body)
                    body-str)
         resp (http-fn (cond-> {:url (str api-base path query-string)
                                :method method
                                :headers (auth-headers (resolve-token opts) content-type)}
                        body-str (assoc :body body-str)))
         parsed (json/read-str (:body resp) :key-fn keyword)]
     (when-not (and (< (:status resp) 300) (:success parsed))
       (throw (ex-info "Cloudflare REST request failed"
                       {:status (:status resp) :path path :errors (:errors parsed)})))
     (:result parsed)))))
