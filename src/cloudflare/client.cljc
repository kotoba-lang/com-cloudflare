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
  with a stub, never only against a live account.

  W6 product-shell authority (ADR-0011):
  On the JVM, constants + URL/auth pure helpers DELEGATE to precompiled
  client_core.kir.edn. HTTP/JSON/getenv stay host."
  (:require [clojure.string :as str]
            #?(:clj [clojure.data.json :as json])
            #?(:clj [cloudflare.kotoba.oracle :as oracle])))

(def ^:private oid :client)

#?(:clj
   (defn- o [export args]
     (oracle/call oid export args)))

(def api-base
  #?(:clj (o 'api-base [])
     :cljs "https://api.cloudflare.com/client/v4"))

(def graphql-endpoint
  #?(:clj (o 'graphql-endpoint [])
     :cljs (str api-base "/graphql")))

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
(def api-token-secret-name
  #?(:clj (o 'api-token-secret-name [])
     :cljs "cloudflare-api-token"))

(def api-token-env-name
  #?(:clj (o 'api-token-env-name [])
     :cljs "CLOUDFLARE_API_TOKEN"))

(defn bearer-auth
  "Authorization header value. JVM: kotoba `bearer-auth`."
  [token]
  #?(:clj (o 'bearer-auth [(str token)])
     :cljs (str "Bearer " token)))

(defn rest-url
  "Absolute REST URL for path + query string (no leading `?` in qs).
   JVM: kotoba `rest-url`."
  [path qs]
  #?(:clj (o 'rest-url [(str path) (str (or qs ""))])
     :cljs (if (str/blank? qs)
             (str api-base path)
             (str api-base path "?" qs))))

(defn query-pair
  "One `k=v` query fragment. JVM: kotoba `query-pair`."
  [k v]
  #?(:clj (o 'query-pair [(str k) (str v)])
     :cljs (str k "=" v)))

(defn transport-ok?
  "True when HTTP status is a 2xx. JVM: kotoba `transport-ok?`."
  [status]
  #?(:clj (= 1 (o 'transport-ok? [(long status)]))
     :cljs (< status 300)))

(defn prefer-explicit-token?
  "True when a non-blank explicit token should win over fetch.
   JVM: kotoba `prefer-explicit-token?`."
  [token]
  #?(:clj (= 1 (o 'prefer-explicit-token? [(str (or token ""))]))
     :cljs (and (string? token) (not (str/blank? token)))))

(defn secret-name-matches?
  "True when `name` is the api-token secret id.
   JVM: kotoba `secret-name-matches?`."
  [name]
  #?(:clj (= 1 (o 'secret-name-matches? [(str name)]))
     :cljs (= name api-token-secret-name)))

#?(:clj
(defn env-token-fetch
  "Default host fetch transport: exact env var only (no process-env dump).

  Reply shape matches provider.secret-transport:
  `{:tag :value :value s}` | `{:tag :error :code :message}`."
  []
  (fn [{:keys [name]}]
    (if (secret-name-matches? name)
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
   (or (when (prefer-explicit-token? token) token)
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
   (cond-> {"Authorization" (bearer-auth token)}
     content-type (assoc "Content-Type" content-type)))))

#?(:clj
(defn- resolve-token
  "Pick explicit :token or resolve via named-secret :fetch."
  [{:keys [token fetch] :as opts}]
  (or (when (prefer-explicit-token? token) token)
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
     (when-not (transport-ok? (:status resp))
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
   (let [qs (when (seq query)
              (str/join "&" (map (fn [[k v]] (query-pair (name k) v)) query)))
         url (rest-url path (or qs ""))
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
         resp (http-fn (cond-> {:url url
                                :method method
                                :headers (auth-headers (resolve-token opts) content-type)}
                        body-str (assoc :body body-str)))
         parsed (json/read-str (:body resp) :key-fn keyword)]
     (when-not (and (transport-ok? (:status resp)) (:success parsed))
       (throw (ex-info "Cloudflare REST request failed"
                       {:status (:status resp) :path path :errors (:errors parsed)})))
     (:result parsed)))))
