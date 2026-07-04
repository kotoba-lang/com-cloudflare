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
                     :get (-> builder .GET .build)
                     :delete (-> builder .DELETE .build)
                     (throw (ex-info "Unsupported HTTP method" {:method method})))
           resp (.send (java.net.http.HttpClient/newHttpClient) request
                      (java.net.http.HttpResponse$BodyHandlers/ofString))]
       {:status (.statusCode resp) :body (.body resp)})))))

#?(:clj
(defn api-token
  "CLOUDFLARE_API_TOKEN from the environment, or throw. Callers can always
  override via an explicit :token in opts instead of relying on env."
  []
  (or (System/getenv "CLOUDFLARE_API_TOKEN")
      (throw (ex-info "CLOUDFLARE_API_TOKEN is required" {})))))

#?(:clj
(defn- auth-headers [token]
  {"Authorization" (str "Bearer " token)
   "Content-Type" "application/json"}))

#?(:clj
(defn graphql!
  "POST `request-body` ({:query :variables}) to the Analytics GraphQL API.
  Returns the parsed JSON response ({:data ... :errors ...}), never
  throws on a GraphQL-level error (callers check :errors) -- only throws on
  a transport-level non-2xx HTTP status."
  ([request-body] (graphql! request-body {}))
  ([request-body {:keys [http-fn token] :or {http-fn (jvm-http-fn)}}]
   (let [resp (http-fn {:url graphql-endpoint
                        :method :post
                        :headers (auth-headers (or token (api-token)))
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
  partial-success shape, so failing closed here is the safer default."
  ([path] (rest! path {}))
  ([path {:keys [method body http-fn token query] :or {method :get http-fn (jvm-http-fn)}}]
   (let [query-string (when (seq query)
                        (str "?" (str/join "&" (map (fn [[k v]] (str (name k) "=" v)) query))))
         resp (http-fn (cond-> {:url (str api-base path query-string)
                                :method method
                                :headers (auth-headers (or token (api-token)))}
                        body (assoc :body (json/write-str body))))
         parsed (json/read-str (:body resp) :key-fn keyword)]
     (when-not (and (< (:status resp) 300) (:success parsed))
       (throw (ex-info "Cloudflare REST request failed"
                       {:status (:status resp) :path path :errors (:errors parsed)})))
     (:result parsed)))))
