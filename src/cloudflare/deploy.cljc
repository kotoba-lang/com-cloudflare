(ns cloudflare.deploy
  "W6 cloud-deploy — pure Workers/Pages deploy request builders.

  No ambient network, no wrangler, no filesystem. Callers compose these
  plans with `cloudflare.client/rest!` (or any injectable http-fn) under
  secret-custody for the API token.

  ## Pure surface

  | builder | Cloudflare REST shape |
  |---|---|
  | `workers-script-put-plan` | PUT service-worker JS body |
  | `workers-module-put-plan` | PUT multipart module upload (ADR 0005) |
  | `workers-script-delete-plan` | DELETE script |
  | `pages-deployments-path` | GET/POST deployments collection |
  | `wrangler-pages-deploy-argv` | host process-kit argv (optional ops shell) |

  Live helpers are JVM-only thin wrappers around `client/rest!` + plans."
  (:require [clojure.string :as str]
            #?(:clj [cloudflare.client :as client])
            #?(:clj [clojure.data.json :as json])))

(def max-script-name 64)
(def max-account-id 64)
(def max-module-name 128)
(def max-script-bytes 5242880) ; 5 MiB per module body bound for plan validation
(def max-modules 16)

(defn validate-account-id
  "Pure account-id policy. nil when ok, else error keyword."
  [account-id]
  (let [s (str account-id)]
    (cond
      (str/blank? s) :deploy/empty-account
      (> (count s) max-account-id) :deploy/account-too-long
      (not (re-matches #"[A-Za-z0-9_-]+" s)) :deploy/bad-account
      :else nil)))

(defn validate-script-name
  "Pure Worker script name policy (CF: letters, numbers, underscore, hyphen)."
  [script-name]
  (let [s (str script-name)]
    (cond
      (str/blank? s) :deploy/empty-script
      (> (count s) max-script-name) :deploy/script-too-long
      (not (re-matches #"[A-Za-z0-9][A-Za-z0-9_-]*" s)) :deploy/bad-script
      :else nil)))

(defn validate-project-name
  "Pure Pages project name policy (same charset as script names)."
  [project-name]
  (validate-script-name project-name))

(defn workers-script-path
  "REST path for a Worker script resource."
  [account-id script-name]
  (when-let [err (or (validate-account-id account-id)
                     (validate-script-name script-name))]
    (throw (ex-info "cloudflare.deploy path validation failed"
                    {:phase :cloudflare-deploy :error err})))
  (str "/accounts/" account-id "/workers/scripts/" script-name))

(defn pages-project-path
  "REST path for a Pages project resource."
  [account-id project-name]
  (when-let [err (or (validate-account-id account-id)
                     (validate-project-name project-name))]
    (throw (ex-info "cloudflare.deploy path validation failed"
                    {:phase :cloudflare-deploy :error err})))
  (str "/accounts/" account-id "/pages/projects/" project-name))

(defn pages-deployments-path
  "REST path for listing/creating Pages deployments (metadata API)."
  [account-id project-name]
  (str (pages-project-path account-id project-name) "/deployments"))

(defn workers-script-put-plan
  "Pure plan for PUT of a service-worker format script body.

  Returns
  `{:method :put :path :headers :body :content-type}` — no I/O.

  `script-body` must be a string (JS source). For ES modules use
  `workers-module-put-plan`."
  [account-id script-name script-body]
  (when-let [err (or (validate-account-id account-id)
                     (validate-script-name script-name))]
    (throw (ex-info "cloudflare.deploy put-plan validation failed"
                    {:phase :cloudflare-deploy :error err})))
  (when-not (string? script-body)
    (throw (ex-info "cloudflare.deploy put-plan requires string script-body"
                    {:phase :cloudflare-deploy})))
  (when (> (count script-body) max-script-bytes)
    (throw (ex-info "cloudflare.deploy script body too large"
                    {:phase :cloudflare-deploy :error :deploy/script-too-large})))
  {:method :put
   :path (workers-script-path account-id script-name)
   :content-type "application/javascript"
   :headers {"Content-Type" "application/javascript"}
   :body script-body})

(defn validate-module-name
  "Pure module file name policy (e.g. main.js, src/index.mjs)."
  [module-name]
  (let [s (str module-name)]
    (cond
      (str/blank? s) :deploy/empty-module
      (> (count s) max-module-name) :deploy/module-too-long
      (str/includes? s "\0") :deploy/null-byte
      (str/includes? s "..") :deploy/module-escape
      (str/starts-with? s "/") :deploy/module-absolute
      (not (re-matches #"[A-Za-z0-9][A-Za-z0-9_./-]*" s)) :deploy/bad-module
      :else nil)))

(defn module-metadata
  "Pure Workers multipart `metadata` JSON map (ES modules).

  opts:
    :main-module       required module name (must appear in modules)
    :compatibility-date  optional string
    :bindings          optional vector of binding maps (passed through)"
  [{:keys [main-module compatibility-date bindings] :as opts}]
  (when-let [err (validate-module-name main-module)]
    (throw (ex-info "cloudflare.deploy module-metadata validation failed"
                    {:phase :cloudflare-deploy :error err})))
  (cond-> {:main_module (str main-module)}
    compatibility-date (assoc :compatibility_date (str compatibility-date))
    (seq bindings) (assoc :bindings (vec bindings))))

(defn- multipart-part
  "Encode one multipart form part (CRLF). Pure string."
  [boundary {:keys [name filename content-type body]}]
  (str "--" boundary "\r\n"
       "Content-Disposition: form-data; name=\"" name "\""
       (when filename (str "; filename=\"" filename "\""))
       "\r\n"
       (when content-type (str "Content-Type: " content-type "\r\n"))
       "\r\n"
       body "\r\n"))

(defn encode-multipart
  "Pure multipart/form-data body for the given parts + boundary.

  parts: seq of {:name :filename? :content-type? :body}"
  [boundary parts]
  (when (or (str/blank? (str boundary))
            (str/includes? (str boundary) " ")
            (str/includes? (str boundary) "\""))
    (throw (ex-info "cloudflare.deploy multipart boundary invalid"
                    {:phase :cloudflare-deploy})))
  (str (apply str (map #(multipart-part boundary %) parts))
       "--" boundary "--\r\n"))

(defn workers-module-put-plan
  "Pure plan for ES-module Worker upload (multipart metadata + modules).

  `modules` is a map of module-name → source string, e.g.
  `{\"main.js\" \"export default { fetch(){...} }\"}`.

  opts:
    :main-module         defaults to first key or \"main.js\"
    :compatibility-date  optional
    :bindings            optional
    :boundary            optional multipart boundary string

  Returns `{:method :put :path :content-type :headers :body :metadata :modules}`."
  ([account-id script-name modules]
   (workers-module-put-plan account-id script-name modules {}))
  ([account-id script-name modules {:keys [main-module compatibility-date bindings boundary]
                                    :as opts}]
   (when-let [err (or (validate-account-id account-id)
                      (validate-script-name script-name))]
     (throw (ex-info "cloudflare.deploy module-put validation failed"
                     {:phase :cloudflare-deploy :error err})))
   (when-not (and (map? modules) (seq modules)
                  (every? string? (keys modules))
                  (every? string? (vals modules)))
     (throw (ex-info "cloudflare.deploy modules must be non-empty string map"
                     {:phase :cloudflare-deploy})))
   (when (> (count modules) max-modules)
     (throw (ex-info "cloudflare.deploy too many modules"
                     {:phase :cloudflare-deploy :error :deploy/too-many-modules})))
   (doseq [[n body] modules]
     (when-let [err (validate-module-name n)]
       (throw (ex-info "cloudflare.deploy module name invalid"
                       {:phase :cloudflare-deploy :error err :module n})))
     (when (> (count body) max-script-bytes)
       (throw (ex-info "cloudflare.deploy module body too large"
                       {:phase :cloudflare-deploy :error :deploy/script-too-large
                        :module n}))))
   (let [main (str (or main-module (first (keys modules)) "main.js"))
         _ (when-not (contains? modules main)
             (throw (ex-info "cloudflare.deploy main_module missing from modules"
                             {:phase :cloudflare-deploy :main-module main})))
         meta (module-metadata (cond-> {:main-module main}
                                 compatibility-date (assoc :compatibility-date compatibility-date)
                                 bindings (assoc :bindings bindings)))
         boundary (or boundary (str "----kotoba-cf-boundary-"
                                    #?(:clj (Long/toHexString (System/nanoTime))
                                       :cljs (.toString (.now js/Date) 16))))
         meta-json #?(:clj (json/write-str meta)
                      :cljs (.stringify js/JSON (clj->js meta)))
         parts (into [{:name "metadata"
                       :content-type "application/json"
                       :body meta-json}]
                     (map (fn [[n body]]
                            {:name n
                             :filename n
                             :content-type "application/javascript+module"
                             :body body})
                          modules))
         body (encode-multipart boundary parts)
         ct (str "multipart/form-data; boundary=" boundary)]
     {:method :put
      :path (workers-script-path account-id script-name)
      :content-type ct
      :headers {"Content-Type" ct}
      :body body
      :metadata meta
      :modules modules
      :boundary boundary})))

(defn workers-script-delete-plan
  "Pure plan for DELETE of a Worker script."
  [account-id script-name]
  {:method :delete
   :path (workers-script-path account-id script-name)
   :headers {}
   :body nil})

(defn wrangler-pages-deploy-argv
  "Optional host process-kit argv for `wrangler pages deploy`.

  Pure vector — no PATH resolution. Host must map basename `wrangler` via
  process-transport `:binaries` (or pass absolute bin as first element
  via `wrangler-bin`)."
  ([project directory]
   (wrangler-pages-deploy-argv project directory nil))
  ([project directory wrangler-bin]
   (when-let [err (validate-project-name project)]
     (throw (ex-info "cloudflare.deploy wrangler plan validation failed"
                     {:phase :cloudflare-deploy :error err})))
   (when (or (str/blank? (str directory))
             (str/includes? (str directory) "\0"))
     (throw (ex-info "cloudflare.deploy wrangler plan bad directory"
                     {:phase :cloudflare-deploy})))
   (let [bin (or (not-empty (str wrangler-bin)) "wrangler")]
     [bin "pages" "deploy" (str directory) "--project-name" (str project)])))

#?(:clj
(defn put-worker-script!
  "Upload a service-worker format Worker script via REST PUT.

  Uses pure `workers-script-put-plan` + `client/rest!`. Auth via
  `:token` / `:fetch` (named secret kit). Injectable `:http-fn`."
  ([account-id script-name script-body]
   (put-worker-script! account-id script-name script-body {}))
  ([account-id script-name script-body opts]
   (let [plan (workers-script-put-plan account-id script-name script-body)]
     (client/rest! (:path plan)
                   (merge opts
                          {:method :put
                           :body (:body plan)
                           :content-type (:content-type plan)}))))))

#?(:clj
(defn put-worker-module!
  "Upload an ES-module Worker via multipart PUT (metadata + modules).

  `modules` map module-name → source. See `workers-module-put-plan`."
  ([account-id script-name modules]
   (put-worker-module! account-id script-name modules {}))
  ([account-id script-name modules opts]
   (let [plan (workers-module-put-plan account-id script-name modules
                                       (select-keys opts [:main-module
                                                          :compatibility-date
                                                          :bindings
                                                          :boundary]))]
     (client/rest! (:path plan)
                   (merge (dissoc opts :main-module :compatibility-date
                                  :bindings :boundary)
                          {:method :put
                           :body (:body plan)
                           :content-type (:content-type plan)
                           :raw-body? true}))))))

#?(:clj
(defn delete-worker-script!
  "Delete a Worker script via REST DELETE."
  ([account-id script-name]
   (delete-worker-script! account-id script-name {}))
  ([account-id script-name opts]
   (let [plan (workers-script-delete-plan account-id script-name)]
     (client/rest! (:path plan) (merge opts {:method :delete}))))))
