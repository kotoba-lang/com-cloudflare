(ns cloudflare.deploy
  "W6 cloud-deploy first slice — pure Workers/Pages deploy request builders.

  No ambient network, no wrangler, no filesystem. Callers compose these
  plans with `cloudflare.client/rest!` (or any injectable http-fn) under
  secret-custody for the API token.

  ## Pure surface

  | builder | Cloudflare REST shape |
  |---|---|
  | `workers-script-put-plan` | PUT .../workers/scripts/{name} (service-worker JS) |
  | `workers-script-delete-plan` | DELETE same path |
  | `pages-project-get-path` | GET .../pages/projects/{name} |
  | `wrangler-pages-deploy-argv` | host process-kit argv (optional ops shell) |

  Live helpers (`put-worker-script!` / `delete-worker-script!`) are JVM-only
  thin wrappers around `client/rest!` + the pure plans."
  (:require [clojure.string :as str]
            #?(:clj [cloudflare.client :as client])))

(def max-script-name 64)
(def max-account-id 64)
(def max-script-bytes 5242880) ; 5 MiB service-worker body bound for plan validation

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

  `script-body` must be a string (JS source). Module-format multipart
  uploads are out of scope for this first slice."
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
(defn delete-worker-script!
  "Delete a Worker script via REST DELETE."
  ([account-id script-name]
   (delete-worker-script! account-id script-name {}))
  ([account-id script-name opts]
   (let [plan (workers-script-delete-plan account-id script-name)]
     (client/rest! (:path plan) (merge opts {:method :delete}))))))
