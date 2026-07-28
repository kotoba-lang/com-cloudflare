;; W6 product-shell oracle authority for com-cloudflare:
;;   1. precompiled KIR resources are loadable and execute pure helpers
;;   2. JVM public APIs match live-compiled kotoba
;;   3. checked-in KIR resources do not drift from kotoba/*_core.kotoba

(ns cloudflare.kotoba-oracle-authority-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [cloudflare.client :as client]
            [cloudflare.workers :as workers]
            [cloudflare.zones :as zones]
            [cloudflare.pages :as pages]
            [cloudflare.logpush :as logpush]
            [cloudflare.kotoba.oracle :as oracle]
            [cloudflare.kotoba-oracle-gen :as gen]
            [kotoba.compiler.core :as compiler]
            [kotoba.kir :as ir]))

(deftest oracle-catalog-ready
  (is (= 9 (oracle/catalog-count))
      "full product-shell catalog ships all kotoba/*_core artifacts")
  (doseq [id (oracle/catalog-ids)]
    (is (oracle/ready? id) (str "not ready: " id)))
  (is (some #{:client} (oracle/catalog-ids)))
  (is (some #{:workers-path} (oracle/catalog-ids)))
  (is (some #{:zones-path} (oracle/catalog-ids)))
  (is (some #{:logpush-path} (oracle/catalog-ids))))

(deftest product-shell-client-uses-oracle-results
  (testing "constants from client_core"
    (is (= "https://api.cloudflare.com/client/v4" client/api-base))
    (is (= "https://api.cloudflare.com/client/v4/graphql" client/graphql-endpoint))
    (is (= "cloudflare-api-token" client/api-token-secret-name))
    (is (= "CLOUDFLARE_API_TOKEN" client/api-token-env-name)))
  (testing "URL/auth pure helpers"
    (is (= "Bearer tok" (client/bearer-auth "tok")))
    (is (= "name=app.example.com" (client/query-pair "name" "app.example.com")))
    (is (= (str client/api-base "/zones")
           (client/rest-url "/zones" "")))
    (is (= (str client/api-base "/zones?per_page=50")
           (client/rest-url "/zones" "per_page=50")))
    (is (true? (client/transport-ok? 200)))
    (is (false? (client/transport-ok? 500)))
    (is (true? (client/prefer-explicit-token? "abc")))
    (is (false? (client/prefer-explicit-token? "")))
    (is (true? (client/secret-name-matches? "cloudflare-api-token")))
    (is (false? (client/secret-name-matches? "other"))))
  (testing "rest! uses oracle rest-url + bearer-auth"
    (let [captured (atom nil)
          http-fn (fn [req]
                    (reset! captured req)
                    {:status 200 :body "{\"success\":true,\"result\":[]}"})]
      (client/rest! "/zones" {:http-fn http-fn :token "t"
                              :query {:per_page "50"}})
      (is (= (client/rest-url "/zones" "per_page=50") (:url @captured)))
      (is (= "Bearer t" (get (:headers @captured) "Authorization"))))))

(deftest product-shell-path-helpers-use-oracle
  (is (= "/zones/z1/workers/routes" (workers/zone-routes-path "z1")))
  (is (= "/accounts/a1/workers/domains" (workers/custom-domains-path "a1")))
  (is (= "/accounts/a1/workers/scripts" (workers/scripts-path "a1")))
  (is (= "/zones" (zones/list-zones-path)))
  (is (= "/zones?per_page=50" (zones/list-zones-request-path)))
  (is (= "/zones/z1/dns_records" (zones/dns-records-path "z1")))
  (is (true? (zones/hostname-matches? "a" "a")))
  (is (false? (zones/hostname-matches? "a" "b")))
  (is (= "/accounts/a1/pages/projects" (pages/projects-path "a1")))
  (is (= "/zones/z1/logpush/datasets" (logpush/datasets-path "z1")))
  (is (= "/zones/z1/logpush/jobs" (logpush/jobs-path "z1")))
  (is (= "/zones/z1/logpush/jobs/j9" (logpush/job-path "z1" "j9"))))

(deftest client-oracle-call-matches-live-compile
  (let [live (:kir (compiler/compile-source (slurp "kotoba/client_core.kotoba")
                                            :wasm32-kotoba-v1 {}))]
    (is (= (ir/execute live 'api-base [])
           (oracle/call :client 'api-base [])))
    (is (= (ir/execute live 'graphql-endpoint [])
           (oracle/call :client 'graphql-endpoint [])))
    (is (= (ir/execute live 'rest-url ["/zones" "per_page=50"])
           (oracle/call :client 'rest-url ["/zones" "per_page=50"])))
    (is (= (ir/execute live 'bearer-auth ["t"])
           (oracle/call :client 'bearer-auth ["t"])))
    (is (= (ir/execute live 'transport-ok? [200])
           (oracle/call :client 'transport-ok? [200])))))

(deftest workers-path-oracle-call-matches-live-compile
  (let [live (:kir (compiler/compile-source (slurp "kotoba/workers_path_core.kotoba")
                                            :wasm32-kotoba-v1 {}))]
    (is (= (ir/execute live 'zone-routes-path ["z"])
           (oracle/call :workers-path 'zone-routes-path ["z"])))
    (is (= (ir/execute live 'dns-records-path ["z"])
           (oracle/call :workers-path 'dns-records-path ["z"])))
    (is (= (ir/execute live 'pages-projects-path ["a"])
           (oracle/call :workers-path 'pages-projects-path ["a"])))))

(deftest logpush-path-oracle-call-matches-live-compile
  (let [live (:kir (compiler/compile-source (slurp "kotoba/logpush_path_core.kotoba")
                                            :wasm32-kotoba-v1 {}))]
    (is (= (ir/execute live 'job-path ["z" "j"])
           (oracle/call :logpush-path 'job-path ["z" "j"])))))

(deftest precompiled-client-kir-does-not-drift
  (let [live (:kir (compiler/compile-source (slurp "kotoba/client_core.kotoba")
                                            :wasm32-kotoba-v1 {}))
        shipped (edn/read-string
                 (slurp (io/resource "cloudflare/oracle/client_core.kir.edn")))]
    (is (= live shipped)
        "client_core KIR drift — run: clojure -M:test -m cloudflare.kotoba-oracle-gen")))

(deftest gen-compile-kir-roundtrip
  (let [kir (gen/compile-kir "kotoba/client_core.kotoba")]
    (is (map? kir))
    (is (= "https://api.cloudflare.com/client/v4"
           (ir/execute kir 'api-base [])))))
