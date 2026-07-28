;; W6 cloud-deploy oracle: cloudflare.deploy validators/paths
;; vs kotoba/deploy_core.kotoba.

(ns cloudflare.deploy-kotoba-parity-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cloudflare.deploy :as deploy]
            [kotoba.compiler.core :as compiler]
            [kotoba.kir :as ir]))

(def port-source (slurp "kotoba/deploy_core.kotoba"))

(def export-prefix
  (str "max-account-id max-script-name max-script-bytes blank? ws? "
       "alnum-char? account-char? account-body-ok? script-body-ok? "
       "validate-account-id validate-script-name validate-project-name "
       "put-content-type workers-script-path pages-project-path "
       "pages-deployments-path put-method delete-method "
       "script-body-ok-size? wrangler-pages-deploy-cmd directory-ok?"))

(defn- kotoba-literal [s]
  (str \" (-> (str s) (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")) \"))

(defn- compile-string-cases [cases]
  (let [defs (for [[name body] cases]
               (str "(defn " name " [] :string " body ")"))
        names (map first cases)
        src (-> port-source
                (str/replace-first
                 #"\(:export \[[^\]]+\]\)"
                 (str "(:export [" export-prefix " " (str/join " " names) "])"))
                (str "\n" (str/join "\n" defs)))
        kir (:kir (compiler/compile-source src :wasm32-kotoba-v1 {}))]
    (into {} (map (fn [n] [n (ir/execute kir (symbol n) [])]) names))))

(defn- compile-i64-cases [cases]
  (let [defs (for [[name body] cases]
               (str "(defn " name " [] :i64 " body ")"))
        names (map first cases)
        src (-> port-source
                (str/replace-first
                 #"\(:export \[[^\]]+\]\)"
                 (str "(:export [" export-prefix " " (str/join " " names) "])"))
                (str "\n" (str/join "\n" defs)))
        kir (:kir (compiler/compile-source src :wasm32-kotoba-v1 {}))]
    (into {} (map (fn [n] [n (ir/execute kir (symbol n) [])]) names))))

(defn- err-tag [kw-or-nil]
  (if (nil? kw-or-nil)
    ""
    (name kw-or-nil)))

(deftest constants-match-deploy
  (let [actual (compile-i64-cases
                {"ma" "(max-account-id)"
                 "ms" "(max-script-name)"
                 "mb" "(max-script-bytes)"})
        s (compile-string-cases
           {"ct" "(put-content-type)"
            "pm" "(put-method)"
            "dm" "(delete-method)"})]
    (is (= deploy/max-account-id (get actual "ma")))
    (is (= deploy/max-script-name (get actual "ms")))
    (is (= deploy/max-script-bytes (get actual "mb")))
    (is (= "application/javascript" (get s "ct")))
    (is (= "put" (get s "pm")))
    (is (= "delete" (get s "dm")))))

(deftest validate-account-id-matches-deploy
  (let [corpus ["" "   " "\t"
                "abc123" "A_b-9"
                "a/b" "has space" "weird!"
                (apply str (repeat 65 "a"))]
        cases (into {} (map-indexed
                        (fn [i s]
                          [(str "va_" i)
                           (str "(validate-account-id " (kotoba-literal s) ")")])
                        corpus))
        actual (compile-string-cases cases)]
    (doseq [[i s] (map-indexed vector corpus)]
      (testing (pr-str s)
        (is (= (err-tag (deploy/validate-account-id s))
               (get actual (str "va_" i))))))))

(deftest validate-script-name-matches-deploy
  (let [corpus ["" "   "
                "local-murakumo" "Worker1" "a" "a_b-c"
                "-bad" "_bad" "has space" "bad!"
                (apply str (repeat 65 "a"))]
        cases (into {} (map-indexed
                        (fn [i s]
                          [(str "vs_" i)
                           (str "(validate-script-name " (kotoba-literal s) ")")])
                        corpus))
        actual (compile-string-cases cases)]
    (doseq [[i s] (map-indexed vector corpus)]
      (testing (pr-str s)
        (is (= (err-tag (deploy/validate-script-name s))
               (get actual (str "vs_" i))))))
    (testing "project-name shares script policy"
      (let [p (compile-string-cases
               {"ok" (str "(validate-project-name " (kotoba-literal "site") ")")
                "bad" (str "(validate-project-name " (kotoba-literal "bad name") ")")})]
        (is (= (err-tag (deploy/validate-project-name "site")) (get p "ok")))
        (is (= (err-tag (deploy/validate-project-name "bad name")) (get p "bad")))))))

(deftest path-builders-match-deploy
  (let [acct "acct1"
        script "my-worker"
        project "site"
        actual (compile-string-cases
                {"wsp" (str "(workers-script-path " (kotoba-literal acct) " "
                            (kotoba-literal script) ")")
                 "ppp" (str "(pages-project-path " (kotoba-literal acct) " "
                            (kotoba-literal project) ")")
                 "pdp" (str "(pages-deployments-path " (kotoba-literal acct) " "
                            (kotoba-literal project) ")")})]
    (is (= (deploy/workers-script-path acct script) (get actual "wsp")))
    (is (= (deploy/pages-project-path acct project) (get actual "ppp")))
    (is (= (deploy/pages-deployments-path acct project) (get actual "pdp")))
    (testing "put-plan path/content-type surface"
      (let [plan (deploy/workers-script-put-plan acct script "export default {}")]
        (is (= (get actual "wsp") (:path plan)))
        (is (= "application/javascript" (:content-type plan)))
        (is (= :put (:method plan))))
      (is (= :delete (:method (deploy/workers-script-delete-plan acct script)))))))

(deftest wrangler-cmd-matches-argv
  (let [join #(str/join " " %)
        actual (compile-string-cases
                {"def" (str "(wrangler-pages-deploy-cmd "
                            (kotoba-literal "site") " "
                            (kotoba-literal "./dist") " \"\")")
                 "abs" (str "(wrangler-pages-deploy-cmd "
                            (kotoba-literal "site") " "
                            (kotoba-literal "./dist") " "
                            (kotoba-literal "/usr/local/bin/wrangler") ")")})
        dir (compile-i64-cases
             {"ok" (str "(directory-ok? " (kotoba-literal "./dist") ")")
              "blank" (str "(directory-ok? " (kotoba-literal "") ")")
              "ws" (str "(directory-ok? " (kotoba-literal "  ") ")")})
        size (compile-i64-cases
              {"small" "(script-body-ok-size? 100)"
               "exact" (str "(script-body-ok-size? " deploy/max-script-bytes ")")
               "over" (str "(script-body-ok-size? " (inc deploy/max-script-bytes) ")")})]
    (is (= (join (deploy/wrangler-pages-deploy-argv "site" "./dist"))
           (get actual "def")))
    (is (= (join (deploy/wrangler-pages-deploy-argv "site" "./dist" "/usr/local/bin/wrangler"))
           (get actual "abs")))
    (is (= 1 (get dir "ok")))
    (is (= 0 (get dir "blank")))
    (is (= 0 (get dir "ws")))
    (is (= 1 (get size "small")))
    (is (= 1 (get size "exact")))
    (is (= 0 (get size "over")))))
