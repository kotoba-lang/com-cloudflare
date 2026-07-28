(ns cloudflare.deploy-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cloudflare.deploy :as deploy]))

(deftest validate-names
  (is (nil? (deploy/validate-account-id "abc123")))
  (is (= :deploy/empty-account (deploy/validate-account-id "")))
  (is (= :deploy/bad-account (deploy/validate-account-id "a/b")))
  (is (nil? (deploy/validate-script-name "local-murakumo")))
  (is (= :deploy/bad-script (deploy/validate-script-name "-bad")))
  (is (= :deploy/empty-script (deploy/validate-script-name ""))))

(deftest pure-paths-and-plans
  (is (= "/accounts/acct1/workers/scripts/my-worker"
         (deploy/workers-script-path "acct1" "my-worker")))
  (is (= "/accounts/acct1/pages/projects/site/deployments"
         (deploy/pages-deployments-path "acct1" "site")))
  (let [plan (deploy/workers-script-put-plan "acct1" "my-worker" "export default {}")]
    (is (= :put (:method plan)))
    (is (= "application/javascript" (:content-type plan)))
    (is (= "export default {}" (:body plan)))
    (is (str/includes? (:path plan) "/workers/scripts/my-worker")))
  (is (= :delete (:method (deploy/workers-script-delete-plan "acct1" "my-worker"))))
  (is (thrown-with-msg? #?(:clj Exception :cljs js/Error) #"validation failed"
        (deploy/workers-script-path "acct1" "../x"))))

(deftest wrangler-pages-deploy-argv-pure
  (is (= ["wrangler" "pages" "deploy" "./dist" "--project-name" "site"]
         (deploy/wrangler-pages-deploy-argv "site" "./dist")))
  (is (= ["/usr/local/bin/wrangler" "pages" "deploy" "./dist" "--project-name" "site"]
         (deploy/wrangler-pages-deploy-argv "site" "./dist" "/usr/local/bin/wrangler")))
  (is (thrown? #?(:clj Exception :cljs js/Error)
        (deploy/wrangler-pages-deploy-argv "bad name" "./dist"))))

#?(:clj
(deftest put-worker-script-uses-put-and-js-content-type
  (let [captured (atom nil)
        http-fn (fn [req]
                  (reset! captured req)
                  {:status 200 :body "{\"success\":true,\"result\":{\"id\":\"s1\"}}"})]
    (is (= {:id "s1"}
           (deploy/put-worker-script! "acct1" "my-worker" "console.log(1)"
                                      {:http-fn http-fn :token "t"})))
    (is (= :put (:method @captured)))
    (is (str/includes? (:url @captured) "/workers/scripts/my-worker"))
    (is (= "application/javascript" (get (:headers @captured) "Content-Type")))
    (is (= "console.log(1)" (:body @captured))))))

#?(:clj
(deftest delete-worker-script-uses-delete
  (let [captured (atom nil)
        http-fn (fn [req]
                  (reset! captured req)
                  {:status 200 :body "{\"success\":true,\"result\":null}"})]
    (deploy/delete-worker-script! "acct1" "my-worker" {:http-fn http-fn :token "t"})
    (is (= :delete (:method @captured))))))
