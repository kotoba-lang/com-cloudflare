;; W6 analytics parse oracle: tally/sum core vs parse-path-report /
;; parse-daily-report pure folds (cloudflare.analytics).

(ns cloudflare.analytics-parse-kotoba-parity-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [cloudflare.analytics :as analytics]
            [cloudflare.client :as client]
            [kotoba.compiler.core :as compiler]
            [kotoba.kir :as ir]))

(def port-source (slurp "kotoba/analytics_parse_core.kotoba"))

(def export-prefix
  (str "empty-string-tally empty-i64-tally "
       "string-tally-get string-tally-add "
       "i64-tally-get i64-tally-add "
       "sum2 sum3 sum4 report-ok?"))

(defn- kotoba-literal [s]
  (str \" (-> (str s) (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")) \"))

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

(defn- stub-http-fn [body]
  (fn [_req] {:status 200 :body body}))

(def path-fixture-body
  "{\"data\":{\"viewer\":{\"zones\":[{\"httpRequestsAdaptiveGroups\":[
   {\"count\":22,\"dimensions\":{\"clientRequestPath\":\"/a\",\"clientDeviceType\":\"desktop\",\"clientCountryName\":\"JP\",\"edgeResponseStatus\":200}},
   {\"count\":3,\"dimensions\":{\"clientRequestPath\":\"/b\",\"clientDeviceType\":\"desktop\",\"clientCountryName\":\"JP\",\"edgeResponseStatus\":201}},
   {\"count\":25,\"dimensions\":{\"clientRequestPath\":\"/c\",\"clientDeviceType\":\"mobile\",\"clientCountryName\":\"CH\",\"edgeResponseStatus\":404}}
   ]}]}},\"errors\":null}")

(def daily-fixture-body
  "{\"data\":{\"viewer\":{\"zones\":[{\"httpRequests1dGroups\":[
   {\"sum\":{\"requests\":842,\"pageViews\":178,\"bytes\":1487599},\"uniq\":{\"uniques\":44},\"dimensions\":{\"date\":\"2026-07-03\"}},
   {\"sum\":{\"requests\":6,\"pageViews\":0,\"bytes\":2986},\"uniq\":{\"uniques\":3},\"dimensions\":{\"date\":\"2026-06-28\"}}
   ]}]}},\"errors\":null}")

(deftest report-ok-flag
  (let [actual (compile-i64-cases
                {"ok" "(report-ok? 0)"
                 "err" "(report-ok? 1)"})]
    (is (= 1 (get actual "ok")))
    (is (= 0 (get actual "err")))))

(deftest path-report-tallies-match-fixture
  (let [response (client/graphql! {} {:http-fn (stub-http-fn path-fixture-body) :token "t"})
        report (analytics/parse-path-report response)
        rows (analytics/path-report-rows response)
        ;; Host projects each row into sequential string-tally-add / i64-tally-add
        path-fold (reduce (fn [expr r]
                            (str "(string-tally-add " expr " "
                                 (kotoba-literal (:path r)) " " (:count r) ")"))
                          "(empty-string-tally)"
                          rows)
        device-fold (reduce (fn [expr r]
                              (str "(string-tally-add " expr " "
                                   (kotoba-literal (:device r)) " " (:count r) ")"))
                            "(empty-string-tally)"
                            rows)
        country-fold (reduce (fn [expr r]
                               (str "(string-tally-add " expr " "
                                    (kotoba-literal (:country r)) " " (:count r) ")"))
                             "(empty-string-tally)"
                             rows)
        status-fold (reduce (fn [expr r]
                              (str "(i64-tally-add " expr " "
                                   (:status r) " " (:count r) ")"))
                            "(empty-i64-tally)"
                            rows)
        total-expr (str "(sum3 " (str/join " " (map :count rows)) ")")
        actual (compile-i64-cases
                {"pa" (str "(string-tally-get " path-fold " " (kotoba-literal "/a") ")")
                 "pb" (str "(string-tally-get " path-fold " " (kotoba-literal "/b") ")")
                 "pc" (str "(string-tally-get " path-fold " " (kotoba-literal "/c") ")")
                 "dd" (str "(string-tally-get " device-fold " " (kotoba-literal "desktop") ")")
                 "dm" (str "(string-tally-get " device-fold " " (kotoba-literal "mobile") ")")
                 "jp" (str "(string-tally-get " country-fold " " (kotoba-literal "JP") ")")
                 "ch" (str "(string-tally-get " country-fold " " (kotoba-literal "CH") ")")
                 "s200" (str "(i64-tally-get " status-fold " 200)")
                 "s201" (str "(i64-tally-get " status-fold " 201)")
                 "s404" (str "(i64-tally-get " status-fold " 404)")
                 "tot" total-expr
                 "miss" (str "(string-tally-get " path-fold " " (kotoba-literal "/z") ")")})]
    (is (true? (:ok? report)))
    (is (= 22 (get actual "pa") (get (:by-path report) "/a")))
    (is (= 3 (get actual "pb") (get (:by-path report) "/b")))
    (is (= 25 (get actual "pc") (get (:by-path report) "/c")))
    (is (= 25 (get actual "dd") (get (:by-device report) "desktop")))
    (is (= 25 (get actual "dm") (get (:by-device report) "mobile")))
    (is (= 25 (get actual "jp") (get (:by-country report) "JP")))
    (is (= 25 (get actual "ch") (get (:by-country report) "CH")))
    (is (= 22 (get actual "s200") (get (:by-status report) 200)))
    (is (= 3 (get actual "s201") (get (:by-status report) 201)))
    (is (= 25 (get actual "s404") (get (:by-status report) 404)))
    (is (= 50 (get actual "tot") (:total report)))
    (is (= 0 (get actual "miss")))))

(deftest daily-report-totals-match-fixture
  (let [response (client/graphql! {} {:http-fn (stub-http-fn daily-fixture-body) :token "t"})
        report (analytics/parse-daily-report response)
        days (:days report)
        reqs (mapv :requests days)
        pvs (mapv :page-views days)
        unqs (mapv :uniques days)
        bytes (mapv :bytes days)
        actual (compile-i64-cases
                {"req" (str "(sum2 " (str/join " " reqs) ")")
                 "pv" (str "(sum2 " (str/join " " pvs) ")")
                 "unq" (str "(sum2 " (str/join " " unqs) ")")
                 "by" (str "(sum2 " (str/join " " bytes) ")")})]
    (is (true? (:ok? report)))
    (is (= (:requests (:totals report)) (get actual "req")))
    (is (= (:page-views (:totals report)) (get actual "pv")))
    (is (= (:uniques (:totals report)) (get actual "unq")))
    (is (= (:bytes (:totals report)) (get actual "by")))
    (is (= 848 (get actual "req")))
    (is (= 178 (get actual "pv")))
    (is (= 47 (get actual "unq")))))

(deftest sum-helpers
  (let [actual (compile-i64-cases
                {"s2" "(sum2 1 2)"
                 "s3" "(sum3 1 2 3)"
                 "s4" "(sum4 1 2 3 4)"})]
    (is (= 3 (get actual "s2")))
    (is (= 6 (get actual "s3")))
    (is (= 10 (get actual "s4")))))
