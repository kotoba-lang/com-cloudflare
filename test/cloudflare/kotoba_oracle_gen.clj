;; cloudflare.kotoba-oracle-gen — regenerate precompiled KIR product-shell artifacts.
;;
;; Run under :test (needs kotoba.compiler):
;;   clojure -M:test -m cloudflare.kotoba-oracle-gen
;;
;; Discovers every kotoba/*_core.kotoba and writes
;; resources/cloudflare/oracle/<name>.kir.edn using the same compile-source
;; path as parity tests. No new runtime.

(ns cloudflare.kotoba-oracle-gen
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [kotoba.compiler.core :as compiler])
  (:gen-class))

(defn discover-artifacts
  "Every kotoba/*_core.kotoba → resources/cloudflare/oracle/*.kir.edn."
  []
  (->> (file-seq (io/file "kotoba"))
       (filter #(and (.isFile %) (str/ends-with? (.getName %) "_core.kotoba")))
       (sort-by #(.getName %))
       (mapv (fn [f]
               (let [base (str/replace (.getName f) #".kotoba$" "")]
                 {"source" (.getPath f)
                  "out" (str "resources/cloudflare/oracle/" base ".kir.edn")})))))

(def artifacts
  "Lazy discovery at load; used by regenerate-all! and tests."
  (discover-artifacts))

(defn compile-kir
  "Compile one .kotoba file to a KIR map (same path as parity tests)."
  [source-path]
  (let [src (slurp source-path)
        r (compiler/compile-source src :wasm32-kotoba-v1 {})]
    (or (:kir r)
        (throw (ex-info "compile-source returned no :kir" {:source source-path})))))

(defn write-artifact!
  "Compile source and pretty-print KIR EDN to out-path. Returns out-path."
  [{:strs [source out]}]
  (let [kir (compile-kir source)
        f (io/file out)]
    (io/make-parents f)
    (spit f (with-out-str (pp/pprint kir)))
    out))

(defn regenerate-all!
  "Regenerate every product-shell oracle artifact. Returns written paths."
  []
  (mapv write-artifact! (discover-artifacts)))

(defn -main [& _]
  (doseq [p (regenerate-all!)]
    (println "wrote" p))
  (System/exit 0))
