(ns carve.main
  (:require
   [carve.impl :as impl]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.spec.alpha :as s]
   [expound.alpha :as expound]))

(set! *warn-on-reflection* true)
(set! s/*explain-out* expound/printer)
(s/check-asserts true)

(s/def ::paths (s/coll-of string?))
(s/def ::ignore-vars (s/coll-of symbol?))
(s/def ::api-namespaces (s/coll-of symbol?))
(s/def ::carve-ignore-file string?)
(s/def ::interactive boolean?)
(s/def ::interactive? boolean?)                             ;; deprecated
(s/def ::dry-run boolean?)
(s/def ::dry-run? boolean?)                                 ;; deprecated
(s/def ::format #{:edn :text})
(s/def ::aggressive boolean?)
(s/def ::aggressive? boolean?)                              ;; deprecated
(s/def ::out-dir string?)
(s/def ::report-format (s/keys :req-un [::format]))
(s/def ::report (s/or :bool boolean? :map ::report-format))
(s/def ::silent boolean?)

(s/def ::opts (s/keys :req-un [::paths]
                      :opt-un [::ignore-vars
                               ::api-namespaces
                               ::carve-ignore-file
                               ::interactive
                               ::interactive?
                               ::out-dir
                               ::dry-run
                               ::dry-run?
                               ::aggressive
                               ::aggressive?
                               ::report
                               ::silent]))

(defn- valid-path?
  [p]
  (.exists (io/file p)))

(defn validate-opts!
  "Validate options throwing an exception if they don't validate"
  [{:keys [paths] :as opts}]
  (s/assert ::opts opts)
  (when-not (every? valid-path? paths)
    (throw (ex-info "Path not found" {:paths paths})))
  opts)

(defn main
  [opts]
  (let [start  (. System (nanoTime))
        opts   (validate-opts! opts)
        format (-> opts :report :format)
        report (impl/run! opts)
        took   (long (/ (double (- (. System (nanoTime)) start)) 1000000.0))]
    (when format
      (impl/print-report report format))
    (when (seq report)
      (println "Took:" took "msecs" "-" (count report) "unused vars found."))
    (if (empty? report) 0 1)))

(defn carve
  [options]
  (System/exit (main options)))
