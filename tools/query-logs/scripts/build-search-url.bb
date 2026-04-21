#!/usr/bin/env bb

;; Build a Logs Explorer /search URL for a given run session_id and needle.
;;
;; Usage:
;;   build-search-url.bb <session_id> <needle> [--op contains|equals|regex]
;;                                             [--source <name>]
;;
;; The session_id is the Logs Explorer "s" identifier (format
;; <hex32>-<major>-<minor>), NOT the testRunId.

(require '[cheshire.core :as json]
         '[clojure.string :as str])

(def tenant (or (System/getenv "ANTITHESIS_TENANT") "cardano"))

(defn parse-args [argv]
  (loop [args argv
         out  {:op "contains" :source nil :positional []}]
    (if (empty? args)
      out
      (case (first args)
        "--op"     (recur (drop 2 args) (assoc out :op (second args)))
        "--source" (recur (drop 2 args) (assoc out :source (second args)))
        (recur (rest args)
               (update out :positional conj (first args)))))))

(defn b64url-nopad [s]
  (-> (.encodeToString (java.util.Base64/getUrlEncoder) (.getBytes s "UTF-8"))
      (str/replace #"=+$" "")))

(defn build-query-json
  "Each leaf goes into its own inner group (h[].h[] of length 1, inner o=\"or\"),
   and the groups are joined by the outer AND. The Monaco query builder
   serialises to this shape; collapsing both leaves into a single inner group
   is rejected silently (the SPA falls back to the empty builder)."
  [{:keys [sid needle op source]}]
  (let [needle-group {:h [{:c false :f "general.output_text" :o op :v needle}] :o "or"}
        source-group (when source
                       {:h [{:c false :f "general.source" :o "equals" :v source}] :o "or"})
        groups       (cond-> [needle-group]
                       source-group (conj source-group))]
    {:q {:n {:r {:h groups :o "and"}
             :t {:g false :m ""}
             :y "none"}}
     :s sid}))

(let [{:keys [op source positional]} (parse-args *command-line-args*)
      [sid needle] positional]
  (when (or (nil? sid) (nil? needle))
    (binding [*out* *err*]
      (println "usage: build-search-url.bb <session_id> <needle> [--op OP] [--source NAME]"))
    (System/exit 2))
  (let [json-str (json/generate-string (build-query-json {:sid sid :needle needle :op op :source source}))
        b64 (b64url-nopad json-str)]
    (println (str "https://" tenant ".antithesis.com/search?search=v5v" b64
                  "&report_name=&get_logs=false&get_logs_event_desc="))))
