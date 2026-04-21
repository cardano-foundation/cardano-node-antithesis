#!/usr/bin/env bb

;; Stream the tenant's kv_table/runs SSE endpoint and emit run records.
;;
;; Usage:
;;   pangolin-runs.bb                   # JSONL: one run per line
;;   pangolin-runs.bb --session-ids     # table: submission_time session_id status findings_new
;;   pangolin-runs.bb --latest-completed   # single session_id of most recent Completed run
;;   pangolin-runs.bb --by-commit <sha>    # JSONL filtered to runs mentioning <sha>
;;
;; Env:
;;   ANTITHESIS_COOKIE_FILE   defaults to /tmp/antithesis-cookie.txt
;;   ANTITHESIS_TENANT        defaults to cardano
;;
;; Why the magic headers: /api/pangolin/v1/WYATT/kv_table/runs returns 403
;; unless the request carries all three of X-Antithesis-Allow-Effects: 1,
;; a browser User-Agent, and content-type: application/json.

(require '[babashka.process :refer [shell]]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def tenant (or (System/getenv "ANTITHESIS_TENANT") "cardano"))
(def cookie-file (or (System/getenv "ANTITHESIS_COOKIE_FILE") "/tmp/antithesis-cookie.txt"))
(def user-agent
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/147.0.0.0 Safari/537.36")

(defn die [msg & {:keys [code] :or {code 1}}]
  (binding [*out* *err*] (println msg))
  (System/exit code))

(defn load-cookie []
  (let [f (java.io.File. cookie-file)]
    (when-not (.canRead f)
      (die (str "error: " cookie-file " missing — run set-cookie.sh")))
    (str/trim (slurp f))))

(defn fetch-runs []
  ;; The SSE endpoint streams indefinitely; curl exits 28 when --max-time
  ;; trips. That's expected and the buffered body is still usable, so we
  ;; only fail if stdout is empty.
  (let [cookie (load-cookie)
        res (shell {:out :string :err :string :continue true}
                   "curl" "-sN" "--max-time" "15"
                   "-b" (str "__Host-antithesis_sso_session=" cookie)
                   "-H" "X-Antithesis-Allow-Effects: 1"
                   "-H" "Cache-Control: no-store"
                   "-H" "Referer;"
                   "-H" (str "User-Agent: " user-agent)
                   "-H" "content-type: application/json"
                   (str "https://" tenant ".antithesis.com/api/pangolin/v1/WYATT/kv_table/runs"))]
    (when (str/blank? (:out res))
      (die (str "curl returned empty body (exit=" (:exit res) " err=" (:err res) ")")))
    (:out res)))

(defn parse-sse [sse-text]
  ;; Each frame is "data:{...}\n". Body is {Initial|Added: {key,value}}.
  (->> (str/split-lines sse-text)
       (filter #(str/starts-with? % "data:"))
       (map #(subs % 5))
       (keep (fn [line]
               (try
                 (let [obj (json/parse-string line true)]
                   (or (:Initial obj) (:Added obj)))
                 (catch Exception _ nil))))))

(defn emit-session-ids [runs]
  (let [rows (->> runs
                  (map (fn [{:keys [value]}]
                         [(or (:submission_time value) "")
                          (or (:session_id value) "")
                          (or (:status value) "")
                          (or (:findings_new value) 0)]))
                  (sort-by first #(compare %2 %1)))]
    (printf "%-22s  %-38s  %-20s  %s%n"
            "submission_time" "session_id" "status" "findings_new")
    (doseq [[t sid st fn_] rows]
      (printf "%-22s  %-38s  %-20s  %s%n" t sid st fn_))))

(def session-id-pattern #"^[0-9a-f]{32}-\d+-\d+$")

(defn testnet-run? [run]
  ;; Webhook aggregator runs (e.g. tenant_data_aggregator.nb2) carry
  ;; synthetic session_ids like "OTIS" that won't open in Logs Explorer.
  ;; Real testnet runs have a <hex32>-<major>-<minor> session_id.
  (some->> (get-in run [:value :session_id])
           (re-matches session-id-pattern)))

(defn latest-completed-sid [runs]
  (->> runs
       (filter testnet-run?)
       (filter #(= "Completed" (get-in % [:value :status])))
       (sort-by #(get-in % [:value :submission_time]) #(compare %2 %1))
       first
       :value
       :session_id))

(defn by-commit [runs sha]
  ;; A run references its commit in params["antithesis.commit_id"] or
  ;; params["antithesis.description"] depending on launcher.
  (filter (fn [{:keys [value]}]
            (let [params (:params value)
                  cid    (str (get params (keyword "antithesis.commit_id")) (get params "antithesis.commit_id"))
                  desc   (str (get params (keyword "antithesis.description")) (get params "antithesis.description"))]
              (or (str/starts-with? (or cid "") sha)
                  (str/includes?    (or desc "") sha))))
          runs))

(let [args *command-line-args*
      runs (parse-sse (fetch-runs))]
  (case (first args)
    "--session-ids"
    (emit-session-ids runs)

    "--latest-completed"
    (if-let [sid (latest-completed-sid runs)]
      (println sid)
      (die "no completed run found" :code 3))

    "--by-commit"
    (let [sha (second args)]
      (when (str/blank? sha) (die "usage: pangolin-runs.bb --by-commit <sha>" :code 2))
      (doseq [r (by-commit runs sha)]
        (println (json/generate-string r))))

    ;; default: JSONL
    (doseq [r runs]
      (println (json/generate-string r)))))
