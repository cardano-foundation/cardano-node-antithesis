#!/usr/bin/env bb

;; One-shot Antithesis Logs Explorer query: resolve the run, fire the
;; search, parse the rendered body, emit a structured summary.
;;
;; Usage:
;;   query.bb [selector] [options] <needle>
;;
;; Run selector (default --latest):
;;   --latest                most recent Completed run on this tenant
;;   --commit <sha>          run matching <sha> in params
;;   --session <id>          use this session_id directly
;;
;; Query options:
;;   --op contains|equals|regex   (default contains)
;;   --source <name>              filter events to source=<name>
;;   --count-only                 emit just the match count
;;   --raw                        emit the raw body dump (no parsing)
;;   --wait-seconds N             max seconds to wait (default 30)

(require '[babashka.process :refer [shell]]
         '[clojure.string :as str])

(def here (-> *file* java.io.File. .getAbsoluteFile .getParent))

(defn die [msg & {:keys [code] :or {code 1}}]
  (binding [*out* *err*] (println msg))
  (System/exit code))

(defn parse-args [argv]
  (loop [args argv
         out  {:mode :latest :forward [] :count-only? false :raw? false
               :from-dump nil :commit nil :session nil :needle nil}]
    (if (empty? args)
      out
      (case (first args)
        "--latest"       (recur (rest args) (assoc out :mode :latest))
        "--commit"       (recur (drop 2 args) (assoc out :mode :commit :commit (second args)))
        "--session"      (recur (drop 2 args) (assoc out :mode :session :session (second args)))
        "--op"           (recur (drop 2 args) (update out :forward into ["--op" (second args)]))
        "--source"       (recur (drop 2 args) (update out :forward into ["--source" (second args)]))
        "--wait-seconds" (recur (drop 2 args) (update out :forward into ["--wait-seconds" (second args)]))
        "--count-only"   (recur (rest args) (assoc out :count-only? true))
        "--raw"          (recur (rest args) (assoc out :raw? true))
        "--from-dump"    (recur (drop 2 args) (assoc out :from-dump (second args)))
        ("-h" "--help")  (do (shell "sed" "-n" "2,22p" *file*) (System/exit 0))
        (if (str/starts-with? (first args) "--")
          (die (str "unknown flag: " (first args)) :code 2)
          (recur (rest args) (assoc out :needle (first args))))))))

(defn resolve-session-id [{:keys [mode commit session]}]
  (let [runs (str here "/pangolin-runs.bb")]
    (case mode
      :session session
      :latest  (let [{:keys [out exit err]}
                     (shell {:out :string :err :string :continue true} runs "--latest-completed")]
                 (when-not (zero? exit) (die (str "resolver failed: " err) :code 3))
                 (str/trim out))
      :commit  (let [{:keys [out exit err]}
                     (shell {:out :string :err :string :continue true} runs "--by-commit" commit)]
                 (when-not (zero? exit) (die (str "resolver failed: " err) :code 3))
                 ;; pick newest match
                 (->> (str/split-lines out)
                      (remove str/blank?)
                      (map #(cheshire.core/parse-string % true))
                      (sort-by #(get-in % [:value :submission_time]) #(compare %2 %1))
                      first
                      (#(get-in % [:value :session_id])))))))

(defn run-search [sid needle forward]
  (let [res (apply shell {:out :string :err :string :continue true}
                   (str here "/open-search.bb") sid needle forward)]
    (when-not (zero? (:exit res))
      (die (str "open-search failed: " (:err res)) :code 4))
    (:out res)))

(defn extract-body [dump]
  (or (second (re-find #"(?s)=== Body ===\n(.*)" dump)) ""))

(defn extract-count [body]
  (when-let [m (re-find #"([0-9,]+) matching events" body)]
    (Long/parseLong (str/replace (second m) #"," ""))))

(defn parse-events
  "Each rendered row is 4 contiguous lines: <vtime> <source> <host> <json>.
   The JSON payload can wrap across lines; we greedily accumulate until the
   next decimal virtual-time line."
  [body]
  (let [lines (->> (str/split-lines body)
                   (remove #(re-matches #"^\s*$" %))
                   (remove #(#{"Search results" "Share search" "List" "Map"
                               "Download logs" "Explore your logs"} %))
                   (remove #(re-matches #".*matching events$" %))
                   (remove #(re-matches #"Loading results.*" %)))
        vtime? (fn [s] (re-matches #"^\d+\.\d+$" s))]
    (loop [ls lines state :vtime row {} out []]
      (if (empty? ls)
        (if (and (= state :json) (seq (:json row))) (conj out row) out)
        (let [l (first ls)]
          (case state
            :vtime  (if (vtime? l)
                      (recur (rest ls) :source {:vtime l :json ""} out)
                      (recur (rest ls) :vtime row out))
            :source (recur (rest ls) :host (assoc row :source l) out)
            :host   (recur (rest ls) :json (assoc row :host l) out)
            :json   (if (vtime? l)
                      (recur ls :vtime {} (conj out row))
                      (recur (rest ls) :json (update row :json str l) out))))))))

(defn extract-ns-sev [json-line]
  {:ns  (second (re-find #"\"ns\":\"([^\"]+)\"" json-line))
   :sev (second (re-find #"\"sev\":\"([^\"]+)\"" json-line))})

(defn summarise [sid needle match-count events]
  (let [enriched (map #(merge % (extract-ns-sev (:json %))) events)
        unique   (->> enriched
                      (map #(select-keys % [:vtime :source :host :ns :sev]))
                      distinct)
        by-src   (->> unique
                      (group-by :source)
                      (map (fn [[k vs]] [k (count vs)]))
                      (sort-by second >))
        by-triple (->> unique
                       (group-by (juxt :source :ns :sev))
                       (map (fn [[k vs]] [k (count vs)]))
                       (sort-by second >)
                       (take 30))]
    (println "session_id:" sid)
    (println "needle:    " needle)
    (println "matches:   " (or match-count "?"))
    (println "distinct rows rendered:" (count unique))
    (println)
    (println "By source:")
    (doseq [[s n] by-src] (printf "  %5d  %s%n" n (or s "")))
    (println)
    (println "By (source, ns, sev):")
    (doseq [[[s ns sev] n] by-triple]
      (printf "  %5d  %-14s  %-40s  %s%n" n (or s "") (or ns "") (or sev "")))
    (println)
    (println "First 10 distinct events:")
    (doseq [{:keys [vtime source host ns sev]} (take 10 unique)]
      (printf "  %-8s  %-14s  %-14s  %-40s  %s%n"
              (or vtime "") (or source "") (or host "") (or ns "") (or sev "")))))

(let [{:keys [count-only? raw? forward needle from-dump] :as opts} (parse-args *command-line-args*)]
  (when (str/blank? needle)
    (die "error: needle required. See --help." :code 2))
  (let [sid (if from-dump
              (or (:session opts) "<from-dump>")
              (resolve-session-id opts))]
    (when (str/blank? sid) (die "error: could not resolve session_id" :code 3))
    (let [dump (if from-dump (slurp from-dump) (run-search sid needle forward))]
      (cond
        raw? (println dump)
        :else
        (let [body (if from-dump dump (extract-body dump))
              cnt  (extract-count body)]
          (if count-only?
            (println (or cnt 0))
            (summarise sid needle cnt (parse-events body))))))))
