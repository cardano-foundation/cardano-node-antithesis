#!/usr/bin/env bb

;; Drive the Logs Explorer /search page through a CDP-attached chromium and
;; dump the rendered results.
;;
;; Usage:
;;   open-search.bb <session_id> <needle> [--op OP] [--source NAME]
;;                                        [--wait-seconds N]

(require '[babashka.process :refer [shell process]]
         '[clojure.string :as str])

(def here (-> *file* java.io.File. .getAbsoluteFile .getParent))
(def tenant (or (System/getenv "ANTITHESIS_TENANT") "cardano"))
(def cookie-file (or (System/getenv "ANTITHESIS_COOKIE_FILE") "/tmp/antithesis-cookie.txt"))
(def profile (or (System/getenv "AGENT_BROWSER_PROFILE")
                 (str (System/getProperty "user.home") "/.cache/agent-browser-triage/profile")))
(def session (or (System/getenv "SESSION")
                 (str "antithesis-query-" (.pid (java.lang.ProcessHandle/current)))))

(defn die [msg & {:keys [code] :or {code 1}}]
  (binding [*out* *err*] (println msg))
  (System/exit code))

(defn parse-args [argv]
  (loop [args argv
         out  {:wait-seconds 30 :forward [] :positional []}]
    (if (empty? args)
      out
      (case (first args)
        "--wait-seconds" (recur (drop 2 args) (assoc out :wait-seconds (Long/parseLong (second args))))
        ("--op" "--source") (recur (drop 2 args) (update out :forward into [(first args) (second args)]))
        (recur (rest args) (update out :positional conj (first args)))))))

(defn npm-path-prepended []
  (str (System/getProperty "user.home") "/.local/npm-global/bin:" (System/getenv "PATH")))

(defn resolve-bin
  "Find an executable on the augmented PATH. Java's Runtime.exec resolves
   the program name against the PARENT process's PATH, ignoring :extra-env,
   so we have to hand it an absolute path."
  [bin]
  (let [res (shell {:out :string :err :string :continue true
                    :extra-env {"PATH" (npm-path-prepended)}}
                   "which" bin)]
    (when (zero? (:exit res))
      (str/trim (:out res)))))

(def agent-browser-bin (delay (or (resolve-bin "agent-browser")
                                  (die "error: agent-browser not in PATH"))))
(def chromium-bin      (delay (or (resolve-bin "chromium")
                                  (die "error: chromium not in PATH"))))

(defn build-url [sid needle forward]
  (let [res (apply shell {:out :string :err :string :continue true}
                   (str here "/build-search-url.bb") sid needle forward)]
    (when-not (zero? (:exit res))
      (die (str "build-search-url failed: " (:err res))))
    (str/trim (:out res))))

(defn wait-for-port [port attempts]
  (loop [n attempts]
    (let [res (shell {:out :string :err :string :continue true}
                     "curl" "-s" (str "http://localhost:" port "/json/version"))]
      (cond
        (zero? (:exit res)) true
        (zero? n) false
        :else (do (Thread/sleep 250) (recur (dec n)))))))

(defn ab [session-name & args]
  (apply shell {:out :string :err :string :continue true}
         @agent-browser-bin (concat args ["--session" session-name])))

(defn saw-results? [body]
  ;; The results pane has rendered — either a real count, or any of the
  ;; placeholder/loading phrases the SPA shows while the query runs.
  (and (not (str/blank? body))
       (or (re-find #"[0-9,]+ matching events" body)
           (re-find #"No matching events found" body)
           (re-find #"No results found" body)
           (re-find #"Initializing query" body)
           (re-find #"Loading results" body))))

(defn load-finished? [body]
  ;; A final state is either a concrete count or a definitive empty result.
  ;; "No matching events found yet" with "Initializing query..." still on
  ;; screen is NOT final — the SPA hasn't produced its first count yet.
  (and (not (str/blank? body))
       (not (re-find #"Initializing query" body))
       (not (re-find #"Loading results" body))
       (or (re-find #"[0-9,]+ matching events" body)
           (re-find #"No results found" body))))

(defn poll-body
  "Two-phase wait:
   (1) block until the results pane has rendered (spinner or count line
       visible), capped at wait-seconds/2,
   (2) then wait for 'Loading results' to disappear, capped at remaining
       budget. Returns the last body regardless of whether the deadline
       was hit, so the caller can still dump what's there for debugging."
  [wait-seconds]
  (let [deadline (+ (System/currentTimeMillis) (* wait-seconds 1000))
        phase1-deadline (+ (System/currentTimeMillis) (* (quot wait-seconds 2) 1000))]
    ;; Phase 1: wait for the results pane to appear.
    (loop [last-body ""]
      (let [res (ab session "get" "text" "body")
            body (if (zero? (:exit res)) (:out res) last-body)]
        (cond
          (saw-results? body)
          ;; Phase 2: wait for loading to finish.
          (loop [b body]
            (cond
              (load-finished? b) b
              (>= (System/currentTimeMillis) deadline) b
              :else
              (do (Thread/sleep 1000)
                  (let [r2 (ab session "get" "text" "body")]
                    (recur (if (zero? (:exit r2)) (:out r2) b))))))

          (>= (System/currentTimeMillis) phase1-deadline) body
          :else (do (Thread/sleep 1000) (recur body)))))))

(let [{:keys [wait-seconds forward positional]} (parse-args *command-line-args*)
      [sid needle] positional]
  (when (or (nil? sid) (nil? needle))
    (die "usage: open-search.bb <session_id> <needle> [--op OP] [--source NAME] [--wait-seconds N]" :code 2))
  (when-not (.canRead (java.io.File. cookie-file))
    (die (str "error: " cookie-file " missing — run set-cookie.sh")))
  @agent-browser-bin
  @chromium-bin

  (let [cookie (str/trim (slurp cookie-file))
        url    (build-url sid needle forward)
        port   (+ 9222 (rand-int 1000))]

    (.mkdirs (java.io.File. profile))
    ;; Clear stale SingletonLocks from prior runs.
    (doseq [f (.listFiles (java.io.File. profile))]
      (when (str/starts-with? (.getName f) "Singleton")
        (.delete f)))

    (let [log-file (java.io.File. "/tmp/chromium-query-logs.log")
          chrome  (process [@chromium-bin
                            (str "--remote-debugging-port=" port)
                            (str "--user-data-dir=" profile)
                            "--no-first-run" "--no-default-browser-check"
                            "--disable-gpu" "--headless=new" "about:blank"]
                           {:out log-file :err log-file})]
      (try
        (when-not (wait-for-port port 40)
          (die "chromium did not open CDP port"))

        (ab session "connect" (str port))
        (ab session "open" (str "https://" tenant ".antithesis.com/dashboard/assets/feature_tests.js"))
        (ab session "cookies" "set" "__Host-antithesis_sso_session" cookie
            "--url" (str "https://" tenant ".antithesis.com/")
            "--path" "/" "--secure" "--httpOnly" "--sameSite" "Lax")
        (ab session "open" url)

        ;; Give the query builder time to parse the URL, then click the primary
        ;; Search button — the play-icon one stays disabled. Sleep matches the
        ;; working shell version; 3s was too short for the SPA to materialise
        ;; the populated clauses before the click fired.
        (Thread/sleep 6000)
        (ab session "click" "a-button[variant=\"primary\"][icon=\"search\"]")
        (Thread/sleep 1000)

        (let [body (poll-body wait-seconds)]
          ;; Always dump the full body to /tmp so a failed run is inspectable
          ;; without having to rerun the query.
          (spit "/tmp/open-search-last.txt" body)
          (println "=== URL ===")
          (println url)
          (println)
          (println "=== Body ===")
          (let [m (re-find #"(?s)Search results.*?Explore your logs" body)]
            (println (or m body))))

        (finally
          (ab session "close")
          (.destroy (:proc chrome)))))))
