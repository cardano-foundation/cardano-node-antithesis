#!/usr/bin/env bb

;; One-time interactive login. Launches a real chromium window on the
;; persistent profile, lets you complete Google SSO by hand, then pulls
;; __Host-antithesis_sso_session out over CDP and writes it to the same
;; $ANTITHESIS_COOKIE_FILE set-cookie.sh uses. No DevTools, no paste.
;;
;; Usage:
;;
;;   scripts/login.bb
;;
;; Re-run whenever the cookie expires (~8–9h). The persistent profile
;; keeps Google session state between runs, so subsequent logins are
;; often a single click.
;;
;; If you invoke this from inside an agent harness that captures tool
;; stdout into a transcript, invoke it via the harness's shell-passthrough
;; prefix so the interactive Enter prompt reads from your terminal.

(require '[babashka.process :refer [shell process]]
         '[cheshire.core :as json]
         '[clojure.string :as str])

(def tenant (or (System/getenv "ANTITHESIS_TENANT") "cardano"))
(def cookie-file (or (System/getenv "ANTITHESIS_COOKIE_FILE") "/tmp/antithesis-cookie.txt"))
(def profile (or (System/getenv "AGENT_BROWSER_PROFILE")
                 (str (System/getProperty "user.home") "/.cache/agent-browser-triage/profile")))
(def session (str "antithesis-login-" (.pid (java.lang.ProcessHandle/current))))
(def cookie-name "__Host-antithesis_sso_session")

(defn die [msg & {:keys [code] :or {code 1}}]
  (binding [*out* *err*] (println msg))
  (System/exit code))

(defn npm-path-prepended []
  (str (System/getProperty "user.home") "/.local/npm-global/bin:" (System/getenv "PATH")))

(defn resolve-bin [bin]
  (let [res (shell {:out :string :err :string :continue true
                    :extra-env {"PATH" (npm-path-prepended)}}
                   "which" bin)]
    (when (zero? (:exit res)) (str/trim (:out res)))))

(def agent-browser-bin (delay (or (resolve-bin "agent-browser")
                                  (die "error: agent-browser not in PATH"))))
(def chromium-bin      (delay (or (resolve-bin "chromium")
                                  (die "error: chromium not in PATH"))))

(defn ab [& args]
  (apply shell {:out :string :err :string :continue true}
         @agent-browser-bin (concat args ["--session" session])))

(defn wait-for-port [port attempts]
  (loop [n attempts]
    (let [res (shell {:out :string :err :string :continue true}
                     "curl" "-s" (str "http://localhost:" port "/json/version"))]
      (cond
        (zero? (:exit res)) true
        (zero? n) false
        :else (do (Thread/sleep 250) (recur (dec n)))))))

(defn decode-paseto-payload
  "v2.public layout is 'v2.public.' + base64url(payload_json || ed25519_sig[64]).
   Returns the JSON string."
  [paseto]
  (let [b64   (subs paseto (count "v2.public."))
        pad   (mod (- 4 (mod (count b64) 4)) 4)
        b64p  (str b64 (apply str (repeat pad \=)))
        raw-b (.decode (java.util.Base64/getUrlDecoder) b64p)
        n     (alength raw-b)
        json-b (byte-array (take (- n 64) raw-b))]
    (String. json-b "UTF-8")))

(defn sanity-check [cookie]
  (let [root (shell {:out :string :err :string :continue true}
                    "curl" "-s" "-o" "/dev/null" "-w" "%{http_code}"
                    "-b" (str cookie-name "=" cookie)
                    (str "https://" tenant ".antithesis.com/"))
        home (shell {:out :string :err :string :continue true}
                    "curl" "-sL" "-o" "/dev/null" "-w" "%{http_code}"
                    "-b" (str cookie-name "=" cookie)
                    (str "https://" tenant ".antithesis.com/"))]
    [(:out root) (:out home)]))

(defn prompt-enter! []
  (binding [*out* *err*]
    (println)
    (println "A chromium window is open. Complete the Google login flow.")
    (println "When you see the tenant dashboard, come back here and press Enter.")
    (print "> ")
    (flush))
  (.readLine (java.io.BufferedReader. (java.io.InputStreamReader. System/in))))

;; ---- main ----
@agent-browser-bin
@chromium-bin

(when (and (str/blank? (System/getenv "DISPLAY"))
           (str/blank? (System/getenv "WAYLAND_DISPLAY")))
  (die (str "error: no display available — login.bb needs a graphical session.\n"
            "On a headless machine, do the login in your laptop's browser and\n"
            "pipe the cookie over with set-cookie.sh:\n"
            "\n"
            "  1. In your laptop's DevTools Network tab, right-click any\n"
            "     <tenant>.antithesis.com request → Copy → Copy as cURL.\n"
            "  2. ssh to this machine and run:\n"
            "       scripts/set-cookie.sh\n"
            "  3. Paste the whole curl blob and press Enter. The script greps\n"
            "     __Host-antithesis_sso_session out for you.")
       :code 4))

(.mkdirs (java.io.File. profile))
(doseq [f (.listFiles (java.io.File. profile))]
  (when (str/starts-with? (.getName f) "Singleton") (.delete f)))

(let [port     (+ 9222 (rand-int 1000))
      log-file (java.io.File. "/tmp/chromium-login.log")
      chrome   (process [@chromium-bin
                         (str "--remote-debugging-port=" port)
                         (str "--user-data-dir=" profile)
                         "--no-first-run" "--no-default-browser-check"
                         (str "https://" tenant ".antithesis.com/")]
                        {:out log-file :err log-file})]
  (try
    (when-not (wait-for-port port 80)
      (die "chromium did not open CDP port"))
    (ab "connect" (str port))

    (prompt-enter!)

    (let [{:keys [exit out err]} (ab "cookies" "get" "--json")]
      (when-not (zero? exit)
        (die (str "cookies get failed: " err)))
      (let [entries (try (json/parse-string out true)
                         (catch Exception _ (die (str "could not parse cookies JSON"))))
            hit (->> entries
                     (filter #(= cookie-name (:name %)))
                     (filter #(str/starts-with? (or (:value %) "") "v2.public."))
                     first)]
        (when-not hit
          (die (str "no " cookie-name " cookie found — did the login finish?")))
        (let [value (:value hit)
              payload (try (decode-paseto-payload value) (catch Exception _ ""))
              nickname (second (re-find #"\"nickname\":\"([^\"]+)\"" payload))
              expiry   (second (re-find #"\"exp\":\"([^\"]+)\"" payload))]
          (spit cookie-file value)
          (.setReadable (java.io.File. cookie-file) false false)
          (.setReadable (java.io.File. cookie-file) true true)
          (.setWritable (java.io.File. cookie-file) false false)
          (.setWritable (java.io.File. cookie-file) true true)
          (println (str "wrote " cookie-file " (len=" (count value) ")"))
          (when nickname (println (str "user:    " nickname)))
          (when expiry   (println (str "expires: " expiry)))
          (let [[root home] (sanity-check value)]
            (println (str "tenant:  " tenant ".antithesis.com  root=" root "  follow=" home))
            (when-not (and (= "302" root) (= "200" home))
              (binding [*out* *err*]
                (println "warn: unexpected HTTP codes — cookie may be invalid or tenant wrong"))
              (System/exit 2))))))

    (finally
      (ab "close")
      (.destroy (:proc chrome)))))
