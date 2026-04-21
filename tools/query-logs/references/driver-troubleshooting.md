# open-search.sh troubleshooting

## Chromium won't start

Symptom: `chromium-query-logs.log` shows a missing-library error
(`libglib-2.0`, `libnspr4`, etc.). Cause: on NixOS, `agent-browser
install` downloads a Chrome for Testing binary that is dynamically
linked against glibc-world system libraries not present on a Nix
host.

Fix: use the nixpkgs chromium. `scripts/open-search.sh` already does
this — it never asks agent-browser to auto-launch. If you are running
the commands manually, invoke chromium from `~/.nix-profile/bin`.

## Page loads but search field is empty

Symptom: navigation succeeds but the text dump shows the default
"Explore your logs" empty state and no query. Cause: the URL's
`search=v5v…` parameter decoded to something the SPA rejected, so it
fell back to the empty builder.

Debug: decode your b64 back to JSON and validate against
`references/search-url-format.md`. Common mistakes:

- Forgot to strip base64 padding (`=` chars).
- Forgot to url-safe-encode (`+/` → `-_`).
- Added the `v5v` prefix *inside* the base64 string rather than outside.

## Button click does nothing

The primary Search button is
`<a-button variant="primary" icon="search">`. The other play-shaped
button `.action_run_button_wrapper` (`icon="play"`) stays **disabled**
until query validation — clicking it never fires.

Keyboard alternative: focus the query field, `Ctrl+Enter`.

## Results never appear

Symptom: networkidle never resolves and the text dump stays on the
loading spinner. Cause: either the session_id is invalid (silent
failure — see `indexed-sources.md`) or the backend is rate-limiting.

Debug: open the same URL in a real browser with the same cookie. If it
also hangs, the session_id is stale. If it loads quickly in a real
browser but hangs in headless, bump the `sleep` in `open-search.sh`.

## Cookie is rejected by the page

Symptom: `get url` after open returns something on `antithesis.com`
(the login host) instead of `<tenant>.antithesis.com`. Cause: cookie
expired — PASETO `exp` is typically 8-9h after issue.

Fix: re-run `scripts/set-cookie.sh` with a fresh value. The helper's
end-of-run sanity check would have caught this at intake time.

## Persistent profile conflicts

`scripts/open-search.sh` uses
`~/.cache/agent-browser-triage/profile` for the chromium user data
directory. If a prior chromium didn't shut down cleanly its SingletonLock
will block a fresh launch. Remove the lock or kill stale processes:

```bash
rm -f ~/.cache/agent-browser-triage/profile/Singleton*
pkill -f 'chromium.*remote-debugging-port' || true
```

## Parallel calls

`agent-browser` is stateful per session. **Do not** fan out multiple
`agent-browser eval` calls against the same session in parallel — the
results cross-contaminate and error messages become misleading.
