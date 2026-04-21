#!/usr/bin/env bash
# Prompt the user interactively for the Antithesis SSO PASETO cookie and
# write it to /tmp/antithesis-cookie.txt with 0600 perms.
#
# The read prompt is silent (-s) so the value does NOT land in shell history
# or terminal scrollback. Only this script's stdout is ever visible — the
# cookie value itself is never printed, only its length, the user's nickname,
# and expiry.
#
# Usage:
#
#   scripts/set-cookie.sh
#
# Then paste either the bare __Host-antithesis_sso_session value
# (v2.public....) or the full 'Copy as cURL' blob from DevTools, and
# press Enter. The value is stored at $ANTITHESIS_COOKIE_FILE (default
# /tmp/antithesis-cookie.txt) for subsequent calls.
#
# If you invoke this from inside an agent harness that captures tool
# stdout into a transcript, invoke it via the harness's shell-passthrough
# prefix so the silent read happens in your terminal and the pasted
# value never reaches the transcript.

set -euo pipefail

COOKIE_FILE="${ANTITHESIS_COOKIE_FILE:-/tmp/antithesis-cookie.txt}"

printf "Paste the Antithesis cookie — either the bare v2.public.… value or\n"
printf "the full 'Copy as cURL' blob from DevTools — then Enter (hidden): "
read -rs RAW_INPUT
printf '\n'

if [[ -z "$RAW_INPUT" ]]; then
  echo "error: empty input" >&2
  exit 1
fi

# Accept two shapes:
#   1. a bare PASETO starting with v2.public.
#   2. a full 'Copy as cURL' blob — pull the cookie out of -b/--cookie or
#      the Cookie: header. The Cookie header can carry several name=value
#      pairs joined by '; '; we only want __Host-antithesis_sso_session.
if [[ "$RAW_INPUT" == v2.public.* && "$RAW_INPUT" != *$'\n'* && "$RAW_INPUT" != *" "* ]]; then
  COOKIE="$RAW_INPUT"
else
  COOKIE="$(printf '%s' "$RAW_INPUT" \
    | grep -oE '__Host-antithesis_sso_session=v2\.public\.[A-Za-z0-9_-]+' \
    | head -1 \
    | sed 's/^__Host-antithesis_sso_session=//')"
fi

if [[ -z "$COOKIE" ]]; then
  echo "error: could not find __Host-antithesis_sso_session=v2.public.… in input" >&2
  exit 1
fi
if [[ "$COOKIE" != v2.public.* ]]; then
  echo "error: expected value starting with v2.public." >&2
  exit 1
fi

umask 077
printf '%s' "$COOKIE" > "$COOKIE_FILE"

# Decode the payload (middle segment of PASETO v2.public) to extract expiry.
# v2.public layout: "v2.public." + base64url( payload_json || ed25519_sig[64] ).
# So after decoding, trim the last 64 bytes to recover the JSON.
B64="${COOKIE#v2.public.}"
PAD=$(( (4 - ${#B64} % 4) % 4 ))
for _ in $(seq 1 "$PAD"); do B64="${B64}="; done
RAW="$(printf '%s' "$B64" | tr '_-' '/+' | base64 -d 2>/dev/null || true)"
JSON="${RAW%????????????????????????????????????????????????????????????????}"
EXP="$(printf '%s' "$JSON" | grep -oE '"exp":"[^"]+"' | head -1 | sed 's/.*:"//; s/"$//')"
NICKNAME="$(printf '%s' "$JSON" | grep -oE '"nickname":"[^"]+"' | head -1 | sed 's/.*:"//; s/"$//')"

echo "wrote $COOKIE_FILE (len=${#COOKIE})"
[[ -n "$NICKNAME" ]] && echo "user:    $NICKNAME"
[[ -n "$EXP" ]]      && echo "expires: $EXP"

# Quick sanity check against the tenant dashboard — expect root redirect (302)
# and /home load (200). If this fails the cookie is wrong or expired.
TENANT="${ANTITHESIS_TENANT:-cardano}"
ROOT_CODE=$(curl -s -o /dev/null -w "%{http_code}" \
  -b "__Host-antithesis_sso_session=$COOKIE" \
  "https://${TENANT}.antithesis.com/")
HOME_CODE=$(curl -sL -o /dev/null -w "%{http_code}" \
  -b "__Host-antithesis_sso_session=$COOKIE" \
  "https://${TENANT}.antithesis.com/")
echo "tenant:  ${TENANT}.antithesis.com  root=$ROOT_CODE  follow=$HOME_CODE"
if [[ "$ROOT_CODE" != "302" || "$HOME_CODE" != "200" ]]; then
  echo "warn: unexpected HTTP codes — cookie may be invalid or tenant wrong" >&2
  exit 2
fi
