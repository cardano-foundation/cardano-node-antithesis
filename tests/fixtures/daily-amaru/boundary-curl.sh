#!/usr/bin/env bash
set -euo pipefail

# Stand-in for the two App-authenticated GitHub calls the consumer mint makes.
# It accepts only the real invocation shape: the Authorization header comes from
# a file descriptor (never argv), the JWT scheme is Bearer, its payload names
# the configured App and expires within minutes, and the token request is scoped
# to this repository alone with exactly the three consumer permissions.
log=${DAILY_AMARU_BOUNDARY_CURL_LOG:?DAILY_AMARU_BOUNDARY_CURL_LOG is required}
{
  printf 'curl'
  printf ' %s' "$@"
  printf '\n'
} >>"$log"

method=GET
header_file=''
has_body=0
url=''
while [ "$#" -gt 0 ]; do
  case "$1" in
    --silent | --show-error | --fail-with-body) shift ;;
    --max-time) shift 2 ;;
    --request) method=$2; shift 2 ;;
    --header)
      case "$2" in
        @*) header_file=${2#@} ;;
        'Accept: application/vnd.github+json') ;;
        *) exit 64 ;;
      esac
      shift 2
      ;;
    --data-binary) [ "$2" = @- ] || exit 64; has_body=1; shift 2 ;;
    -*) exit 64 ;;
    *) url=$1; shift ;;
  esac
done
[ -n "$header_file" ] && [ -n "$url" ] || exit 64

authorization=$(cat "$header_file")
[[ "$authorization" =~ ^Authorization:\ Bearer\ ([A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+)$ ]] || exit 22
jwt=${BASH_REMATCH[1]}
payload=$(jq -R 'split(".")[1] | gsub("-";"+") | gsub("_";"/") | @base64d | fromjson' <<<"$jwt") || exit 22
jq -e --arg app "${DAILY_AMARU_APP_ID:?}" \
  '.iss == $app and .exp > .iat and (.exp - .iat) <= 660' <<<"$payload" >/dev/null || exit 22

repository=${DAILY_AMARU_REPOSITORY:-cardano-foundation/cardano-node-antithesis}
case "$method $url" in
  "GET https://api.github.com/repos/$repository/installation")
    [ "$has_body" -eq 0 ] || exit 64
    [ "${DAILY_AMARU_BOUNDARY_INSTALLATION:-present}" = present ] || exit 22
    printf '{"id":424242}\n'
    ;;
  "POST https://api.github.com/app/installations/424242/access_tokens")
    [ "$has_body" -eq 1 ] || exit 64
    body=$(cat)
    jq -e --arg repo "$repository" '
      .repositories == [($repo | split("/")[1])]
      and .permissions == {contents: "write", pull_requests: "write", metadata: "read"}
    ' <<<"$body" >/dev/null || exit 22
    counter=${DAILY_AMARU_BOUNDARY_MINT_COUNTER:?}
    n=$(($(cat "$counter" 2>/dev/null || printf 0) + 1))
    printf '%s\n' "$n" >"$counter"
    printf '{"token":"boundary-consumer-token-%s"}\n' "$n"
    ;;
  *) exit 64 ;;
esac
