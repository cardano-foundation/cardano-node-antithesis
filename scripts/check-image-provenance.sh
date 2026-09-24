#!/usr/bin/env bash
# Daemon-free OCI tarball provenance check. Compares created and all
# four OCI labels to independently supplied producing inputs.
set -euo pipefail

usage() {
  printf 'usage: check-image-provenance.sh --image <tarball> --expected-revision <rev> --expected-created <rfc3339> --expected-source <url> --expected-version <ver>\n' >&2
  exit 1
}

fail() {
  printf 'FAIL %s\n' "$*" >&2
  exit 1
}

image=
expected_revision=
expected_created=
expected_source=
expected_version=

while [ $# -gt 0 ]; do
  case $1 in
    --image)
      [ $# -ge 2 ] || usage
      image=$2
      shift 2
      ;;
    --expected-revision)
      [ $# -ge 2 ] || usage
      expected_revision=$2
      shift 2
      ;;
    --expected-created)
      [ $# -ge 2 ] || usage
      expected_created=$2
      shift 2
      ;;
    --expected-source)
      [ $# -ge 2 ] || usage
      expected_source=$2
      shift 2
      ;;
    --expected-version)
      [ $# -ge 2 ] || usage
      expected_version=$2
      shift 2
      ;;
    *)
      usage
      ;;
  esac
done

[ -n "$image" ] && [ -n "$expected_revision" ] &&
  [ -n "$expected_created" ] && [ -n "$expected_source" ] &&
  [ -n "$expected_version" ] || usage

[ -e "$image" ] && [ -s "$image" ] ||
  fail "empty-or-unreadable $image"

tar_member() {
  local member=$1
  if gzip -t "$image" 2>/dev/null; then
    tar -xzOf "$image" "$member"
  else
    tar -xOf "$image" "$member"
  fi
}

manifest=$(tar_member manifest.json) ||
  fail "unreadable-manifest $image"
cfg=$(printf '%s' "$manifest" | jq -r '.[0].Config // empty')
[ -n "$cfg" ] && [ "$cfg" != "null" ] ||
  fail "unreadable-config-name $image"

config_json=$(tar_member "$cfg") ||
  fail "unreadable-config $image"

created=$(printf '%s' "$config_json" | jq -r '.created // empty')
[ -n "$created" ] || fail "missing-created"

if [ "$created" = "1970-01-01T00:00:01Z" ]; then
  fail "epoch-default created=$created"
fi
if [ "$created" = "0001-01-01T00:00:00Z" ]; then
  fail "zero-default created=$created"
fi

printf '%s' "$created" |
  grep -Eq '^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$' ||
  fail "created-not-rfc3339 created=$created"

[ "$created" = "$expected_created" ] ||
  fail "created-mismatch actual=$created expected=$expected_created"

labels_json=$(printf '%s' "$config_json" | jq -c '.config.Labels // empty')
[ -n "$labels_json" ] && [ "$labels_json" != "null" ] ||
  fail "missing-labels"

for key in org.opencontainers.image.created \
  org.opencontainers.image.revision \
  org.opencontainers.image.source \
  org.opencontainers.image.version; do
  val=$(printf '%s' "$labels_json" | jq -r --arg k "$key" '.[$k] // empty')
  [ -n "$val" ] || fail "missing-label $key"
done

label_created=$(
  printf '%s' "$labels_json" |
    jq -r '.["org.opencontainers.image.created"]'
)
label_rev=$(
  printf '%s' "$labels_json" |
    jq -r '.["org.opencontainers.image.revision"]'
)
label_source=$(
  printf '%s' "$labels_json" |
    jq -r '.["org.opencontainers.image.source"]'
)
label_version=$(
  printf '%s' "$labels_json" |
    jq -r '.["org.opencontainers.image.version"]'
)

[ "$label_created" = "$created" ] ||
  fail "label-created-mismatch label=$label_created created=$created"
[ "$label_rev" = "$expected_revision" ] ||
  fail "revision-mismatch actual=$label_rev expected=$expected_revision"
[ "$label_source" = "$expected_source" ] ||
  fail "source-mismatch actual=$label_source expected=$expected_source"
[ "$label_version" = "$expected_version" ] ||
  fail "version-mismatch actual=$label_version expected=$expected_version"

printf '%s' "$label_rev" | grep -Eq '^[0-9a-f]{40}(-dirty)?$' ||
  fail "revision-not-sha revision=$label_rev"

tarball_sha256=$(sha256sum "$image" | awk '{print $1}')
config_digest=$cfg

printf 'created=%s revision=%s config_digest=%s tarball_sha256=%s\n' \
  "$created" "$label_rev" "$config_digest" "$tarball_sha256"
