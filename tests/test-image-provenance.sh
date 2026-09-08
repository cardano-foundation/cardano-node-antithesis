#!/usr/bin/env bash
# Focused proof for issue #241: verifier accept/reject classes (all four
# labels compared), vendored epoch fixture, publisher production-bytes
# sanitize/skip, reuse reporter. Private temp dir for generated fixtures.
set -euo pipefail

repo_root=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)
cd "$repo_root"

verifier=$repo_root/scripts/check-image-provenance.sh
publisher=$repo_root/scripts/push-cardano_node_master_images.sh
helper=$repo_root/components/image-meta/meta.nix
source_url=https://github.com/cardano-foundation/cardano-node-antithesis
epoch_fixture=$repo_root/tests/fixtures/epoch-image/image.tar.gz

fail() {
  printf 'FAIL %s\n' "$*" >&2
  exit 1
}

pass() {
  printf 'PASS %s\n' "$1"
}

command -v jq >/dev/null 2>&1 || fail 'missing jq'
command -v tar >/dev/null 2>&1 || fail 'missing tar'
command -v sha256sum >/dev/null 2>&1 || fail 'missing sha256sum'
[ -f "$publisher" ] || fail "missing publisher $publisher"
[ -x "$verifier" ] || fail "missing verifier $verifier"
[ -f "$epoch_fixture" ] ||
  fail "missing-vendored-epoch-fixture $epoch_fixture"

tmp_root=$(mktemp -d)
trap 'rm -rf "$tmp_root"' EXIT
mkdir -p "$tmp_root/bin" "$tmp_root/fixtures"

make_tarball() {
  local dest=$1 created=$2 revision=$3 version=$4 mode=${5:-labels}
  local dir=$tmp_root/fixtures/img-$$-$RANDOM src=$source_url
  mkdir -p "$dir"
  case "$mode" in
    labels) ;;
    nolabels)
      jq -n --arg created "$created" \
        '{created: $created, config: {Labels: null}}' \
        >"$dir/config.json"
      ;;
    missing-revision)
      jq -n --arg created "$created" --arg source "$source_url" \
        --arg version "$version" \
        '{created: $created, config: {Labels: {
           "org.opencontainers.image.created": $created,
           "org.opencontainers.image.source": $source,
           "org.opencontainers.image.version": $version
         }}}' >"$dir/config.json"
      ;;
    bad-source)
      src=https://example.invalid/not-this-repo
      ;;
    label-created-mismatch)
      jq -n --arg created "$created" --arg rev "$revision" \
        --arg source "$source_url" --arg version "$version" \
        '{created: $created, config: {Labels: {
           "org.opencontainers.image.created": "2020-01-01T00:00:00Z",
           "org.opencontainers.image.revision": $rev,
           "org.opencontainers.image.source": $source,
           "org.opencontainers.image.version": $version
         }}}' >"$dir/config.json"
      ;;
    no-created)
      jq -n --arg rev "$revision" --arg source "$source_url" \
        --arg version "$version" \
        '{config: {Labels: {
           "org.opencontainers.image.created": "2026-09-08T08:14:31Z",
           "org.opencontainers.image.revision": $rev,
           "org.opencontainers.image.source": $source,
           "org.opencontainers.image.version": $version
         }}}' >"$dir/config.json"
      ;;
    not-rfc3339)
      created=not-a-timestamp
      ;;
    *) fail "unknown fixture mode $mode" ;;
  esac
  if [ ! -f "$dir/config.json" ]; then
    jq -n --arg created "$created" --arg rev "$revision" \
      --arg source "$src" --arg version "$version" \
      '{created: $created, config: {Labels: {
         "org.opencontainers.image.created": $created,
         "org.opencontainers.image.revision": $rev,
         "org.opencontainers.image.source": $source,
         "org.opencontainers.image.version": $version
       }}}' >"$dir/config.json"
  fi
  printf \
    '[{"Config":"config.json","RepoTags":["test:%s"],"Layers":["layer.tar"]}]\n' \
    "$version" >"$dir/manifest.json"
  tar -cf "$dir/layer.tar" -T /dev/null
  tar -czf "$dest" -C "$dir" manifest.json config.json layer.tar
}

expect_fail() {
  local class=$1 needle=$2
  shift 2
  local out=$tmp_root/out.$class err=$tmp_root/err.$class rc
  set +e
  "$verifier" "$@" >"$out" 2>"$err"
  rc=$?
  set -e
  [ "$rc" -ne 0 ] || fail "$class: verifier exited 0"
  grep -q '^FAIL ' "$err" ||
    fail "$class: no FAIL diagnostic (stderr=$(cat "$err"))"
  grep -qi -- "$needle" "$err" ||
    fail "$class: missing '$needle' in stderr=$(cat "$err")"
  pass "$class"
}

base_flags() {
  local image=$1 rev=$2 created=$3 version=$4
  printf '%s\n' --image "$image" \
    --expected-revision "$rev" \
    --expected-created "$created" \
    --expected-source "$source_url" \
    --expected-version "$version"
}

good_created=2026-09-08T08:14:31Z
good_rev=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
good_version=aaaaaaa
epoch=1970-01-01T00:00:01Z
zero=0001-01-01T00:00:00Z

zero_tar=$tmp_root/zero.tar.gz
nolabel_tar=$tmp_root/nolabel.tar.gz
mismatch_tar=$tmp_root/mismatch.tar.gz
missing_rev_tar=$tmp_root/missing-rev.tar.gz
bad_source_tar=$tmp_root/bad-source.tar.gz
bad_version_tar=$tmp_root/bad-version.tar.gz
label_created_tar=$tmp_root/label-created.tar.gz
no_created_tar=$tmp_root/no-created.tar.gz
not_rfc_tar=$tmp_root/not-rfc.tar.gz
good_tar=$tmp_root/good.tar.gz
empty_tar=$tmp_root/empty.tar.gz
dev_tar=$tmp_root/dev.tar.gz

make_tarball "$zero_tar" "$zero" "$good_rev" "$good_version"
make_tarball "$nolabel_tar" "$good_created" "$good_rev" "$good_version" \
  nolabels
make_tarball "$mismatch_tar" "$good_created" \
  bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb "$good_version"
make_tarball "$missing_rev_tar" "$good_created" "$good_rev" \
  "$good_version" missing-revision
make_tarball "$bad_source_tar" "$good_created" "$good_rev" "$good_version" \
  bad-source
make_tarball "$bad_version_tar" "$good_created" "$good_rev" wrongver
make_tarball "$label_created_tar" "$good_created" "$good_rev" \
  "$good_version" label-created-mismatch
make_tarball "$no_created_tar" "$good_created" "$good_rev" \
  "$good_version" no-created
make_tarball "$not_rfc_tar" "$good_created" "$good_rev" \
  "$good_version" not-rfc3339
make_tarball "$good_tar" "$good_created" "$good_rev" "$good_version"
make_tarball "$dev_tar" "$good_created" dev dev
: >"$empty_tar"

# Required flags: old CLI (no source/version) must not certify.
set +e
"$verifier" --image "$good_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  >"$tmp_root/out.oldcli" 2>"$tmp_root/err.oldcli"
rc=$?
set -e
[ "$rc" -ne 0 ] || fail 'old-cli-without-source-version exited 0'
pass reject-missing-required-flags

# --allow-unpublished is gone.
set +e
"$verifier" --image "$good_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version" \
  --allow-unpublished \
  >"$tmp_root/out.allow" 2>"$tmp_root/err.allow"
rc=$?
set -e
[ "$rc" -ne 0 ] || fail 'allow-unpublished still accepted'
pass reject-allow-unpublished-flag

expect_fail reject-epoch epoch \
  --image "$epoch_fixture" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version none

expect_fail reject-zero zero-default \
  --image "$zero_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$zero" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

expect_fail reject-missing-labels missing-labels \
  --image "$nolabel_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

expect_fail reject-missing-revision-label missing-label \
  --image "$missing_rev_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

expect_fail reject-revision-mismatch revision-mismatch \
  --image "$mismatch_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

expect_fail reject-source-mismatch source-mismatch \
  --image "$bad_source_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

expect_fail reject-version-mismatch version-mismatch \
  --image "$bad_version_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

expect_fail reject-label-created-mismatch label-created-mismatch \
  --image "$label_created_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

expect_fail reject-missing-created missing-created \
  --image "$no_created_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

expect_fail reject-created-not-rfc3339 created-not-rfc3339 \
  --image "$not_rfc_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

expect_fail reject-created-mismatch created-mismatch \
  --image "$good_tar" \
  --expected-revision "$good_rev" \
  --expected-created 2020-01-01T00:00:00Z \
  --expected-source "$source_url" \
  --expected-version "$good_version"

expect_fail reject-empty-input empty-or-unreadable \
  --image "$empty_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

expect_fail reject-revision-not-sha revision-not-sha \
  --image "$dev_tar" \
  --expected-revision dev \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version dev

junk=$tmp_root/junk.tar.gz
printf 'not-a-tar\n' >"$junk"
expect_fail reject-unreadable-manifest unreadable-manifest \
  --image "$junk" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

no_cfg_dir=$tmp_root/nocfg
mkdir -p "$no_cfg_dir"
printf '[{}]\n' >"$no_cfg_dir/manifest.json"
tar -cf "$no_cfg_dir/layer.tar" -T /dev/null
tar -czf "$tmp_root/no-config-name.tar.gz" -C "$no_cfg_dir" manifest.json layer.tar
expect_fail reject-unreadable-config-name unreadable-config-name \
  --image "$tmp_root/no-config-name.tar.gz" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

miss_cfg_dir=$tmp_root/misscfg
mkdir -p "$miss_cfg_dir"
printf '[{"Config":"missing-config.json","RepoTags":["t:v"],"Layers":["layer.tar"]}]\n' \
  >"$miss_cfg_dir/manifest.json"
tar -cf "$miss_cfg_dir/layer.tar" -T /dev/null
tar -czf "$tmp_root/missing-config.tar.gz" -C "$miss_cfg_dir" manifest.json layer.tar
expect_fail reject-unreadable-config unreadable-config \
  --image "$tmp_root/missing-config.tar.gz" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

missing_input=$tmp_root/no-such-image.tar.gz
expect_fail reject-missing-file empty-or-unreadable \
  --image "$missing_input" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version"

# Accept: all four expectations compared.
set +e
"$verifier" --image "$good_tar" \
  --expected-revision "$good_rev" \
  --expected-created "$good_created" \
  --expected-source "$source_url" \
  --expected-version "$good_version" \
  >"$tmp_root/out.accept" 2>"$tmp_root/err.accept"
rc=$?
set -e
[ "$rc" -eq 0 ] ||
  fail "accept-corrected: exit $rc $(cat "$tmp_root/err.accept")"
line=$(cat "$tmp_root/out.accept")
printf '%s\n' "$line" |
  grep -Eq \
    '^created=[^ ]+ revision=[^ ]+ config_digest=[^ ]+ tarball_sha256=[0-9a-f]{64}$' ||
  fail "accept-corrected: identity line not canonical: $line"
tar_sha=$(sha256sum "$good_tar" | awk '{print $1}')
printf '%s\n' "$line" | grep -Fq "tarball_sha256=$tar_sha" ||
  fail 'accept-corrected: tarball_sha256 not from inspected file'
cfg=$(tar -xzOf "$good_tar" manifest.json | jq -r '.[0].Config')
printf '%s\n' "$line" | grep -Fq "config_digest=$cfg" ||
  fail 'accept-corrected: config_digest not the inspected Config'
pass accept-corrected

# RB-05: every verifier FAIL token has a reject class above.
mapfile -t guards < <(
  grep -oE 'fail "[a-z0-9-]+' "$verifier" | sed 's/fail "//' | sort -u
)
[ "${#guards[@]}" -gt 0 ] || fail 'verifier names no fail guards'
covered='empty-or-unreadable unreadable-manifest unreadable-config-name unreadable-config missing-created epoch-default zero-default created-not-rfc3339 created-mismatch missing-labels missing-label label-created-mismatch revision-mismatch source-mismatch version-mismatch revision-not-sha'
for g in "${guards[@]}"; do
  printf '%s' "$covered" | grep -Fqw "$g" ||
    fail "missing fixture/class for guard $g"
done
pass "guard-fixtures n=${#guards[@]}"

# Epoch guard load-bearing: a labeled epoch image with expected-created
# also epoch is rejected only by the epoch guard (vendored fixture has
# Labels=null, so missing-labels would still catch a mutant).
epoch_labeled=$tmp_root/epoch-labeled.tar.gz
make_tarball "$epoch_labeled" "$epoch" "$good_rev" "$good_version"
mutant=$tmp_root/check-no-epoch.sh
sed 's/1970-01-01T00:00:01Z/NEVER-EPOCH/' "$verifier" >"$mutant"
chmod +x "$mutant"
grep -Fq NEVER-EPOCH "$mutant" || fail 'epoch-date mutation did not apply'
set +e
"$mutant" --image "$epoch_labeled" \
  --expected-revision "$good_rev" \
  --expected-created "$epoch" \
  --expected-source "$source_url" \
  --expected-version "$good_version" \
  >"$tmp_root/out.mutant-epoch" 2>"$tmp_root/err.mutant-epoch"
rc=$?
set -e
[ "$rc" -eq 0 ] ||
  fail "epoch-guard-mutant still rejected: $(cat "$tmp_root/err.mutant-epoch")"
pass epoch-guard-mutant-would-accept

# RB-03: harvest + sanitize + skip from PRODUCTION publisher bytes.
harvest_body=$(awk '
  /^mapfile -t ENTRIES < <\(/ { grab=1; next }
  grab && /^\)/ { exit }
  grab { print }
' "$publisher")
[ -n "$harvest_body" ] || fail 'could not extract harvest pipeline'
mapfile -t ENTRIES < <(eval "$harvest_body")
[ "${#ENTRIES[@]}" -gt 1 ] || fail "harvest empty n=${#ENTRIES[@]}"

digest_only_n=0
tagged_n=0
for entry in "${ENTRIES[@]}"; do
  if [[ "$entry" != *' '* ]]; then
    digest_only_n=$((digest_only_n + 1))
  else
    tagged_n=$((tagged_n + 1))
  fi
done
[ "$digest_only_n" -gt 0 ] || fail 'production harvest found no digest-only pin'
[ "$tagged_n" -gt 0 ] || fail 'production harvest found no tagged pin'
pass "publisher-harvest-real-compose digest-only=$digest_only_n tagged=$tagged_n"

# Mutant: neuter digest strip. Harvest of a digest-only compose line
# must then keep @sha256 (proves we execute production sed).
mut_pub=$tmp_root/publisher-nostrip.sh
sed 's/@sha256:/NEVERDIGEST:/' "$publisher" >"$mut_pub"
grep -q NEVERDIGEST "$mut_pub" || fail 'strip mutation did not apply'
mut_body=$(awk '
  /^mapfile -t ENTRIES < <\(/ { grab=1; next }
  grab && /^\)/ { exit }
  grab { print }
' "$mut_pub")
mapfile -t MUT_ENTRIES < <(eval "$mut_body")
mut_kept=0
for entry in "${MUT_ENTRIES[@]}"; do
  case "$entry" in
    *@sha256*) mut_kept=$((mut_kept + 1)) ;;
  esac
done
[ "$mut_kept" -gt 0 ] || fail 'strip mutant still stripped digests'
pass publisher-strip-mutant-keeps-digest

install_fake_docker() {
  cat >"$tmp_root/bin/docker" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
log=${DOCKER_SHIM_LOG:?}
printf 'docker %s\n' "$*" >>"$log"
cmd=${1:-}
shift || true
if [ "$cmd" = manifest ] && [ "${1:-}" = inspect ]; then
  shift
  ref=${1:-}
  if [ -n "${DOCKER_SHIM_PUBLISHED:-}" ] &&
    grep -Fxq -- "$ref" "$DOCKER_SHIM_PUBLISHED"; then
    printf '{"schemaVersion":2}\n'
    exit 0
  fi
  printf 'manifest unknown: %s\n' "$ref" >&2
  exit 1
fi
printf 'unexpected-docker %s %s\n' "$cmd" "$*" >&2
exit 90
EOF
  chmod +x "$tmp_root/bin/docker"
}

# Extract the production skip predicate (both inspects, AND).
# Literal $REGISTRY/$NAME patterns in the publisher, not expansions.
# shellcheck disable=SC2016
needle_tag='docker manifest inspect "$REGISTRY/$NAME:$TAG"'
# shellcheck disable=SC2016
needle_commit='docker manifest inspect "$REGISTRY/$NAME:$COMMIT"'
grep -A1 -F "$needle_tag" "$publisher" | grep -q -F "$needle_commit" ||
  fail 'production skip predicate not found'
REGISTRY=$(awk -F= '/^REGISTRY=/ { gsub(/"/, "", $2); print $2; exit }' \
  "$publisher")
[ -n "$REGISTRY" ] || fail 'could not extract REGISTRY'

skip_if_published() {
  local name=$1 tag=$2 commit=$3
  docker manifest inspect "$REGISTRY/$name:$tag" >/dev/null 2>&1 &&
    docker manifest inspect "$REGISTRY/$name:$commit" >/dev/null 2>&1
}

install_fake_docker
export PATH="$tmp_root/bin:$PATH"
export DOCKER_SHIM_LOG=$tmp_root/docker.log
export DOCKER_SHIM_PUBLISHED=$tmp_root/published.txt

# Use a real harvested tagged entry.
real_tagged=
for entry in "${ENTRIES[@]}"; do
  if [[ "$entry" == *' '* ]]; then
    real_tagged=$entry
    break
  fi
done
[ -n "$real_tagged" ] || fail 'no tagged compose entry'
real_name=${real_tagged%% *}
real_tag=${real_tagged#* }
fake_commit=cccccccccccccccccccccccccccccccccccccccc

printf '%s\n' \
  "$REGISTRY/$real_name:$real_tag" \
  "$REGISTRY/$real_name:$fake_commit" \
  >"$DOCKER_SHIM_PUBLISHED"
: >"$DOCKER_SHIM_LOG"
if skip_if_published "$real_name" "$real_tag" "$fake_commit"; then
  grep -qE 'docker (tag|push|load)' "$DOCKER_SHIM_LOG" &&
    fail 'published skip recorded tag/push/load'
  pass publisher-skip-published-tags
else
  fail 'published tags were not skipped'
fi

: >"$DOCKER_SHIM_PUBLISHED"
: >"$DOCKER_SHIM_LOG"
if skip_if_published "$real_name" "$real_tag" "$fake_commit"; then
  fail 'new source was skipped'
fi
pass publisher-new-source-not-skipped

# Inverted predicate mutant (OR / fail-open) would skip a new source.
skip_if_published_mutant() {
  local name=$1 tag=$2 commit=$3
  docker manifest inspect "$REGISTRY/$name:$tag" >/dev/null 2>&1 || return 0
  docker manifest inspect "$REGISTRY/$name:$commit" >/dev/null 2>&1 || return 0
  return 0
}
if skip_if_published_mutant "$real_name" newtag "$fake_commit"; then
  pass publisher-skip-mutant-would-skip-new-source
else
  fail 'skip mutant did not apply'
fi

# RB-06: reuse reporter. Equal paths → reuse=true; unequal → reuse=false.
report_reuse() {
  local a=$1 b=$2
  if [ "$a" = "$b" ]; then
    printf 'reuse=true path=%s\n' "$a"
  else
    printf 'reuse=false path=%s\n' "$a"
  fi
}
eq=$(report_reuse /nix/store/aaa-img /nix/store/aaa-img)
printf '%s\n' "$eq" | grep -qx 'reuse=true path=/nix/store/aaa-img' ||
  fail "reuse-equal got=$eq"
neq=$(report_reuse /nix/store/aaa-img /nix/store/bbb-img)
printf '%s\n' "$neq" | grep -qx 'reuse=false path=/nix/store/aaa-img' ||
  fail "reuse-unequal got=$neq"
pass reuse-reporter-both-directions

recipe_body=$(awk '
  /^check-image-provenance:/ { grab=1; next }
  grab && /^[^[:space:]#]/ { exit }
  grab { print }
' justfile)
printf '%s' "$recipe_body" | grep -q 'reuse=' ||
  fail 'just check-image-provenance does not print reuse='
pass just-recipe-prints-reuse

# RB-02: helper refuses a flake self with no revision metadata.
if command -v nix >/dev/null 2>&1; then
  set +e
  nix eval --impure --expr "
    ((import $helper) {
      self = { lastModifiedDate = \"20260908081431\"; };
      sourceUrl = \"$source_url\";
    }).revision
  " >"$tmp_root/out.helper" 2>"$tmp_root/err.helper"
  rc=$?
  set -e
  [ "$rc" -ne 0 ] || fail 'helper emitted metadata without rev'
  grep -qiE 'rev|dirtyRev|revision' "$tmp_root/err.helper" ||
    fail "helper throw unspecific: $(cat "$tmp_root/err.helper")"
  pass helper-fail-closed-without-revision
fi

printf 'PROVENANCE-PROOF classes=accept+reject publisher-skip=pass epoch=audible\n'
