#!/usr/bin/env bash
# Clone or update every repo listed in repos.txt into git/<repo-name>.
#
# Usage: scripts/sync_repos.sh [repos.txt] [git]
#
# Public repos, anonymous HTTPS. Full clones with all branches, so the analysis
# can `git show <run-commit>:<path>` even for runs launched from test branches.
set -euo pipefail

LIST="${1:-repos.txt}"
DEST="${2:-git}"
mkdir -p "$DEST"

while IFS= read -r line; do
  line="${line%%#*}"; line="${line//[[:space:]]/}"
  [[ -z "$line" ]] && continue
  name="${line##*/}"
  dir="$DEST/$name"
  if [[ -d "$dir/.git" ]]; then
    echo "update  $line"
    git -C "$dir" fetch --all --tags --prune --quiet
    default="$(git -C "$dir" symbolic-ref --short refs/remotes/origin/HEAD 2>/dev/null | sed 's#^origin/##' || true)"
    if [[ -n "$default" ]]; then
      git -C "$dir" checkout --quiet "$default"
      git -C "$dir" pull --ff-only --quiet
    fi
  else
    echo "clone   $line"
    git clone --quiet "https://github.com/$line.git" "$dir"
  fi
  printf '        %s  %s  %s\n' \
    "$(git -C "$dir" rev-parse --short HEAD)" \
    "$(git -C "$dir" log -1 --format=%cs)" \
    "$(git -C "$dir" rev-parse --abbrev-ref HEAD)"
done < "$LIST"
