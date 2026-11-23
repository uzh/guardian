#!/usr/bin/env bash

set -euo pipefail

REPO_URL="https://github.com/uzh/guardian"

usage() {
  cat <<EOF
Usage:
  $0 --patch
  $0 --minor
  $0 --major
  $0 X.Y.Z    (release the given version, e.g. the one already in dune-project)
EOF
  exit 1
}

[[ $# -eq 1 ]] || usage

cd "$(dirname "$0")/.."

BRANCH=$(git rev-parse --abbrev-ref HEAD)
if [[ "$BRANCH" != "main" ]]; then
  echo "GIT: Releases are cut from main (current branch: ${BRANCH})."
  exit 1
fi

if ! git diff --quiet || ! git diff --cached --quiet; then
  echo "GIT: Working tree is not clean."
  echo "GIT: Commit your changes before bumping the version."
  exit 1
fi

CURRENT_VERSION=$(sed -n 's/^(version \(.*\))$/\1/p' dune-project)

if [[ -z "$CURRENT_VERSION" ]]; then
  echo "Failed to read version from dune-project"
  exit 1
fi

IFS='.' read -r MAJOR MINOR PATCH <<< "$CURRENT_VERSION"

case "$1" in
  --patch)
    NEW_VERSION="${MAJOR}.${MINOR}.$((PATCH + 1))"
    ;;
  --minor)
    NEW_VERSION="${MAJOR}.$((MINOR + 1)).0"
    ;;
  --major)
    NEW_VERSION="$((MAJOR + 1)).0.0"
    ;;
  [0-9]*.[0-9]*.[0-9]*)
    NEW_VERSION="$1"
    ;;
  *)
    usage
    ;;
esac

if git rev-parse -q --verify "refs/tags/${NEW_VERSION}" > /dev/null; then
  echo "Tag ${NEW_VERSION} already exists."
  exit 1
fi

if ! grep -qi '^## \[unreleased\]' CHANGELOG.md; then
  echo "CHANGELOG.md has no '## [Unreleased]' section to release."
  exit 1
fi

if ! awk '/^## / { if (found) exit; found = tolower($0) ~ /^## \[unreleased\]/; next }
          found && !/^To be released as / && /[^[:space:]]/ { ok = 1 }
          END { exit !ok }' CHANGELOG.md; then
  echo "CHANGELOG.md's unreleased section is empty: document the changes first."
  exit 1
fi

if grep -q "^## \[${NEW_VERSION}\]" CHANGELOG.md; then
  echo "CHANGELOG.md already has a section for ${NEW_VERSION}."
  exit 1
fi

echo "Releasing version: ${CURRENT_VERSION} -> ${NEW_VERSION}"

# dune-project
perl -pi -e \
  "s/^\\(version .*\\)/(version $NEW_VERSION)/" \
  ./dune-project

# guardian.opam (generated from dune-project, kept in sync for the CI check)
perl -pi -e \
  "s/^version: \".*\"/version: \"$NEW_VERSION\"/" \
  ./guardian.opam

# CHANGELOG.md: the unreleased section becomes the release, a new empty one goes on top
DATE=$(date +%Y-%m-%d)
NEW_VERSION="$NEW_VERSION" DATE="$DATE" REPO_URL="$REPO_URL" perl -0pi -e '
  s{^## \[unreleased\]\([^)]*\)\n(?:\nTo be released as [^\n]*\n)?}
   {## [Unreleased]($ENV{REPO_URL}/tree/HEAD)\n\n## [$ENV{NEW_VERSION}]($ENV{REPO_URL}/tree/$ENV{NEW_VERSION}) - $ENV{DATE}\n}mi
' ./CHANGELOG.md

git add dune-project guardian.opam CHANGELOG.md

git commit -m "chore: release ${NEW_VERSION}"

git tag -a "${NEW_VERSION}" -m "Release ${NEW_VERSION}"

echo
echo "Created:"
echo "  Commit: chore: release ${NEW_VERSION}"
echo "  Tag:    ${NEW_VERSION}"
echo
echo "To publish:"
echo "  git push"
echo "  git push origin ${NEW_VERSION}"
