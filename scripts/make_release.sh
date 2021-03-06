#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nix coreutils gnugrep gnused

set -euo pipefail

# A handy utility for filling in the github RELEASE_TEMPLATE.
# Since we are using nix, this script shoud work whether on linux or mac.
#
# NOTE: This script will target the LATEST wiki version. Make sure the
# wiki hasn't changed undesirably since the release tag.
#
# Usage:
# cd cardano-wallet # (only works from here!)
# export GITHUB_API_TOKEN="..."
# ./scripts/make_release.sh
#

################################################################################
# Release-specific parameters (Change when you bump the version)
OLD_GIT_TAG="v2020-08-03"
OLD_CABAL_VERSION="2020.8.3"

GIT_TAG="v2020-09-09"
CABAL_VERSION="2020.9.9"

JORM_TAG="v0.9.0"
CARDANO_NODE_TAG="1.19.1"
################################################################################
OLD_DATE="${OLD_GIT_TAG//v}"
CHANGELOG=GENERATED_CHANGELOG.md
OUT=GENERATED_RELEASE_NOTES-$GIT_TAG.md
REPO="input-output-hk/cardano-wallet"
WIKI_COMMIT=$(git ls-remote https://github.com/$REPO.wiki.git HEAD | cut -f1)

echo ""
echo "Replacing $OLD_CABAL_VERSION with $CABAL_VERSION"
sed -i "s/$OLD_CABAL_VERSION/$CABAL_VERSION/" \
    $(git ls-files '*.nix'; git ls-files '*.cabal')
echo "Looking for remaining references to old version:"
git grep $OLD_CABAL_VERSION
echo ""

echo "Generating changelog..."
./scripts/make_changelog $OLD_DATE > $CHANGELOG
echo ""
echo "Filling in template..."
sed -e "s/{{GIT_TAG}}/$GIT_TAG/g"                   \
    -e "s/{{JORM_TAG}}/$JORM_TAG/g"                 \
    -e "s/{{CARDANO_NODE_TAG}}/$CARDANO_NODE_TAG/g" \
    -e "s/{{CABAL_VERSION}}/$CABAL_VERSION/g"       \
    -e "s/{{WIKI_COMMIT}}/$WIKI_COMMIT/g"    \
    -e "/{{CHANGELOG}}/r $CHANGELOG"                \
    -e "/{{CHANGELOG}}/d"                           \
    .github/RELEASE_TEMPLATE.md > $OUT

read -p "Do you want to create a commit and release-tag? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
  msg="Bump version from $OLD_CABAL_VERSION to $CABAL_VERSION"
  git diff --quiet || git commit -am "$msg"
  git tag -s -m $GIT_TAG $GIT_TAG
fi
