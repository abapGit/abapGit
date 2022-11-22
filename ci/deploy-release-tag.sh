#!/bin/bash
echo "Detecting version change ..."

VERSION_FILE=src/zif_abapgit_version.intf.abap
VERSION_CONSTANT=c_abap_version

git diff-tree --no-commit-id --name-only -r HEAD | grep $VERSION_FILE > /dev/null 2>&1
if [ $? -ne 0 ]; then
    echo "no version change detected, skipping tag creation ($VERSION_FILE unchaged)"
    exit 0
fi

VERSION_DIFF=$(git diff HEAD^:$VERSION_FILE HEAD:$VERSION_FILE)

echo "$VERSION_DIFF" | grep $VERSION_CONSTANT > /dev/null 2>&1
if [ $? -ne 0 ]; then
    echo "no version change detected, skipping tag creation ($VERSION_CONSTANT unchanged)"
    exit 0
fi

VERSION_BEFORE=$(echo "$VERSION_DIFF" | grep "^-.\+\b$VERSION_CONSTANT\b" | grep -E -o "[0-9]+\.[0-9]+\.[0-9]+")
VERSION_AFTER=$(echo "$VERSION_DIFF" | grep "^+.\+\b$VERSION_CONSTANT\b" | grep -E -o "[0-9]+\.[0-9]+\.[0-9]+")

if [ -z $VERSION_BEFORE ] || [ -z $VERSION_AFTER ]; then
    echo "unexpected version parsing error"
    echo "$VERSION_DIFF" | grep $VERSION_CONSTANT
    exit 1
fi

if [ $VERSION_BEFORE = $VERSION_AFTER ]; then
    echo "no version change detected, skipping tag creation ($VERSION_BEFORE = $VERSION_AFTER)"
    exit 0
fi

TAG="v$VERSION_AFTER"
echo "version change detected [$VERSION_BEFORE > $VERSION_AFTER], creating a new tag ..."

# DEPLOY
if [ -z $GIT_USER_EMAIL ] || [ -z $GIT_USER_NAME ]; then
    echo "Failed: Git user name and email must be defined via env"
    exit 1
fi
git config user.email "$GIT_USER_EMAIL"
git config user.name "$GIT_USER_NAME"

REPO_URL=$(git remote -v | grep -m1 '^origin' | sed -Ene 's#.*(https://[^[:space:]]+).*#\1#p')
PUSH_URL=$(echo "$REPO_URL" | sed -Ene "s#(https://)#\1$GITHUB_API_KEY@#p")
# e.g. https://$GITHUB_API_KEY@github.com/larshp/abapGit.git

git tag $TAG || exit 1
git push $PUSH_URL $TAG || exit 1
echo "pushedTag=$TAG" >> $GITHUB_OUTPUT
# https://github.blog/changelog/2022-10-11-github-actions-deprecating-save-state-and-set-output-commands/
