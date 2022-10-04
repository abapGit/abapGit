#!/bin/bash
TAG="$1"

if [ -z $TAG ]; then
    echo "Failed: Tag was not specified"
    exit 1
fi
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
