#!/bin/bash

# Build merged
npm run merge || exit 1
wc -l ./zabapgit.abap

# Deploy artifacts
git clone https://github.com/abapGit/build.git ../build
cp zabapgit.abap ../build/zabapgit.abap
cd ../build

# Commit
git status
git config user.email "ci@abapgit.org"
git config user.name "CI"
git add zabapgit.abap
git commit -m "CI build $TRAVIS_BUILD_NUMBER" || exit 1
git push -q https://$GITHUB_API_KEY@github.com/abapGit/build.git > /dev/null 2>&1
