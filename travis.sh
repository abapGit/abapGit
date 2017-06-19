#!/bin/bash
abapmerge src/zabapgit.prog.abap > ../zabapgit.abap
wc -l ../zabapgit.abap
cd ..
git clone https://github.com/larshp/abapGit.git -b build build
ls -l
cp zabapgit.abap build/zabapgit.abap
cd build
git status
git config --global user.email "builds@travis-ci.com"
git config --global user.name "Travis CI"
git add zabapgit.abap
git commit -m "Travis build $TRAVIS_BUILD_NUMBER"
git push -q https://$GITHUB_API_KEY@github.com/larshp/abapGit.git build > /dev/null 2>&1
