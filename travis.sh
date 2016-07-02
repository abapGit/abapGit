#!/bin/bash
abapmerge src/zabapgit.prog.abap > ../zabapgit.abap
wc -l ../zabapgit.abap
cd ..
git clone https://github.com/larshp/abapGit.git -b gh-pages pages
ls -l
cp zabapgit.abap pages/build/zabapgit$(date -u "+%Y-%m-%d-%H-%M-%S").abap
cp zabapgit.abap pages/build/zabapgit.abap
cd pages
git status
git config --global user.email "builds@travis-ci.com"
git config --global user.name "Travis CI"
git add build/*.abap
git commit -m "Travis build $TRAVIS_BUILD_NUMBER"
#git status
git push -f -q https://$GITHUB_API_KEY@github.com/larshp/abapGit.git &2>/dev/null
