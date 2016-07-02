#!/bin/bash
abapmerge src/zabapgit.prog.abap > ../zabapgit.abap
wc -l ../zabapgit.abap
cd ..
git clone https://github.com/larshp/abapGit.git -b gh-pages pages
ls -l
cp zabapgit.abap pages/build/zabapgit$(date -u "+%Y-%m-%d-%H-%M-%S").txt
cp zabapgit.abap pages/build/zabapgit.txt
cd pages
git status
git config --global user.email "builds@travis-ci.com"
git config --global user.name "Travis CI"
git add build/*.txt
git commit -m "Travis build $TRAVIS_BUILD_NUMBER"
git push -q https://$GITHUB_API_KEY@github.com/larshp/abapGit.git gh-pages > /dev/null 2>&1
