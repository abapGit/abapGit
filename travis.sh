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
