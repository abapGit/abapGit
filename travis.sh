#!/bin/bash
abapmerge src/zabapgit.prog.abap > ../zabapgit.abap
wc -l ../zabapgit.abap
cd ..
git clone https://github.com/larshp/abapGit.git -b gh-pages pages
ls -l
