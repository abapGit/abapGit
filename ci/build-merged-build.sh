#!/bin/bash

# Build merged
npm run merge || exit 1
wc -l ./zabapgit.abap

# Deploy artifacts
git clone https://github.com/abapGit/build.git ../build
cp zabapgit.abap ../build/zabapgit_standalone.prog.abap
cd ../build