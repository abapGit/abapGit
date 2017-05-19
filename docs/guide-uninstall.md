---
title: Uinstalling abapGit
---

* Delete ABAP reports ZABAPGIT* using SE38 or SE80
* Delete EZABAPGIT enqueue object via SE11
* Delete ZABAPGIT transparent table via SE11
* Delete all standard texts named ZABAPGIT* via SO10 (only relevant for old versions of abapGit)
* Delete mime objects ZABAPGIT_CSS_COMMON + ZABAPGIT_JS_COMMON in transaction SMW0