---
title: Namespaces
category: reference
order: 70
---

Objects with namespaces are serialized, `/FOOBAR/REPORT` to `#foobar#report.prog.abap`

Using abapGit to install to customer systems:

A: Create namespace in SE03, namespace role = C, and add the repair license

B: Open namespace for modifications in SE03

C: Create namespaced package

D: Change package original system to current in SE03 -> Change Object Directory Entries

E: Clone/pull like normal


Objects will appear as repaired in the customer system, compare with git source to determine if the objects are modified.
