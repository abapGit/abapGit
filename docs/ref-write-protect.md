---
title: Write protect
category: reference
order: 30
---

The write protected repository feature will make sure users cannot overwrite objects in the SAP system with objects from the git repository. So it helps enforcing that data can only go from the SAP system to the Git repository.

The feature disables pulls, uninstall of all objects, switching branches etc.

To enable:

1. from the abapGit main screen: Advanced -> Database util

2. find the repository, click "Edit"

3. find the value `<WRITE_PROTECT/>` and replace with `<WRITE_PROTECT>X</WRITE_PROTECT>`

4. restart abapGit(just to make sure caches are cleared)