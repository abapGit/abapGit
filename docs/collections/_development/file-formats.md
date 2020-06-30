---
title: Object File Formats
order: 40
---

All files for an object are located in the same folder, each folder corresponds to a package. Subpackages are organized under parent packages(directories) according to the logic described in [http://docs.abapgit.org/ref-dot-abapgit.html](http://docs.abapgit.org/ref-dot-abapgit.html)

# CLAS
Files corresponding to the editable parts in source based class builder or ABAP in Eclipse:

* `zcl_example.clas.abap`
* `zcl_example.clas.locals_def.abap`
* `zcl_example.clas.locals_imp.abap`
* `zcl_example.clas.testclasses.abap`
* `zcl_example.clas.macros.abap`

Files does not exist if empty, ie. the developer did not choose to implement it

One XML file `zcl_example.clas.xml` containing:

* VSEOCLASS information
* TPOOL information
* SOTR information
* LINES from DOKU
* Descriptions from SEOCOMPOTX

See example [https://github.com/abapGit-tests/CLAS_full](https://github.com/abapGit-tests/CLAS_full)
