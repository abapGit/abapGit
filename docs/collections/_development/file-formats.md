---
title: Object File Formats
order: 40
---

# CLAS
One ABAP file, corresponding to the editable parts in source based class builder or ABAP in Eclipse:

* `zcl_example.clas.abap`
* `zcl_example.clas.locals_def.abap`
* `zcl_example.clas.locals_imp.abap`
* `zcl_example.clas.testclasses.abap`
* `zcl_example.clas.macros.abap`

Files does not exist if empty

One XML file `zcl_example.clas.xml` containing:

* VSEOCLASS information
* TPOOL information
* SOTR information
* LINES from DOKU
* Descriptions from SEOCOMPOTX