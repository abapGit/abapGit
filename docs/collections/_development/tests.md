---
title: Unit Tests
order: 70
---

abapGit uses dependency lookup as an approach for writing unit tests, see example in ZCL_ABAPGIT_TADIR, ZCL_ABAPGIT_FACTORY, and [ZCL_ABAPGIT_INJECTOR](https://github.com/abapGit/abapGit/blob/master/src/zcl_abapgit_injector.clas.testclasses.abap).

Unit tests that create and delete objects in the system are categorized as dangerous, these tests can be enabled via "Advanced > Settings".

Use organization [abapGit-tests](https://github.com/abapGit-tests) for storing serialization test cases.
