---
title: CI Tests
order: 75
---

# abapGit CI Tests

Latest build: https://ci.abapgit.org

Repo [abapGit CI](https://github.com/abapGit/CI) provides basic continuous integration capabilities for abapGit. The repository aims to test the serialization and deserialization of object types, as these cannot be unit tested properly.

The CI process fetches abapGit test repos from https://github.com/abapGit-tests. Then for each repo the following steps are executed:
- create package (if needed)
- clone
- pull
- syntax check
- purge (uninstall)

Additionally, a set of generic tests is performed checking the latest abapGit build.

If there is any error in these steps abapGit CI will indicate that.

Run report `ZABAPGIT_CI_UPDATE_ABAPGIT` to update the abapGit repo.

Run report `ZABAPGIT_CI_UPDATE_ABAPGIT_CI` to update the abapGit CI repo.

There are currently three possibilities to run abapGit CI:

**1) Adhoc CI with report `ZABAPGIT_CI`**

Selection screen:

![abapGit CI selection screen](/img/ZABAPGIT_CI_selection_screen.png)

Result list:

![abapGit CI results](/img/ZABAPGIT_CI_result.png)

**2) REST API**

`POST /sap/zabapgit_ci/run`

[response.json](example/response.json)

**3) Schedule job sequence with report `ZABAPGIT_CI_JOB_SCHEDULER`**

A job sequence is created with

1) Update abapGit repo

2) Update abapGit CI repo

3) Run abapGit CI

![abapGit CI job scheduler](/img/ZABAPGIT_CI_JOB_SCHEDULER.png)

**Check transportable packages**

Optionally you can also let abapGit CI create the repositories in transportable packages. You need to activate the checkbox in the selection screen and specify the transport layer. Then the pull and purge actions will each get recorded into a transport request per repository. These get released and their content is checked to ensure it matches the objects in the repository.

To set up a suitable transport landscape on the AS ABAP Developer Edition you need to create a new virtual system in STMS (for example `EXP`, `Virtual export system`). This serves as a dummy export target for transport requests. Next you need create a new transport layer (for example `ZEXP`) and connect the current system and the virtual system with it. Then save and activate the configuration.

![STMS transports landscape](/img/STMS.png)

**Prerequisites:**

- abapGit developer version 
- SAP NetWeaver 7.50 or higher
