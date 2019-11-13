---
title: Installation
category: getting-started
order: 10
---

## ABAP Report ##
Copy the [ABAP code](https://raw.githubusercontent.com/abapGit/build/master/zabapgit.abap) into a new report named i.e. ZABAPGIT_FULL via SE38 or SE80. To update abapGit to a newer version, replace the code in the report with the most recent. Don't use ZABAPGIT for the report name as this is used by the installation via abapGit itself.

Typically abapGit will only be used in the development system, so it can be installed in a local `$` package.

To use the online feature, [SSL must be setup](guide-ssl-setup.html). Offline projects will work behind firewalls and without SSL.

## Done ##
Run report
