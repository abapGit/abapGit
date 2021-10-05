---
title: Local
category: repo settings
order: 20
---

## Local Settings

The following settings are stored in and valid for the current system only. 

![](img/repo_settings_locals.png)

### Display Name

This setting overwrites the default name of the repository (which is derived from the later part of the repository URL).

### Write Protected

If you write protect a repository, it will make sure that users cannot overwrite objects in the SAP system with objects from the  repository. It helps enforcing that data can only go from the SAP system to the repository.

Turning on this option disables pulls, uninstall of all objects, switching branches, etc.

Caution: The option influences only processes within abapGit. It does **not** protect against changes to objects using other means like ABAP Workbench or ABAP Development Tools. 

### Ignore Sub-Packages

This setting impacts how abapGit determines which objects belong to a repository. The default is to select all objects assigned to the root SAP package including any other subpackages (and subpackages of subpackages). If "ignore subpackages" is turned on, then only objects of the root package are selected.

### Only Local Objects 

This setting impacts how abapGit determines which objects belong to a repository. If "only local objects" is turned on, then the selection is limited to objects assigned to the current system (tadir-srcsystem = sy-sysid).

### Serialize Main Language Only

By default, abapGit will serialize objects in all installed languages (or maintained translation languages). If this option is turned on, then this process is limited to the main language defined in the repository settings (see above). 

### Code Inspector

The repository objects can be checked with the [Code inspector](https://help.sap.com/viewer/ba879a6e2ea04d9bb94c7ccd7cdac446/7.5.18/en-US/49205531d0fc14cfe10000000a42189b.html) or [ABAP Test Cockpit (ATC)](https://help.sap.com/viewer/ba879a6e2ea04d9bb94c7ccd7cdac446/7.5.18/en-US/62c41ad841554516bb06fb3620540e47.html) before staging. It's possible to perform a code inspector or ABAP Test Cockpit check without maintaing a check variant. In this case the end user is prompted with a F4 search help to choose a check variant during runtime.

#### Code Inspector Check Variant

By entering a Code inspector or the ABAP Test Cockpit check variant, the check is activated. Only global check variants are supported. Thus the check variant has to be available to all developers. A check variant can either make use of local checks of the developer system or a reference to a check variant on a remote ABAP Test Cockpit system. The available checks in a check variant on a remote ATC system may differ from the available checks of a check variant of the developer system due to the ATC system being on a newer release. For more details about release-specific availability of ABAP Test Cockpit security-related checks delivered by SAP Code Vulnerability Analyzer (CVA) see SAP Note [1921820](https://launchpad.support.sap.com/#/notes/1921820).

#### Block Commit if Code Inspection has Errors

This option can be used to prevent staging if errors of priority 1 and 2 were found during the Code Inspector or ABAP Test Cockpit  check. Findings of priority &ge; 3 are not reported. A check variant must be configured to activate this option. abapGit won't change its behavior based on the transport settings of the `Transport Tool Integration` of the ATC setup in transaction `ATC`. If this option is not active and errors were found, the end user can stage anyway. It's not possible to view or request exemptions from within abapGit during the staging process. Furthermore it's not able to access the ATC check documentation for a finding from within abapGit.

