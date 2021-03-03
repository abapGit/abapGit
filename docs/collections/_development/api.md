---
title: API
order: 80
---

*******************************

This page describes how to execute various abapGit tasks using your own code. These classes and methods have existed for quite some time and are stable. However, they do **not** provide a guaranteed API. Future changes are a possibility.

## Repositories ##

### Create Online ###

Create a new abapGit repository for a given online project and branch and associate it with an SAP package (must exist already):

```abap
DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->new_online(
  iv_url         = lv_url
  iv_branch_name = lv_branch_name
  iv_package     = lv_package ).
```

Optional parameters correspond to the input fields of "New Online" in abapGit.

### Create Offline ###

Create a new abapGit repository for an offline project and associate it with an SAP package (must exist already):

```abap
DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->new_offline(
  iv_url     = lv_name_of_repo   "not a URL
  iv_package = lv_package ).
```

Optional parameters correspond to the input fields of "New Offline" in abapGit.

### List ###

Get a list of all repository instances:

```abap
DATA(lt_repos) = zcl_abapgit_repo_srv=>get_instance( )->list( ).
```

Get a structured list of all repositories with properties and local settings:

```abap
DATA(lt_list) = zcl_abapgit_persist_factory=>get_repo( )->list( ).
```

### Read ###

Get an instance of a repository:

```abap
DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).
```

Get a structure containing all properties and local settings of a repository:

```abap
DATA(ls_repo) = NEW zcl_abapgit_persistence_repo( )->read( iv_key ).
```

### Find ###

Find the repository for a given SAP package:

```abap
DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->get_repo_from_package( iv_package ).
```

### Delete ###

Remove an abapGit repository (the objects in SAP packages remain untouched):

```abap
zcl_abapgit_repo_srv=>get_instance( )->delete( lo_repo ).
```

### Purge ###

Delete all objects that are part of an abapGit repository (i.e. full uninstall):

```abap
ls_checks = lo_repo->delete_checks( ).
IF ls_checks-transport-required = abap_true.
  ls_checks-transport-transport = 'SIDK900000'. "transport request
ENDIF.

zcl_abapgit_repo_srv=>get_instance( )->purge(
  io_repo  = lo_repo
  is_check = ls_check ).
```

### Status ###

Get the status of all files included in a repository:

```abap
DATA(lt_result) = zcl_abapgit_file_status=>status( lo_repo ).
```


*******************************

## Online Repository ##

The following tasks are supported for online repositories only (`lo_repo type ref to zcl_abapgit_repo_online`).

**Note:** Certain tasks will require authentication (user/password or token). In such cases, you will have to provide the login details upfront (see [#1331](https://github.com/abapGit/abapGit/issues/1331) for details), authentication can also be set via user exit or configured in SM59,

```abap
zcl_abapgit_login_manager=>set(
  iv_uri      = lo_repo->get_url( )
  iv_username = lv_username
  iv_password = lv_password ).
```

### List Branches ###

Get a list of all branches (including main branch):

```abap
lo_branches = zcl_abapgit_git_transport=>branches( lo_repo->get_url( ) ).
lt_list = lo_branches->get_branches_only( ).
```

### Switch Branch ###

Switch abapGit repository to a different branch:

```abap
lo_repo->set_branch_name( lv_name ).
```

### Create Branch ###

Create a new branch in an online repository, note that IV_FROM can also be set, if not the branch will be created from the current checked out SHA1 of the repo,

```abap
lo_repo->create_branch( lv_name ).
```

### Delete Branch ###

Delete a branch of an online repository:

```abap
lo_branches = zcl_abapgit_git_transport=>branches( lo_repo->get_url( ) ).
ls_branch = lo_branches->find_by_name( lv_name ).

zcl_abapgit_git_porcelain=>delete_branch(
  iv_url    = lo_repo->get_url( )
  is_branch = ls_branch ).
```

### Push Changes ###

Push changes to an online repository:

```abap
CREATE OBJECT li_log TYPE zcl_abapgit_log.
CREATE OBJECT li_background TYPE zcl_abapgit_background_push_au. " or push_fi

li_background->run(
  io_repo     = lo_repo
  ii_log      = li_log
  it_settings = lt_settings ).
```

Alternatively, implement your own logic using interface `zif_abapgit_background` (see
[Background Package](https://github.com/abapGit/abapGit/tree/main/src/background) for details).

### Pull Changes ##

Pull changes from an online repository:

```abap
CREATE OBJECT li_log TYPE zcl_abapgit_log.
CREATE OBJECT li_background TYPE zcl_abapgit_background_pull.

li_background->run(
  io_repo     = lo_repo
  ii_log      = li_log
  it_settings = lt_settings ).
```

Alternative 1: implement your own logic using interface `zif_abapgit_background` (see
[Background Package](https://github.com/abapGit/abapGit/tree/main/src/background) for details).

Alternative 2: Use the following code to trigger the pull.

```abap
METHOD pull.

  DATA: lo_repo   TYPE REF TO zcl_abapgit_repo_online,
        ls_checks TYPE zif_abapgit_definitions=>ty_deserialize_checks.

  lo_repo ?= zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ).

  ls_checks = lo_repo->deserialize_checks( ).

* the code must decide what to do with warnings, see example below
  ls_checks = decisions( ls_checks ).

  lo_repo->deserialize( ls_checks ).

ENDMETHOD.

METHOD decisions.

* this part can be handled by presenting the warings to the user in UI, or set via logic in code
* this is an example, adjust to fit your business requirements

  rs_checks = is_checks.

  LOOP AT rs_checks-overwrite ASSIGNING FIELD-SYMBOL(<ls_overwrite>).
* Object has been modified locally
* decision field must be filled with Y or N. Y overwrites the object
    <ls_overwrite>-decision = 'Y'.
  ENDLOOP.

  LOOP AT rs_checks-warning_package ASSIGNING FIELD-SYMBOL(<ls_warning_package>).
* Y or N if object from unexpected package should be overwritten
    <ls_warning_package>-decision = 'Y'.
  ENDLOOP.

  IF rs_checks-requirements-met = 'N'.
* code must decide if deserialization should continue or not
    rs_checks-requirements-decision = 'Y'.
  ENDIF.

  IF rs_checks-transport-required = abap_true.
    rs_checks-transport-transport = 'SIDK900000'. "transport request
  ENDIF.

ENDMETHOD.
```

*******************************

## Offline Repository ##

The following tasks are supported for offline repositories only (`lo_repo type ref to zcl_abapgit_repo_offline`).

### Import ZIP ###

Upload ZIP file from frontend to an offline repository:

```abap
lv_xstr = zcl_abapgit_ui_factory=>get_frontend_services( )->file_upload( lv_file_with_path ).

lo_repo->set_files_remote( zcl_abapgit_zip=>load( lv_xstr ) ).
zcl_abapgit_services_repo=>refresh( lv_key ).
```

### Export ZIP ###

Download ZIP file of an offline repository to frontend:

```abap
lv_xstr = zcl_abapgit_zip=>export( lo_repo ).

zcl_abapgit_ui_factory=>get_frontend_services( )->file_download(
  iv_path = lv_file_with_path
  iv_xstr = lv_xstr ).
```

*******************************

## Progress Indicator ##

The default progress indicator shows progress in SAP GUI, however it is possible to inject a custom progress indicator via `ZCL_ABAPGIT_PROGRESS=>SET_INSTANCE` which can catch the status in non SAP GUI scenarios.
