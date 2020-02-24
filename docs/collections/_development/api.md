---
title: API
order: 80
---

*******************************

## Repositories

### Create Online

```abap
DATA(lo_repo) = zcl_abapgit_repo_srv=>get_instance( )->new_online(
  iv_url         = lv_url
  iv_branch_name = lv_branch_name
  iv_package     = lv_package ).
```

### Create Offline

```abap
zcl_abapgit_repo_srv=>get_instance( )->new_offline(
```

### List

```abap
DATA(lt_repos) = NEW zcl_abapgit_persistence_repo( )->list( ).
```

todo, ZCL_ABAPGIT_REPO_SRV->list( returns a list of objects, should this be used
instead of the persistence_repo, probably, having a list of structures is just
easier to use sometimes

### Read

```abap
DATA(ls_repo) = NEW zcl_abapgit_persistence_repo( )->read( iv_key )
```

todo, zcl_abapgit_persistence_repo vs zcl_abapgit_repo_srv

### Delete

Removes the project, objects in package untouched

```abap
zcl_abapgit_repo_srv=>get_instance( )->get( iv_key )->delete( ).
```

(this one looks good)

### Purge

Deletes all objects part of repository

```abap
zcl_abapgit_repo_srv=>get_instance( )->purge(
      zcl_abapgit_repo_srv=>get_instance( )->get( iv_key ) ).
```

todo, to be refactored, in zcl_abapgit_repo_srv inputs for other methods is key not the object.
Generally, should delete + purge + read be methods on the repo object instead?
And the srv class have a get_by_id method?

### Status

Online only?

```abap
DATA(lt_result) = zcl_abapgit_file_status=>status( lo_repo ).
```

todo, should this be refactored to the repo object instead?

*******************************

## Branches

### List
`zcl_abapgit_git_transport=>branches( io_repo->get_url( ) ).`

### Switching
`repo->set_branch_name( lv_name )`

### Creating
`repo->create_branch( )`

### Deleting
todo

*******************************

## Pushing

Online projects, todo, see
[https://github.com/larshp/abapGit/blob/master/src/zcl_abapgit_background.clas.abap](https://github.com/larshp/abapGit/blob/master/src/zcl_abapgit_background.clas.abap)

todo: staging, commits, tags

*******************************

## Pulling, Online projects

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
    rs_checks-transport-transport = 'SOMETHING'.
  ENDIF.

ENDMETHOD.
```

todo:
[https://github.com/larshp/abapGit/issues/1331](https://github.com/larshp/abapGit/issues/1331)

*******************************

## Progress indicator

The default progress indicator shows progress in SAP GUI, however it is possible to inject a custom progress indicator via `ZCL_ABAPGIT_PROGRESS=>SET_INSTANCE` which can catch the status in non SAP GUI scenarios.
