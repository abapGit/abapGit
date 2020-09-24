---
title: Authorizations
category: reference
order: 50
---

You can block actions from users using **Authorizations**.

You need to create a class named `ZCL_ABAPGIT_AUTH_EXIT` implementing interface `ZIF_ABAPGIT_AUTH`, and put inside include `zabapgit_authorizations_exit` [¹](https://github.com/abapGit/abapGit/blob/52758028a7b08101e9c76d1cdab8639d776d3d2b/src/zabapgit.prog.abap#L35 "Link to source code include location") .

**Note:** If you are using the abapGit development version, do not create the class in the abapGit package.

The interface `ZIF_ABAPGIT_AUTH` has a constant with all the possible authorizations. Such as:

- uninstall
- transport_to_branch
- update_local_checksum

### Example

Suppose you want to limit the uninstalling of packages to a user named Admin. Your code could look like:

```abap
*&---------------------------------------------------------------------*
*&  Include  zabapgit_authorizations_exit
*&---------------------------------------------------------------------*
CLASS zcl_abapgit_auth_exit DEFINITION
 CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_abapgit_auth.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_abapgit_auth_exit IMPLEMENTATION.
  METHOD zif_abapgit_auth~is_allowed.
    IF iv_authorization = zif_abapgit_auth~gc_authorization-uninstall.
      IF sy-uname = 'ADMIN'.
        rv_allowed = abap_true.
      ELSE.
        rv_allowed = abap_false.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```



