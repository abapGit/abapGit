*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_APP
*&---------------------------------------------------------------------*

CLASS lcl_gui DEFINITION DEFERRED.
CLASS lcl_repo_srv DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_app DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS gui
      RETURNING VALUE(ro_gui) TYPE REF TO lcl_gui
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS repo_srv
      RETURNING VALUE(ro_repo_srv) TYPE REF TO lcl_repo_srv.

  PRIVATE SECTION.
    CLASS-DATA: go_gui          TYPE REF TO lcl_gui,
                go_repo_srv     TYPE REF TO lcl_repo_srv.

ENDCLASS.   "lcl_app
