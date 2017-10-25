*&---------------------------------------------------------------------*
*&  Include           ZABAPGIT_APP
*&---------------------------------------------------------------------*

CLASS lcl_gui DEFINITION DEFERRED.
CLASS lcl_persistence_user DEFINITION DEFERRED.
CLASS lcl_repo_srv DEFINITION DEFERRED.
CLASS lcl_persistence_db DEFINITION DEFERRED.
CLASS lcl_persist_settings DEFINITION DEFERRED.
CLASS lcl_proxy_configuration DEFINITION DEFERRED.

*----------------------------------------------------------------------*
*       CLASS lcl_app DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FINAL.
  PUBLIC SECTION.

    CLASS-METHODS gui
      RETURNING VALUE(ro_gui) TYPE REF TO lcl_gui
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS user
      IMPORTING iv_user        TYPE xubname DEFAULT sy-uname
      RETURNING VALUE(ro_user) TYPE REF TO lcl_persistence_user
      RAISING   zcx_abapgit_exception.

    CLASS-METHODS repo_srv
      RETURNING VALUE(ro_repo_srv) TYPE REF TO lcl_repo_srv.

    CLASS-METHODS db
      RETURNING VALUE(ro_db) TYPE REF TO lcl_persistence_db.

    CLASS-METHODS settings
      RETURNING VALUE(ro_settings) TYPE REF TO lcl_persist_settings.

    CLASS-METHODS proxy
      RETURNING VALUE(ro_proxy) TYPE REF TO lcl_proxy_configuration.

  PRIVATE SECTION.
    CLASS-DATA: go_gui          TYPE REF TO lcl_gui,
                go_current_user TYPE REF TO lcl_persistence_user,
                go_db           TYPE REF TO lcl_persistence_db,
                go_repo_srv     TYPE REF TO lcl_repo_srv,
                go_settings     TYPE REF TO lcl_persist_settings,
                go_proxy        TYPE REF TO lcl_proxy_configuration.

ENDCLASS.   "lcl_app
