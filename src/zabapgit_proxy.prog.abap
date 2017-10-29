*&---------------------------------------------------------------------*
*& Include zabapgit_proxy
*&---------------------------------------------------------------------*

CLASS lcl_proxy_configuration DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS:
      constructor,

      get_proxy_url
        IMPORTING
          iv_repo_url         TYPE csequence OPTIONAL
        RETURNING
          VALUE(rv_proxy_url) TYPE string,

      get_proxy_port
        IMPORTING
          iv_repo_url    TYPE csequence OPTIONAL
        RETURNING
          VALUE(rv_port) TYPE string,

      get_proxy_authentication
        IMPORTING
          iv_repo_url    TYPE csequence OPTIONAL
        RETURNING
          VALUE(rv_auth) TYPE abap_bool.

  PRIVATE SECTION.
    DATA: mo_settings TYPE REF TO lcl_settings,
          mi_exit     TYPE REF TO lif_exit.

ENDCLASS.

CLASS lcl_proxy_configuration IMPLEMENTATION.

  METHOD constructor.

    mo_settings = lcl_app=>settings( )->read( ).

    mi_exit = lcl_exit=>get_instance( ).

  ENDMETHOD.

  METHOD get_proxy_url.

    rv_proxy_url = mo_settings->get_proxy_url( ).

    mi_exit->change_proxy_url(
      EXPORTING
        iv_repo_url = iv_repo_url
      CHANGING
        c_proxy_url = rv_proxy_url ).

  ENDMETHOD.

  METHOD get_proxy_port.

    rv_port = mo_settings->get_proxy_port( ).

    mi_exit->change_proxy_port(
      EXPORTING
        iv_repo_url  = iv_repo_url
      CHANGING
        c_proxy_port = rv_port ).

  ENDMETHOD.

  METHOD get_proxy_authentication.

    rv_auth = mo_settings->get_proxy_authentication( ).

    mi_exit->change_proxy_authentication(
      EXPORTING
        iv_repo_url            = iv_repo_url
      CHANGING
        c_proxy_authentication = rv_auth ).

  ENDMETHOD.

ENDCLASS.
