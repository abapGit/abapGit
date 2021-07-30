CLASS zcl_abapgit_proxy_config DEFINITION PUBLIC FINAL CREATE PUBLIC.

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

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mo_settings TYPE REF TO zcl_abapgit_settings,
          mi_exit     TYPE REF TO zif_abapgit_exit.

    METHODS:
      bypass_proxy
        IMPORTING
          iv_repo_url            TYPE csequence OPTIONAL
        RETURNING
          VALUE(rv_bypass_proxy) TYPE abap_bool.

ENDCLASS.



CLASS ZCL_ABAPGIT_PROXY_CONFIG IMPLEMENTATION.


  METHOD bypass_proxy.

    DATA lt_proxy_bypass TYPE zif_abapgit_definitions=>ty_range_proxy_bypass_url.

    lt_proxy_bypass = mo_settings->get_proxy_bypass( ).

    IF lt_proxy_bypass IS NOT INITIAL
    AND iv_repo_url IN lt_proxy_bypass.
      rv_bypass_proxy = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mo_settings = zcl_abapgit_persist_factory=>get_settings( )->read( ).

    mi_exit = zcl_abapgit_exit=>get_instance( ).

  ENDMETHOD.


  METHOD get_proxy_authentication.

    IF bypass_proxy( iv_repo_url ) = abap_false.
      rv_auth = mo_settings->get_proxy_authentication( ).
    ENDIF.

    mi_exit->change_proxy_authentication(
      EXPORTING
        iv_repo_url            = iv_repo_url
      CHANGING
        cv_proxy_authentication = rv_auth ).

  ENDMETHOD.


  METHOD get_proxy_port.

    IF bypass_proxy( iv_repo_url ) = abap_false.
      rv_port = mo_settings->get_proxy_port( ).
    ENDIF.

    mi_exit->change_proxy_port(
      EXPORTING
        iv_repo_url  = iv_repo_url
      CHANGING
        cv_proxy_port = rv_port ).

    CONDENSE rv_port.

  ENDMETHOD.


  METHOD get_proxy_url.

    IF bypass_proxy( iv_repo_url ) = abap_false.
      rv_proxy_url = mo_settings->get_proxy_url( ).
    ENDIF.

    mi_exit->change_proxy_url(
      EXPORTING
        iv_repo_url = iv_repo_url
      CHANGING
        cv_proxy_url = rv_proxy_url ).

  ENDMETHOD.
ENDCLASS.
