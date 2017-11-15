CLASS zcl_abapgit_url DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS:
      host
        IMPORTING
          !iv_repo       TYPE string
        RETURNING
          VALUE(rv_host) TYPE string
        RAISING
          zcx_abapgit_exception,

      name
        IMPORTING
          !iv_repo       TYPE string
        RETURNING
          VALUE(rv_name) TYPE string
        RAISING
          zcx_abapgit_exception,

      path_name
        IMPORTING
          !iv_repo            TYPE string
        RETURNING
          VALUE(rv_path_name) TYPE string
        RAISING
          zcx_abapgit_exception .

  PRIVATE SECTION.
    CLASS-METHODS:
      regex
        IMPORTING
          !iv_repo TYPE string
        EXPORTING
          !ev_host TYPE string
          !ev_path TYPE string
          !ev_name TYPE string
        RAISING
          zcx_abapgit_exception .

ENDCLASS.



CLASS zcl_abapgit_url IMPLEMENTATION.


  METHOD host.

    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_host = rv_host ).

  ENDMETHOD.


  METHOD name.

    regex( EXPORTING iv_repo = iv_repo
           IMPORTING ev_name = rv_name ).

  ENDMETHOD.


  METHOD path_name.


    DATA: lv_host TYPE string ##NEEDED.

    FIND REGEX '(.*://[^/]*)(.*)' IN iv_repo
      SUBMATCHES lv_host rv_path_name.


  ENDMETHOD.


  METHOD regex.


    FIND REGEX '(.*://[^/]*)(.*/)([^\.]*)[\.git]?' IN iv_repo
      SUBMATCHES ev_host ev_path ev_name.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Malformed URL' ).
    ENDIF.


  ENDMETHOD.
ENDCLASS.
