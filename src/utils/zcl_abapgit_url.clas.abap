CLASS zcl_abapgit_url DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS validate
      IMPORTING
        !iv_url TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS host
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_host) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS name
      IMPORTING
        !iv_url        TYPE string
      RETURNING
        VALUE(rv_name) TYPE string
      RAISING
        zcx_abapgit_exception .
    CLASS-METHODS path_name
      IMPORTING
        !iv_url             TYPE string
      RETURNING
        VALUE(rv_path_name) TYPE string
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.

    CLASS-METHODS regex
      IMPORTING
        !iv_url  TYPE string
      EXPORTING
        !ev_host TYPE string
        !ev_path TYPE string
        !ev_name TYPE string
      RAISING
        zcx_abapgit_exception .
ENDCLASS.



CLASS ZCL_ABAPGIT_URL IMPLEMENTATION.


  METHOD host.

    regex( EXPORTING iv_url = iv_url
           IMPORTING ev_host = rv_host ).

  ENDMETHOD.


  METHOD name.

    regex( EXPORTING iv_url = iv_url
           IMPORTING ev_name = rv_name ).

  ENDMETHOD.


  METHOD path_name.

    DATA: lv_host TYPE string ##NEEDED.

    FIND REGEX '(.*://[^/]*)(.*)' IN iv_url
      SUBMATCHES lv_host rv_path_name.

  ENDMETHOD.


  METHOD regex.

    FIND REGEX '(.*://[^/]*)(.*/)([^\.]*)[\.git]?' IN iv_url
      SUBMATCHES ev_host ev_path ev_name.
    IF sy-subrc <> 0.
      zcx_abapgit_exception=>raise( 'Malformed URL' ).
    ENDIF.

  ENDMETHOD.


  METHOD validate.

    regex( iv_url ).

  ENDMETHOD.
ENDCLASS.
