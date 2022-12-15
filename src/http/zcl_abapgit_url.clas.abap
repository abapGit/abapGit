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
        !iv_validate   TYPE abap_bool DEFAULT abap_false
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
    CLASS-METHODS is_abapgit_repo
      IMPORTING
        !iv_url           TYPE string
      RETURNING
        VALUE(rv_abapgit) TYPE abap_bool .
    CLASS-METHODS url_address
      IMPORTING
        !iv_url          TYPE string
      RETURNING
        VALUE(rv_adress) TYPE string
      RAISING
        zcx_abapgit_exception.
  PROTECTED SECTION.
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



CLASS zcl_abapgit_url IMPLEMENTATION.


  METHOD host.

    regex( EXPORTING iv_url = iv_url
           IMPORTING ev_host = rv_host ).

  ENDMETHOD.


  METHOD is_abapgit_repo.

    IF iv_url CS 'github.com' AND ( iv_url CP '*/abapGit' OR iv_url CP '*/abapGit.git' ).
      rv_abapgit = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD name.

    DATA: lv_path TYPE string.

    TRY.
        regex( EXPORTING iv_url = iv_url
               IMPORTING ev_name = rv_name
                         ev_path = lv_path ).

        IF rv_name IS INITIAL.
          FIND REGEX '([\w-]+)/$' IN lv_path SUBMATCHES rv_name.
          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise( 'Malformed URL' ).
          ENDIF.
        ENDIF.

      CATCH zcx_abapgit_exception.
        IF iv_validate = abap_true.
          zcx_abapgit_exception=>raise( 'Malformed URL' ).
        ELSE.
          rv_name = 'URL error (fix repo with "Advanced > Change Remote")'.
        ENDIF.
    ENDTRY.

  ENDMETHOD.


  METHOD path_name.

    DATA: lv_host TYPE string ##NEEDED.

    FIND REGEX '(.*://[^/]*)(.*)' IN iv_url
      SUBMATCHES lv_host rv_path_name.

  ENDMETHOD.


  METHOD regex.

    FIND REGEX '^(https?://[^/]*)(.*/)(.*)\.git$' IN iv_url
      SUBMATCHES ev_host ev_path ev_name.
    IF sy-subrc <> 0.
      FIND REGEX '^(https?://[^/]*)(.*/)(.*)$' IN iv_url
        SUBMATCHES ev_host ev_path ev_name.
      IF sy-subrc <> 0.
        zcx_abapgit_exception=>raise( 'Malformed URL' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD url_address.

    DATA:
      lv_host TYPE string,
      lv_path TYPE string,
      lv_name TYPE string,
      lv_len  TYPE i.

    regex( EXPORTING iv_url  = iv_url
           IMPORTING ev_host = lv_host
                     ev_path = lv_path
                     ev_name = lv_name ).

    IF lv_path IS INITIAL AND lv_name IS INITIAL.
      zcx_abapgit_exception=>raise( 'Malformed URL' ).
    ELSEIF lv_name IS INITIAL.
      lv_len = strlen( lv_path ) - 1.
      IF lv_path+lv_len(1) = '/'.
        lv_path = lv_path(lv_len).
      ENDIF.
    ENDIF.

    rv_adress = |{ lv_host }{ lv_path }{ lv_name }|.

  ENDMETHOD.


  METHOD validate.

    name( iv_url      = iv_url
          iv_validate = abap_true ).

  ENDMETHOD.
ENDCLASS.
